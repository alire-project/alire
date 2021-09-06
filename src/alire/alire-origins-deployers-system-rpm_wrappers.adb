with AAA.Strings; use AAA.Strings;

with Alire.OS_Lib.Subprocess;
with Alire.Errors;

with GNAT.Regpat;

package body Alire.Origins.Deployers.System.RPM_Wrappers is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   ---------------------
   -- Wrapper_Command --
   ---------------------

   function Wrapper_Command (This : Deployer) return String is
     (case This.Wrapper is
         when Dnf => "dnf",
         when Yum => "yum");

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding
   function Already_Installed (This : Deployer) return Boolean is
      Pck    : String renames This.Base.Package_Name;

      Wrapper : constant String := Wrapper_Command (This);

      Output : constant AAA.Strings.Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   (Wrapper,
                    Empty_Vector &
                      "-y" &
                      "list" &
                      "--installed" &
                      This.Base.Package_Name,
                    Valid_Exit_Codes => (0, 1), -- Returned when not found
                    Err_To_Out     => True);
   begin
      if not Output.Is_Empty
        and then
          Output.First_Element = Pck
      then
         return True;
      else
         Trace.Detail ("Cannot find package '" & Pck &
                         "' in " & Wrapper & " installed list: '" &
                         Output.Flatten & "'");
         return False;
      end if;
   end Already_Installed;

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : Deployer) return Version_Outcomes.Outcome is

      Regexp : constant String := "^" & This.Base.Package_Name &
                 "[^\s]+\s+(?:\d+:)?([0-9.]+)";
      --  A line looks like:
      --  gtk3.x86_64    1:3.24.24-1.fc33    updates

      Wrapper : constant String := Wrapper_Command (This);

      Output    : constant AAA.Strings.Vector :=
        Subprocess.Checked_Spawn_And_Capture
          (Wrapper,
           Empty_Vector &
             "-y" &
             "list" &
             This.Base.Package_Name,
           Valid_Exit_Codes => (0, 1), -- Returned when not found
           Err_To_Out       => True);

      use GNAT.Regpat;
      Matches : Match_Array (1 .. 1);
   begin
      for Line of Output loop
         Trace.Debug ("Extracting native version from " & Wrapper &
                        " output: " & Line);
         Match (Regexp, Line, Matches);
         if Matches (1) /= No_Match then
            Trace.Debug ("Candidate version string: "
                         & Line (Matches (1).First .. Matches (1).Last));
            return
              Version_Outcomes.New_Result
                (Semantic_Versioning.Parse
                   (Line (Matches (1).First .. Matches (1).Last),
                    Relaxed => True)); --  Relaxed because some Fedora
                                       --  versions have extra version pts,
                                       --  e.g. 5.8-3.fc33
         else
            Trace.Debug
              ("Unexpected version format, could not identify version");
         end if;
      end loop;

      Trace.Debug ("System deployer could not detect: " & This.Base.Image);
      return Version_Outcomes.Outcome_Failure ("could not be detected",
                                               Report => False);
   end Detect;

   -------------
   -- Install --
   -------------

   overriding
   function Install (This : Deployer) return Outcome is

      Wrapper : constant String := Wrapper_Command (This);

   begin
      Subprocess.Checked_Spawn
        ("sudo", Empty_Vector &
           Wrapper &
           "-y" &
           "install" &
           This.Base.Package_Name);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Install;

end Alire.Origins.Deployers.System.RPM_Wrappers;
