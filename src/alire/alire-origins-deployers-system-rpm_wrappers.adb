with AAA.Strings; use AAA.Strings;

with Alire.OS_Lib.Subprocess;
with Alire.Errors;
with Alire.Platforms.Current;

package body Alire.Origins.Deployers.System.RPM_Wrappers is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   ---------------------
   -- Wrapper_Command --
   ---------------------

   function Wrapper_Command (This : Deployer) return String is
     (case This.Wrapper is
         when Dnf => "dnf",
         when Yum => "yum");

   ------------------------------
   -- Package_Name_With_Archit --
   ------------------------------

   function Package_Name_With_Archit (Name : String) return String
   is (if Contains (Name, ".")
       then Name
       else Name
            & "."
            & To_Lower_Case
                (Platforms.Current.Host_Architecture'Image));

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding
   function Already_Installed (This : Deployer) return Boolean is

      --  Name of the package narrowed down by architecture, to avoid being
      --  confused by i386 packages
      Full_Pkg_Name : constant String :=
                        Package_Name_With_Archit (This.Base.Package_Name);

      Wrapper : constant String := Wrapper_Command (This);

      Output : constant AAA.Strings.Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("rpm",
                    Empty_Vector &
                      "--query" &
                      Full_Pkg_Name,
                    Valid_Exit_Codes => (0, 1), -- Returned when not found
                    Err_To_Out     => True);
   begin
      if not Output.Is_Empty
        and then
          not Contains (Output.First_Element, "not installed")
      then
         return True;
      else
         Trace.Detail ("Cannot find package '" & Full_Pkg_Name &
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

      --  Name of the package narrowed down by architecture, to avoid being
      --  confused by i386 packages
      Full_Pkg_Name : constant String :=
                        Package_Name_With_Archit (This.Base.Package_Name);

      --------------------------
      -- Detect_Not_Installed --
      --------------------------

      function Detect_Not_Installed return Version_Outcomes.Outcome is

         Regexp : constant String :=
                    "^" & AAA.Strings.To_Lower_Case (Full_Pkg_Name) &
                    "[^\s]*\s+(?:\d+:)?([0-9.]+)";
         --  A line looks like:
         --  gtk3.x86_64    1:3.24.24-1.fc33    updates

         Wrapper : constant String := Wrapper_Command (This);

         Output    : constant AAA.Strings.Vector :=
                       Subprocess.Checked_Spawn_And_Capture
                         (Wrapper,
                          Empty_Vector &
                            "-y" &
                            "list" &
                            Full_Pkg_Name,
                          Valid_Exit_Codes => (0, 1), -- 1 when not found
                          Err_To_Out       => True);
      begin
         for Line of Output loop
            Trace.Debug ("Extracting native version from " & Wrapper &
                           " output: " & Line & " with regex " & Regexp);
            declare
               Match : constant String :=
                         Utils.First_Match (Regexp,
                                            AAA.Strings.To_Lower_Case (Line));
            begin
               if Match /= "" then
                  Trace.Debug ("Candidate version string: " & Match);
                  return
                    Version_Outcomes.New_Result
                      (Semantic_Versioning.Parse (Match, Relaxed => True));
                  --  Relaxed because some Fedora versions have extra version
                  --  pts, e.g. 5.8-3.fc33
               else
                  Trace.Debug
                    ("Unexpected version format, could not identify version");
               end if;
            end;
         end loop;

         Trace.Debug ("System deployer could not detect: " & This.Base.Image);
         return Version_Outcomes.Outcome_Failure ("could not be detected",
                                                  Report => False);
      end Detect_Not_Installed;

      ----------------------
      -- Detect_Installed --
      ----------------------
      --  Return "" if no installed matching version
      function Detect_Installed return String
      is
         Output    : constant AAA.Strings.Vector :=
                       Subprocess.Checked_Spawn_And_Capture
                         ("rpm",
                          Empty_Vector &
                            "--query" &
                            "--info" &
                            Full_Pkg_Name,
                          Valid_Exit_Codes => (0, 1), -- 1 when not found
                          Err_To_Out       => True);

      begin
         --  The line we want looks like:
         --  Version     : x.y.z

         for Line of Output loop
            if Has_Prefix (Line, "Version ") then
               return Trim (Tail (Line, ':'));
            end if;
         end loop;

         return "";
      end Detect_Installed;

   begin

      --  Try first a quick detection
      declare
         Result : constant String := Detect_Installed;
      begin
         if Result /= "" then
            Trace.Debug ("Candidate version string: " & Result);
            return Version_Outcomes.New_Result
              (Semantic_Versioning.Parse (Result, Relaxed => True));
            --  The output format of rpm will not have epoch nor the own fedora
            --  release trail after '-'.
         end if;
      end;

      --  This is much slower
      return Detect_Not_Installed;

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
