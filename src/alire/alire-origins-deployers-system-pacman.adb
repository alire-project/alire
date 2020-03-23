with Alire.OS_Lib.Subprocess;
with Alire.Utils;             use Alire.Utils;
with Alire.Errors;

with GNAT.Regpat;

package body Alire.Origins.Deployers.System.Pacman is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   ----------------------
   -- Get_Package_Line --
   ----------------------

   function Get_Package_Line (Package_Name : String) return String is

      Package_Match : constant String := "^" & Package_Name & "$";

      Output : constant Utils.String_Vector :=
        Subprocess.Checked_Spawn_And_Capture
          ("pacman",
           Empty_Vector &
             "-Ss" &
             Package_Match);
   begin
      if not Output.Is_Empty then
         return Output.First_Element;
      else
         return "";
      end if;
   end Get_Package_Line;

   ----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean is

      Regexp : constant String := "^.* ([0-9.\-]+) \[installed\]$";

      Package_Line : constant String :=
        Get_Package_Line (This.Base.Package_Name);

      use GNAT.Regpat;
   begin
      if Package_Line /= "" then
         Trace.Detail ("Extracting native version from pacman output: " &
                         Package_Line);

         if Match (Regexp, Package_Line) then
            return True;
         end if;
      end if;

      return False;
   end Already_Installed;

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : Deployer) return Version_Outcomes.Outcome is

      Regexp : constant String := "^.* ([0-9.\-]+) .*$";

      Package_Line : constant String :=
        Get_Package_Line (This.Base.Package_Name);

      use GNAT.Regpat;
      Matches : Match_Array (1 .. 1);
   begin
      if Package_Line /= "" then
         Trace.Detail ("Extracting native version from pacman output: " &
                         Package_Line);

         Match (Regexp, Package_Line, Matches);
         if Matches (1) /= No_Match then
            Trace.Detail
              ("Candidate version string: "
               & Package_Line (Matches (1).First .. Matches (1).Last));
            return
              Version_Outcomes.New_Result
                (Semantic_Versioning.Parse
                   (Package_Line (Matches (1).First .. Matches (1).Last),
                    Relaxed => True)); -- Relaxed because some pacman
                                       --  versions have extra version dash,
                                       --  e.g. 1.2.3-4
         else
            Trace.Debug
              ("Unexpected version format, could not identify version");
         end if;
      end if;

      return Version_Outcomes.Outcome_Failure ("could not be detected");
   end Detect;

   -------------
   -- Install --
   -------------

   overriding
   function Install (This : Deployer) return Outcome is
   begin
      Subprocess.Checked_Spawn
        ("pacman", Empty_Vector &
           "--needed" &
           "--noconfirm" &
           "-S" &
           This.Base.Package_Name);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Install;

end Alire.Origins.Deployers.System.Pacman;
