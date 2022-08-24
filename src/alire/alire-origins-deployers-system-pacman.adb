with AAA.Strings; use AAA.Strings;

with Alire.OS_Lib.Subprocess;
with Alire.Errors;

package body Alire.Origins.Deployers.System.Pacman is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   ----------------------
   -- Get_Package_Line --
   ----------------------

   function Get_Package_Line (Package_Name : String) return String is

      Package_Match : constant String := "^" & Package_Name & "$";

      Output : constant AAA.Strings.Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("pacman",
                    Empty_Vector &
                      "-Ss" &
                      Package_Match,
                    Valid_Exit_Codes => (0, 1)); -- Returned when not found
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

   overriding
   function Already_Installed (This : Deployer) return Boolean is
      Pck    : String renames This.Base.Package_Name;

      Output : constant AAA.Strings.Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("pacman",
                    Empty_Vector &
                      "-Qq" &
                      This.Base.Package_Name,
                    Valid_Exit_Codes => (0, 1), -- Returned when not found
                    Err_To_Out       => True);
   begin
      if not Output.Is_Empty
        and then
          Output.First_Element = Pck
      then
         return True;
      else
         Trace.Detail ("Cannot find package '" & Pck &
                         "' in pacman installed list: '" &
                         Output.Flatten & "'");
         return False;
      end if;
   end Already_Installed;

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : Deployer) return Version_Outcomes.Outcome is

      Regexp : constant String := "^.* (?:\d+:)?([\d.]+)-?.*$";
      --  repo/pkg opt_epoch:x.x.x.x-release_within_version something_extra
      --  See https://wiki.archlinux.org/index.php/PKGBUILD#pkgver

      Package_Line : constant String :=
        Get_Package_Line (This.Base.Package_Name);

   begin
      if Package_Line /= "" then
         Trace.Detail ("Extracting native version from pacman output: " &
                         Package_Line);

         declare
            Match : constant String :=
                      Utils.First_Match (Regexp, Package_Line);
         begin
            if Match /= "" then
               Trace.Detail ("Candidate version string: " & Match);
               return
                 Version_Outcomes.New_Result
                   (Semantic_Versioning.Parse (Match, Relaxed => True));
               --  Versions in Arch can have more than tree numeric fields,
               --  which runs amok of semantic versioning. If this happens,
               --  the 4th and extra fields will go into the build part of
               --  the version, due to Relaxed parsing.
            else
               Trace.Detail
                 ("Unexpected version format, could not identify version");
            end if;
         end;
      end if;

      Trace.Debug ("System deployer could not detect: " & This.Base.Image);
      return Version_Outcomes.Outcome_Failure ("could not be detected",
                                               Report => False);
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
