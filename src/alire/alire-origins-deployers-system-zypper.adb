with AAA.Strings; use AAA.Strings;

with Alire.OS_Lib.Subprocess;
with Alire.Errors;

package body Alire.Origins.Deployers.System.Zypper is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean
   is
      Output : constant AAA.Strings.Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("zypper",
                    Empty_Vector &
                       "--no-refresh" &
                       "--xmlout" &
                       "--quiet" &
                       "search" &
                       "--match-exact" &
                       "--installed-only" &
                       "--type" &
                       "package" &
                       This.Base.Package_Name,
                    Valid_Exit_Codes => (0, 104), -- returned when not found
                    Err_To_Out       => True);
   begin
      for Line of Output loop
         if Has_Prefix (Line, "<solvable status=""installed""") then
            return True;
         end if;
      end loop;

      return False;
   end Already_Installed;

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : Deployer) return Version_Outcomes.Outcome
   is
      Regexp : constant String :=
                 "edition=""([\d.]+)";

      Output : constant AAA.Strings.Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("zypper",
                    Empty_Vector &
                       "--no-refresh" &
                       "--xmlout" &
                       "--quiet" &
                       "search" &
                       "--match-exact" &
                       "--details" &
                       "--type" &
                       "package" &
                       This.Base.Package_Name,
                    Valid_Exit_Codes => (0, 104), -- returned when not found
                    Err_To_Out => True);

   begin
      for Line of Output loop
         if Has_Prefix (Line, "<solvable ") then
            Trace.Debug ("Extracting native version from zypper output: " &
                         Line);
            declare
               Match : constant String := Utils.First_Match (Regexp, Line);
            begin
               if Match /= "" then
                  Trace.Debug ("Candidate version string: " & Match);
                  return
                    Version_Outcomes.New_Result
                      (Semantic_Versioning.Parse (Match, Relaxed => True));
               else
                  Trace.Debug
                    ("Unexpected version format, could not identify version");
               end if;
            end;
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
   begin
      Subprocess.Checked_Spawn
        ("sudo", Empty_Vector &
           "zypper" &
           "--no-refresh" &
           "--non-interactive" &
           "--quiet" &
           "install" &
           "--name" &
           "--auto-agree-with-licenses" &
           This.Base.Package_Name);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Install;

end Alire.Origins.Deployers.System.Zypper;
