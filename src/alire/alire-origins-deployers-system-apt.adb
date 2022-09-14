with AAA.Strings; use AAA.Strings;

with Alire.OS_Lib.Subprocess;
with Alire.Errors;

package body Alire.Origins.Deployers.System.Apt is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean
   is

      --  The following call is faster than using apt and the output does not
      --  depend on the system locale, so we can check the Status line safely.

      Output : constant AAA.Strings.Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("dpkg",
                    Empty_Vector & "-s" & This.Base.Package_Name,
                    Valid_Exit_Codes => (0, 1), -- returned when not found
                    Err_To_Out       => True);
   begin
      for Line of Output loop
         if Line = "Status: install ok installed" then
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
      --  See www.debian.org/doc/debian-policy/ch-controlfields.html#version
      Regexp : constant String :=
                 "Version: (?:[[:digit:]]:)*([\d\.]+).*";

      --  The show command of apt-cache is locale independent, so we can check
      --  the Version: field safely.

      Output    : constant AAA.Strings.Vector :=
                    Subprocess.Checked_Spawn_And_Capture
                      ("apt-cache",
                       Empty_Vector &
                         "-q" &
                         "show" &
                         This.Base.Package_Name,
                       Valid_Exit_Codes => (0, 100), -- Returned when not found
                       Err_To_Out       => True);

   begin
      for Line of Output loop
         if Contains (Line, "Version:") then
            Trace.Debug ("Extracting native version from apt output: " & Line);
            declare
               Match : constant String := Utils.First_Match (Regexp, Line);
            begin
               if Match /= "" then
                  Trace.Debug ("Candidate version string: " & Match);
                  return
                    Version_Outcomes.New_Result
                      (Semantic_Versioning.Parse (Match, Relaxed => True));
                  --  Relaxed because some Debian versions have extra version
                  --  pts, e.g. 1.2.3.4
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
      if Force then
         Put_Warning (TTY.Terminal ("--force") & " in effect, removal of "
                      & "system packages by apt is allowed");
      end if;

      Subprocess.Checked_Spawn
        ("sudo", Empty_Vector &
           "apt-get" &
           "install" &
           (if Force then Empty_Vector else To_Vector ("--no-remove")) &
           "-q" &
           "-q" &
           "-y" &
           This.Base.Package_Name);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Install;

end Alire.Origins.Deployers.System.Apt;
