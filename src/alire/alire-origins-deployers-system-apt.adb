with Alire.OS_Lib.Subprocess;
with Alire.Utils;             use Alire.Utils;
with Alire.Errors;

with GNAT.Regpat;

package body Alire.Origins.Deployers.System.Apt is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean
   is
      Output : Utils.String_Vector;
   begin

      --  The following call is faster than using apt and the output does not
      --  depend on the system locale, so we can check the Status line safely.

      Subprocess.Checked_Spawn_And_Capture
                   ("dpkg",
                    Empty_Vector & "-s" & This.Base.Package_Name,
                    Output,
                    Err_To_Out => True);

      for Line of Output loop
         if Line = "Status: install ok installed" then
            return True;
         end if;
      end loop;

      return False;
   exception
      when Checked_Error =>
         --  This is normal for packages not installed:
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

      Output : Utils.String_Vector;

      use GNAT.Regpat;
      Matches : Match_Array (1 .. 1);
   begin
      Subprocess.Checked_Spawn_And_Capture
                   ("apt-cache",
                    Empty_Vector &
                      "-q" &
                      "show" &
                      This.Base.Package_Name,
                    Output,
                    Err_To_Out => True);

      for Line of Output loop
         if Contains (Line, "Version:") then
            Trace.Debug ("Extracting native version from apt output: " & Line);
            Match (Regexp, Line, Matches);
            if Matches (1) /= No_Match then
               Trace.Debug ("Candidate version string: "
                            & Line (Matches (1).First .. Matches (1).Last));
               return
                 Version_Outcomes.New_Result
                   (Semantic_Versioning.Parse
                      (Line (Matches (1).First .. Matches (1).Last),
                       Relaxed => True)); -- Relaxed because some Debian
                                          --  versions have extra version pts,
                                          --  e.g. 1.2.3.4
            else
               Trace.Debug
                 ("Unexpected version format, could not identify version");
            end if;
         end if;
      end loop;

      return Version_Outcomes.Outcome_Failure ("could not be detected");
   exception
      when Checked_Error =>
         --  Expected for package names not in the system
         return Version_Outcomes.Outcome_Failure ("does not exist in system");
   end Detect;

   -------------
   -- Install --
   -------------

   overriding
   function Install (This : Deployer) return Outcome is
   begin
      Subprocess.Checked_Spawn
        ("sudo", Empty_Vector &
           "apt-get" &
           "install" &
           "--no-remove" &
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
