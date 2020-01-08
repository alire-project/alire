with Alire.OS_Lib.Subprocess;
with Alire.Utils;             use Alire.Utils;
with Alire.Errors;

with GNAT.Regpat;

package body Alire.Origins.Deployers.Native.Apt is

   package Subprocess renames Alire.OS_Lib.Subprocess;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean
   is
      Output : constant Utils.String_Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("apt-cache",
                    Empty_Vector & "policy"
                    & This.Base.Package_Name);
   begin
      for Line of Output loop
         if Utils.Contains (Line, "Installed")
           and then
            not Utils.Contains (Line, "none")
         then
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
                 "Candidate: (?:[[:digit:]]:)*([\d\.]+).*";

      Output : constant Utils.String_Vector :=
                 Subprocess.Checked_Spawn_And_Capture
                   ("apt-cache",
                    Empty_Vector &
                      "-q" &
                      "policy" &
                      This.Base.Package_Name);
      use GNAT.Regpat;
      Matches : Match_Array (1 .. 1);
   begin
      for Line of Output loop
         if Contains (To_Lower_Case (Line), "candidate:")
           and then
            not Contains (To_Lower_Case (Line), "none")
         then
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

end Alire.Origins.Deployers.Native.Apt;
