with AAA.Strings; use AAA.Strings;

with Alire.OS_Lib.Subprocess;
with Alire.Errors;
with Alire.Warnings;

package body Alire.Origins.Deployers.System.Macports is

   --  Ada.Strings.Unbounded is use-visible via Alire.Origins.

   package Subprocess renames Alire.OS_Lib.Subprocess;

   -----------------------
   -- Already_Installed --
   -----------------------

   overriding function Already_Installed (This : Deployer) return Boolean
   is
      Installed : AAA.Strings.Vector;
   begin
      Trace.Debug ("already_installed? " & This.Base.Package_Name);

      if Subprocess.Unchecked_Spawn_And_Capture
        ("port",
         Empty_Vector & "echo" & "installed",
         Output     => Installed,
         Err_To_Out => True) /= 0
      then
         --  failed.
         Trace.Debug ("port failed to find any installed packages");
         return False;
      end if;

      --  We get a list of all the installed packages,
      --  name {spaces} '@' version

      Trace.Debug (Installed.Length'Image & " installed packages");

      for P of Installed loop
         Trace.Debug ("Checking '" & P & "'");
         if AAA.Strings.Head (P, ' ') = This.Base.Package_Name then
            Trace.Debug ("found '" & P & "'");
            return True;
         end if;
      end loop;

      return False;  -- until we've implemented Install
   end Already_Installed;

   ------------
   -- Detect --
   ------------

   --  Returns the package version, if the package exists, whether or
   --  not it's installed.
   --
   --  If it's not installed, what you get is the version that would
   --  be installed.

   overriding
   function Detect (This : Deployer) return Version_Outcomes.Outcome
   is
      Info : AAA.Strings.Vector;
   begin
      Trace.Debug ("detect? " & This.Base.Package_Name);

      if Subprocess.Unchecked_Spawn_And_Capture
        ("port",
         Empty_Vector & "info" & "--version" & This.Base.Package_Name,
         Output     => Info,
         Err_To_Out => False) /= 0
      then
         --  failed.
         Trace.Debug ("port failed to find " & This.Base.Package_Name);
         return Version_Outcomes.Outcome_Failure
           ("no candidate version found",
            Report => False);
      end if;

      --  Early exit if port is misbehaving. This can be due to outdated port
      --  definitions, see https://github.com/alire-project/alire/issues/1574

      if Integer (Info.Length) /= 1 then
         Warnings.Warn_Once
           ("Detecting system packages: `port info --version "
            & This.Base.Package_Name & "` returned"
            & Info.Length'Image & " lines where exactly one was expected.");
         Trace.Debug ("Port output was: " & Info.Flatten ("\n"));
         return Version_Outcomes.Outcome_Failure
           ("unexpected version output for " & This.Base.Package_Name,
            Report => False);
      end if;

      Trace.Debug ("port info output: " & Info (Info.First));
      declare
         Version : constant String := Trim (Tail (Info (Info.First), ':'));
      begin
         Trace.Debug (" -> version: '" & Version & "'");
         return Version_Outcomes.New_Result (Semantic_Versioning.Parse
                                               (Version,
                                                Relaxed => True));
      end;

   end Detect;

   -------------
   -- Install --
   -------------

   overriding
   function Install (This : Deployer) return Outcome is
   begin
      Trace.Debug ("hoping to install: " & This.Base.Image);
      Subprocess.Checked_Spawn
        ("sudo",
         Empty_Vector & "port" & "install" & This.Base.Package_Name);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Install;

end Alire.Origins.Deployers.System.Macports;
