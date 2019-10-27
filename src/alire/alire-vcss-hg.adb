with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Utils;             use Alire.Utils;
with Alire.OS_Lib;
with Alire.Errors;

with GNAT.OS_Lib;

package body Alire.VCSs.Hg is

   -----------
   -- Clone --
   -----------

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path)
                   return Outcome
   is
      pragma Unreferenced (This);
      use GNAT.OS_Lib;
      Extra : constant String_Vector :=
        Empty_Vector & (if Log_Level < Trace.Info
                        then "-q"
                        else "-v");

      Commit_Arg : constant String_Vector :=
        (if Commit (From) /= ""
         then Empty_Vector & "-u" & Commit (From)
         else Empty_Vector);

   begin
      if Locate_Exec_On_Path ("hg") = null then
         return Outcome_Failure ("hg not found in path, aborting");
      end if;

      Trace.Detail ("Checking out [hg]: " & From);

      OS_Lib.Subprocess.Checked_Spawn
        ("hg",
         Empty_Vector &
           "clone" &
           "-y" &
           Commit_Arg &
           Extra &
           Repo (From) &
           Into);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Clone;

   ------------
   -- Update --
   ------------

   overriding
   function Update (This : VCS;
                    Repo : Directory_Path)
                    return Outcome
   is
      pragma Unreferenced (This);
      Guard : Directories.Guard (Directories.Enter (Repo))
        with Unreferenced;
      Extra : constant String_Vector :=
        Empty_Vector & (if Log_Level < Trace.Info
                        then "-q"
                        else "-v");
   begin
      OS_Lib.Subprocess.Checked_Spawn
        ("hg",
         Empty_Vector &
           "pull" &
           "-u" &
           Extra);
      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Update;

end Alire.VCSs.Hg;
