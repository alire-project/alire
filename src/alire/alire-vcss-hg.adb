with AAA.Strings; use AAA.Strings;

with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Utils;             use Alire.Utils;
with Alire.OS_Lib;
with Alire.Errors;
with Alire.Utils.Tools;

package body Alire.VCSs.Hg is

   -----------
   -- Clone --
   -----------

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path;
                   Commit : String := "")
                   return Outcome
   is
      pragma Unreferenced (This);

      Extra : constant Vector :=
        Empty_Vector & (if Log_Level < Trace.Info
                        then "-q"
                        else "-v");

      Commit_Arg : constant Vector :=
        (if Commit /= ""
         then Empty_Vector & "-u" & Commit
         else Empty_Vector);

   begin

      --  Make sure hg is installed
      Utils.Tools.Check_Tool (Utils.Tools.Mercurial);

      Trace.Detail ("Checking out [hg]: " & From);

      OS_Lib.Subprocess.Checked_Spawn
        ("hg",
         Empty_Vector &
           "clone" &
           "-y" &
           Commit_Arg &
           Extra &
           Repo_URL (From) &
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
      Extra : constant Vector :=
        Empty_Vector & (if Log_Level < Trace.Info
                        then "-q"
                        else "-v");
   begin

      --  Make sure hg is installed
      Utils.Tools.Check_Tool (Utils.Tools.Mercurial);

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
