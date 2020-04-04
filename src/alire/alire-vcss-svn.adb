with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Errors;
with Alire.Utils;             use Alire.Utils;
with Alire.Utils.Tools;

package body Alire.VCSs.SVN is

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
      Extra : constant String_Vector :=
        (if Log_Level < Trace.Info
         then Empty_Vector & "-q"
         else Empty_Vector);

      Commit_Arg : constant String_Vector :=
        (if Commit (From) /= ""
         then Empty_Vector & String'("-r" & Commit (From))
         else Empty_Vector);
   begin
      Trace.Detail ("Checking out [svn]: " & From);

      --  Make sure svn is installed
      Utils.Tools.Check_Tool (Utils.Tools.Subversion);

      OS_Lib.Subprocess.Checked_Spawn
        ("svn",
            Empty_Vector &
              "checkout" &
              Extra &
              Repo (From) &
              Commit_Arg &
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
        (if Log_Level < Trace.Info
         then Empty_Vector & "-q"
         else Empty_Vector);
   begin
      --  Make sure svn is installed
      Utils.Tools.Check_Tool (Utils.Tools.Subversion);

      OS_Lib.Subprocess.Checked_Spawn
        ("svn", Empty_Vector & "update" & Extra);
      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Update;

end Alire.VCSs.SVN;
