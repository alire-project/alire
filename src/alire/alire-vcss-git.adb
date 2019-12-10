with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Errors;
with Alire.Utils;             use Alire.Utils;

package body Alire.VCSs.Git is

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
        Empty_Vector & (if Log_Level < Trace.Info
                        then "-q"
                        else "--progress");
   begin
      Trace.Detail ("Checking out [git]: " & From);

      OS_Lib.Subprocess.Checked_Spawn
        ("git",
         Empty_Vector & "clone" & Extra & Repo (From) & Into);

      if Commit (From) /= "" then
         declare
            Guard : Directories.Guard (Directories.Enter (Into))
              with Unreferenced;
         begin
            OS_Lib.Subprocess.Checked_Spawn
              ("git",
               Empty_Vector & "checkout" & Commit (From));
         end;
      end if;

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
                 then Empty_Vector & "-q "
                 else Empty_Vector & "--progress");
   begin
      OS_Lib.Subprocess.Checked_Spawn ("git", Empty_Vector & "pull" & Extra);
      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Update;

end Alire.VCSs.Git;
