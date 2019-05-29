with Ada.Exceptions;

with Alire.Directories;
with Alire.OS_Lib.Subprocess;

package body Alire.VCSs.Git is

   -----------
   -- Clone --
   -----------

   function Clone (This : VCS;
                   From : URL;
                   Into : Platform_Independent_Path)
                   return Outcome
   is
      pragma Unreferenced (This);
      Extra : constant String :=
                (if Log_Level < Trace.Info
                 then "-q "
                 else "--progress ");
   begin
      Trace.Detail ("Checking out [git]: " & From);

      declare
         Exit_Code : constant Integer := OS_Lib.Subprocess.Spawn
           ("git", "clone " & Extra & Repo (From) & " " & Into);
      begin
         if Exit_Code /= 0 then
            return Outcome_Failure ("git clone exited with code:" &
                                    Exit_Code'Img);
         end if;
      end;

      if Commit (From) /= "" then
         declare
            Guard : Directories.Guard (Directories.Enter (Into))
              with Unreferenced;
            Exit_Code : constant Integer := OS_Lib.Subprocess.Spawn
              ("git", "reset --hard-q " & Commit (From));
         begin
            if Exit_Code /= 0 then
               return Outcome_Failure ("git reset exited with code:" &
                                         Exit_Code'Img);
            end if;
         end;
      end if;

      return Outcome_Success;
   exception
      when E : others =>
         return Outcome_From_Exception
           (E, "Could not check out repo: " & From &
               "; Ex: " & Ada.Exceptions.Exception_Message (E));
   end Clone;

   ------------
   -- Update --
   ------------

   function Update (This : VCS;
                    Repo : Platform_Independent_Path)
                    return Outcome
   is
      pragma Unreferenced (This);
      Guard : Directories.Guard (Directories.Enter (Repo))
        with Unreferenced;
      Extra : constant String :=
                (if Log_Level < Trace.Info
                 then "-q"
                 else "--progress ");
      Exit_Code : constant Integer :=
                    OS_Lib.Subprocess.Spawn ("git", "update " & Extra);
   begin
      if Exit_Code /= 0 then
         return Outcome_Failure ("git update exited with code: " &
                                   Exit_Code'Img);
      else
         return Outcome_Success;
      end if;
   end Update;

end Alire.VCSs.Git;
