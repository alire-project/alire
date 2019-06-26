with Ada.Exceptions;

with Alire.Directories;
with Alire.OS_Lib.Subprocess;

package body Alire.VCSs.SVN is

   -----------
   -- Clone --
   -----------

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Platform_Independent_Path)
                   return Outcome
   is
      pragma Unreferenced (This);
      Extra : constant String :=
                (if Log_Level < Trace.Info
                 then "-q "
                 else "");
   begin
      Trace.Detail ("Checking out [svn]: " & From);

      declare
         Exit_Code : constant Integer := OS_Lib.Subprocess.Spawn
           ("svn",
            "checkout "
            & Extra
            & Repo (From)
            & (if Commit (From) /= "" then " -r" & Commit (From) else "")
            & " " & Into);
      begin
         if Exit_Code /= 0 then
            return Outcome_Failure ("svn checkout exited with code:" &
                                    Exit_Code'Img);
         end if;
      end;

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

   overriding
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
                 else "");
      Exit_Code : constant Integer :=
                    OS_Lib.Subprocess.Spawn ("svn", "update " & Extra);
   begin
      if Exit_Code /= 0 then
         return Outcome_Failure ("svn update exited with code: " &
                                   Exit_Code'Img);
      else
         return Outcome_Success;
      end if;
   end Update;

end Alire.VCSs.SVN;
