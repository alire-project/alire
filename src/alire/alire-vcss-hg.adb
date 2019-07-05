with Ada.Exceptions;

with Alire.Directories;
with Alire.OS_Lib.Subprocess;

with Alire.OS_Lib;

with GNAT.OS_Lib;

package body Alire.VCSs.Hg is

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
      use GNAT.OS_Lib;
      Extra : constant String :=
                (if Log_Level < Trace.Info
                 then "-q "
                 else "-v ");
   begin
      if Locate_Exec_On_Path ("hg") = null then
         return Outcome_Failure ("hg not found in path, aborting");
      end if;

      Trace.Detail ("Checking out [hg]: " & From);

      declare
         Exit_Code : constant Integer := OS_Lib.Subprocess.Spawn
           ("hg",
            "clone -y "
            & (if Commit (From) /= "" then "-u " & Commit (From) else "")
            & Extra & Repo (From) & " " & Into);
      begin
         if Exit_Code /= 0 then
            return Outcome_Failure ("hg clone exited with code:" &
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
                 else "-v ");
      Exit_Code : constant Integer :=
                    OS_Lib.Subprocess.Spawn ("hg", "pull -u " & Extra);
   begin
      if Exit_Code /= 0 then
         return Outcome_Failure ("hg pull exited with code: " &
                                   Exit_Code'Img);
      else
         return Outcome_Success;
      end if;
   end Update;

end Alire.VCSs.Hg;
