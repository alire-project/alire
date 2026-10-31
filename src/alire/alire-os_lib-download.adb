with Ada.Directories;

with Alire.Directories;
with Alire.Errors;
with Alire.OS_Lib.Subprocess;
with Alire.Platforms.Current;

package body Alire.OS_Lib.Download is

   ----------
   -- File --
   ----------

   function File (URL      : String;
                  Filename : Any_Path;
                  Folder   : Directory_Path)
                  return Outcome
   is
      Archive_File : constant Directory_Path :=
                       Folder / Ada.Directories.Simple_Name (Filename);
   begin
      Trace.Debug ("Creating folder: " & Folder);
      Alire.Directories.Create_Tree (Folder);

      Trace.Detail ("Downloading file: " & URL);

      OS_Lib.Subprocess.Checked_Spawn
        ("curl",
         Empty_Vector &
           URL &
           "--location" &  -- allow for redirects at the remote host
           "--fail" &      --  fail fast with no output on HTTP errors
           (if Log_Level < Trace.Info
            then Empty_Vector & "--silent" & "--show-error"
            else Empty_Vector & "--progress-bar") &
           "--output" &
           Archive_File);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end File;

   ---------------------
   -- Mark_Executable --
   ---------------------

   procedure Mark_Executable (Path : Any_Path) is
      package Plat renames Alire.Platforms;
   begin
      case Plat.Current.Operating_System is
         when Plat.FreeBSD | Plat.OpenBSD | Plat.Linux | Plat.MacOS =>
            Alire.OS_Lib.Subprocess.Checked_Spawn
              ("chmod", Empty_Vector & "+x" & Path);

         when Plat.Windows | Plat.OS_Unknown =>
            null;
      end case;
   end Mark_Executable;

end Alire.OS_Lib.Download;
