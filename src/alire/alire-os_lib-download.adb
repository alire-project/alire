
with Alire.Errors;
with Alire.OS_Lib.Subprocess;
with Alire.Utils;             use Alire.Utils;
with Alire.Utils.Tools;

with GNATCOLL.VFS;

package body Alire.OS_Lib.Download is

   ----------
   -- File --
   ----------

   function File (URL      : String;
                  Filename : Any_Path;
                  Folder   : Directory_Path)
                  return Outcome
   is
      use GNATCOLL.VFS;

      Archive_File : constant Directory_Path := Folder / Filename;
   begin

      --  Make sure curl is installed
      Utils.Tools.Check_Tool (Utils.Tools.Curl);

      Trace.Debug ("Creating folder: " & Folder);
      Create (+Folder).Make_Dir;

      Trace.Detail ("Downloading file: " & URL);

      OS_Lib.Subprocess.Checked_Spawn
        ("curl",
         Empty_Vector &
           URL &
           "--location" &  -- allow for redirects at the remote host
           (if Log_Level < Trace.Info
            then Empty_Vector & "--silent"
            else Empty_Vector & "--progress-bar") &
           "--output" &
           Archive_File);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end File;

end Alire.OS_Lib.Download;
