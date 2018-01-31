with Ada.Directories;

with Alire.OS_Lib;

with Alr.Devel;
with Alr.Rolling;
with Alr.Templates;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.Bootstrap is

   ----------------------------------
   -- Check_If_Rolling_And_Respawn --
   ----------------------------------

   procedure Check_If_Rolling_And_Respawn is
      Alr_Exec : constant String := Alr_Src_Folder / "obj" / "alr";
   begin
      if Rolling.Enabled then
         Log ("alr up to date.");
      else
         if Is_Executable_File (Alr_Src_Folder / "obj" / "alr") then
            Log ("...");
            OS_Exit (Alire.OS_Lib.Spawn (Alr_Exec, Current_Command_Line));
            -- NOTE: THIS IS THE END OF EXECUTION OF THE OUTDATED alr
         else
            Log ("alr executable may be out of date, consider running ""alr update --alr"".");
         end if;
      end if;
   end Check_If_Rolling_And_Respawn;

   -------------------------
   -- Rebuild_Stand_Alone --
   -------------------------

   procedure Rebuild_Stand_Alone is
      Source_Folder   : constant String :=
                          (if Devel.Enabled then Ada.Directories.Current_Directory else Alr_Src_Folder);
      Folder_To_Index : constant String :=
                          Source_Folder / "deps" / "alire" / "index";
   begin
      Log ("Generating index for " & Folder_To_Index);
      Templates.Generate_Index (OS.Session_Folder, Folder_To_Index);

      Alire.OS_Lib.Spawn
        ("gprbuild",
         "-p -XROLLING=True -XSELFBUILD=True -XSESSION=" & OS.Session_Folder & " " &
         "-P" & (Source_Folder / "alr_env.gpr"));
   end Rebuild_Stand_Alone;

end Alr.Bootstrap;
