with Alire.Properties;

with Alr.Files;
with Alr.Hardcoded;
with Alr.OS;
with Alr.OS_Lib;
--  with Alr.Session;

with GNAT.Compiler_Version;
with GNAT.Source_Info;

package body Alr.Commands.Version is

   package GNAT_Version is new GNAT.Compiler_Version;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
      use Ada.Text_IO;
   begin
      if Root.Is_Empty then
         Trace.Always ("alr root is empty");
      else
         Trace.Always ("alr root is " &
                       (if Root.Is_Released
                          then Root.Current.Release.Milestone.Image
                          else Root.Image));
      end if;

--        Trace.Always ("alr session hash is " & Session.Hash);

      declare
         Guard : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
      begin
         Trace.Always ("alr project root detection has settled on path: " & OS_Lib.Current_Folder);
         Trace.Always ("alr is finding" & Files.Locate_Any_GPR_File'Img & " GPR project files");
         Trace.Always ("alr session state is " & Session_State'Img);

         if Session_State >= Detached then
            Trace.Always ("alr session folder is " & Hardcoded.Session_Folder (Files.Locate_Metadata_File));
            if Session_State = Valid then
               Trace.Always ("alr internal session hash matches that of " & Files.Locate_Metadata_File);
            else
               if Root.Is_Empty then
                  Trace.Always ("alr candidate metadata file in sight: " & Files.Locate_Metadata_File);
               else
                  Trace.Always ("alr metadata (unmatched hash) file in sight: " & Files.Locate_Metadata_File);
               end if;
            end if;
         else
            Trace.Always ("alr session folder is " & Hardcoded.No_Session_Folder);
         end if;
      end;

      Log ("alr executable launched from " & OS.Own_Executable, Always);
      Log ("alr rolling source folder is " & Hardcoded.Alr_Src_Folder, Always);

      Log ("alr compiled on [" &
             GNAT.Source_Info.Compilation_ISO_Date & " " &
             GNAT.Source_Info.Compilation_Time & "] with GNAT version [" & GNAT_Version.Version & "]",
           Always);

      -- FIXME this is OS dependent
      declare
         Guard : constant Folder_Guard := OS_Lib.Enter_Folder (Hardcoded.Alr_Src_Folder)
           with Unreferenced;
      begin
         OS_Lib.Spawn_Raw (Hardcoded.Scripts_Version);
      end;

      Trace.Always ("platform fingerprint: " & OS.Fingerprint);
      Put ("platform properties:");
      for Prop of OS.Properties loop
         Put (" " & Prop.Image);
      end loop;
      New_Line;
   end Execute;

end Alr.Commands.Version;
