with Alire.Properties;
with Alire.Utils;

with Alr.Files;
with Alr.OS_Lib;
with Alr.Paths;
with Alr.Root;

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
      Trace.Always ("alr build is " & Bootstrap.Status_Line);
      Trace.Always ("config folder is " & Paths.Alr_Config_Folder);
      Trace.Always ("source folder is " & Paths.Alr_Source_Folder);

      if not Root.Current.Is_Valid then
         Trace.Always ("alr root is empty");
      else
         Trace.Always ("alr root is " & Root.Current.Release.Milestone.Image);
      end if;

--        Trace.Always ("alr session hash is " & Session.Hash);

      declare
         Guard : Folder_Guard (Enter_Project_Folder) with Unreferenced;
      begin
         Trace.Always ("alr project root detection has settled on path: " & OS_Lib.Current_Folder);
         Trace.Always ("alr is finding" & Files.Locate_Any_GPR_File'Img & " GPR project files");
         Trace.Always ("alr session state is " & Session_State'Img);
      end;

      Log ("alr compiled on [" &
             GNAT.Source_Info.Compilation_ISO_Date & " " &
             GNAT.Source_Info.Compilation_Time & "] with GNAT version [" & GNAT_Version.Version & "]",
           Always);

      Trace.Always ("platform fingerprint: " & Version.Fingerprint);
      Put ("platform properties:");
      for Prop of Platform.Properties loop
         Put (" " & Prop.Image);
      end loop;
      New_Line;
   end Execute;

end Alr.Commands.Version;
