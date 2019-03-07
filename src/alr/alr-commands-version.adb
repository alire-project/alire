with Alire.Properties;
with Alire.Utils;

with Alr.Files;
with Alr.Paths;
with Alr.OS_Lib;

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

      if Root.Is_Empty then
         Trace.Always ("alr root is empty");
      else
         Trace.Always ("alr root is " & Root.Current.Milestone.Image);
      end if;

--        Trace.Always ("alr session hash is " & Session.Hash);

      declare
         Guard : Folder_Guard (Enter_Project_Folder) with Unreferenced;
      begin
         Trace.Always ("alr project root detection has settled on path: " & OS_Lib.Current_Folder);
         Trace.Always ("alr is finding" & Files.Locate_Any_GPR_File'Img & " GPR project files");
         Trace.Always ("alr session state is " & Session_State'Img);

         if Session_State > Outside then
            Trace.Always ("alr project folder is " & Files.Locate_Above_Project_Folder);
         end if;
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
