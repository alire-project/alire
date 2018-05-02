with Alire.Properties;
with Alire.Utils;

with Alr.Files;
with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Spawn;

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
      if Bootstrap.Session_State = Detached then
         Spawn.Session_Alr_Without_Return;
      end if;

      Trace.Always ("alr build is " & Bootstrap.Status_Line);
      Trace.Always ("alr version (from git tag) is " & Git_Tag);

      if Root.Is_Empty then
         Trace.Always ("alr root is empty");
      else
         Trace.Always ("alr root is " &
                       (if Root.Is_Released
                          then Root.Current.Release.Milestone.Image
                          else +Root.Project));
      end if;

--        Trace.Always ("alr session hash is " & Session.Hash);

      declare
         Guard : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
      begin
         Trace.Always ("alr project root detection has settled on path: " & OS_Lib.Current_Folder);
         Trace.Always ("alr is finding" & Files.Locate_Any_GPR_File'Img & " GPR project files");
         Trace.Always ("alr session state is " & Session_State'Img);

         if Session_State >= Detached then
            Trace.Always ("alr session folder is " & Hardcoded.Session_Folder);
         else
            Trace.Always ("alr session folder is " & Hardcoded.No_Session_Folder);
         end if;
      end;

      Log ("alr executable launched from " & Platform.Own_Executable, Always);
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
         OS_Lib.Spawn_Raw (Hardcoded.Scripts_Git_Fingerprint);
      end;

      Trace.Always ("platform fingerprint: " & Version.Fingerprint);
      Put ("platform properties:");
      for Prop of Platform.Properties loop
         Put (" " & Prop.Image);
      end loop;
      New_Line;
   end Execute;

   -------------
   -- Git_Tag --
   -------------

   function Git_Tag return String is
   begin
      -- FIXME this should be migrated to compiled-in git
      declare
         Guard : constant Folder_Guard :=
                   OS_Lib.Enter_Folder (Hardcoded.Alr_Src_Folder)
                   with Unreferenced;
         Output : Utils.String_Vector;
      begin
         OS_Lib.Spawn_And_Capture (Output, "git", "describe --all --always");
         return Alire.Utils.Split
           (Utils.Trim (Output.Flatten),
            '/',
            Side => Alire.Utils.Tail,
            From => Alire.Utils.Head,
            Raises => False);
      end;
   end Git_Tag;

end Alr.Commands.Version;
