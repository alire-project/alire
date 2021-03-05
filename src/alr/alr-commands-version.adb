with Alire.Index;
with Alire.Properties;
with Alire.Roots.Optional;
with Alire.Utils.TTY;
with Alire.Utils.User_Input;

with Alr.Bootstrap;
with Alr.Files;
with Alr.OS_Lib;
with Alr.Paths;

with GNAT.Compiler_Version;
with GNAT.Source_Info;

package body Alr.Commands.Version is

   package GNAT_Version is new GNAT.Compiler_Version;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      use Ada.Text_IO;
      use all type Alire.Roots.Optional.States;
   begin
      Trace.Always ("Alr version: " & Alr.Version);
      Trace.Always ("Alire Library version: " & Alire.Version);
      Trace.Always ("alr status is " & Bootstrap.Status_Line);
      Trace.Always ("config folder is " & Paths.Alr_Config_Folder);
      Trace.Always ("source folder is " & Paths.Alr_Source_Folder);

      Trace.Always
        ("interaction flags are:"
         & " force:" & Alire.Force'Img
         & " not-interactive:" & Alire.Utils.User_Input.Not_Interactive'Img);

      case Cmd.Optional_Root.Status is
         when Outside =>
            Trace.Always ("alr root is empty");
         when Broken =>
            Trace.Always ("alr root has invalid metadata: "
                          & Alire.Utils.TTY.Error (Cmd.Optional_Root.Message));
         when Valid =>
            Trace.Always ("alr root is " & Cmd.Root.Release.Milestone.Image);
      end case;

      declare
         Guard : Folder_Guard (Enter_Working_Folder) with Unreferenced;
      begin
         Trace.Always ("alr root detection has settled on path: " &
                         OS_Lib.Current_Folder);
         Trace.Always ("alr is finding" & Files.Locate_Any_GPR_File'Img &
                         " GPR project files");
         Trace.Always
           ("alr session state is [" & Cmd.Optional_Root.Status'Img & "]");
      end;

      Log ("alr compiled on [" &
             GNAT.Source_Info.Compilation_ISO_Date & " " &
             GNAT.Source_Info.Compilation_Time & "] with GNAT version [" &
             GNAT_Version.Version & "]",
           Always);

      Trace.Always ("platform fingerprint: " & Version.Fingerprint);
      Put ("platform properties:");
      for Prop of Platform.Properties loop
         Put (" " & Prop.Image);
      end loop;
      New_Line;
      Trace.Always ("community index required branch: "
                    & Alire.Index.Community_Branch);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Shows assorted metadata about the alr executable,"
               & " and about the crate or sandbox found in the current"
               & " directory, if any."));

end Alr.Commands.Version;
