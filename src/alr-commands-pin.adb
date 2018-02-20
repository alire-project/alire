with Ada.Directories; use Ada.Directories;

with Alr.Files;
with Alr.Templates;

with Alire.Query;

package body Alr.Commands.Pin is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command)
   is
      pragma Unreferenced (Cmd);
      Guard : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      Requires_Project;

      declare
         Index_File : constant String := Files.Locate_Index_File (Project.Current.Element.Project);
         Success    :          Boolean;
         Deps       : constant Alire.Query.Instance :=
                        Alire.Query.Resolve (Project.Current.Element.Depends, Success);
      begin
         Log ("Backing up current dependency file to " & Index_File & ".old");
         Ada.Directories.Copy_File (Index_File, Index_File & ".old", "mode=overwrite");

         Templates.Generate_Prj_Alr (Deps,
                                     Project.Current.Element,
                                     Exact    => True,
                                     Filename => Index_File);
      end;
   end Execute;

end Alr.Commands.Pin;
