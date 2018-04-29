with Alr.Query;
with Alr.Spawn;
with Alr.Templates;

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
         Success    :          Boolean;
         Deps       : constant Query.Instance :=
                        Query.Resolve
                          (Root.Platform_Dependencies,
                           Success,
                           Query_Policy);
      begin
         if Success then
            Templates.Generate_Prj_Alr (Templates.Unreleased,
                                        Root.Project,
                                        Deps => Deps.To_Dependencies);
            Spawn.Alr (Cmd_Update);
         else
            Trace.Error ("Could not resolve dependencies");
            raise Command_Failed;
         end if;
      end;
   end Execute;

end Alr.Commands.Pin;
