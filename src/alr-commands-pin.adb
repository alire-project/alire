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
   begin
      Requires_Project;

      declare
         Deps : constant Query.Solution :=
                  Query.Resolve (Root.Platform_Dependencies,
                                 Query_Policy);
      begin
         if Deps.Valid then
            Templates.Generate_Prj_Alr (Templates.Unreleased,
                                        Root.Project,
                                        Deps => Deps.Releases.To_Dependencies);
            Spawn.Alr (Cmd_Update);
         else
            Trace.Error ("Could not resolve dependencies");
            raise Command_Failed;
         end if;
      end;
   end Execute;

end Alr.Commands.Pin;
