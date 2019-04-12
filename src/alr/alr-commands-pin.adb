with Alr.Commands.Update;
with Alr.Query;
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
            Templates.Generate_Prj_Alr
              (Root.Current.Release.Replacing
                 (Dependencies => Deps.Releases.To_Dependencies));

            Update.Execute;
         else
            Trace.Error ("Could not resolve dependencies");
            raise Command_Failed;
         end if;
      end;
   end Execute;

end Alr.Commands.Pin;
