with Alire.Releases;

with Alr.Commands.Update;
with Alr.Platform;
with Alr.Query;
with Alr.Root;
with Alr.Templates;

package body Alr.Commands.Pin is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command)
   is
      pragma Unreferenced (Cmd);
   begin
      Requires_Full_Index;
      Requires_Valid_Session;

      declare
         Sol : constant Query.Solution :=
           Query.Resolve (Root.Current.Release.Dependencies
                            (Platform.Properties),
                          Options => (Age       => Query_Policy,
                                      Detecting => <>,
                                      Hinting   => <>));
      begin
         if Sol.Valid then
            Templates.Generate_Prj_Alr
              (Root.Current.Release.Replacing
                 (Dependencies => Sol.Releases.To_Dependencies));

            Update.Execute;
         else
            Trace.Error ("Could not resolve dependencies");
            raise Command_Failed;
         end if;
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Pins dependencies to its resolved versions, and so prevent"
              & " future update commands from upgrading them."));

end Alr.Commands.Pin;
