with Alire.Index.Hello;

with Alr.Bootstrap;
with Alr.Code;
with Alr.Platform;
with Alr.Query;
with Alr.Spawn;
with Alr.Templates;

package body Alr.Commands.Dev is

   ------------
   -- Custom --
   ------------

   procedure Custom is
   begin
      Code.Generate
        (Alire.Index.Hello.V_1_0_1.Depends
           (Platform.Properties)).Write ("alrcodetest.txt");
   end Custom;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Custom then
         Custom;
      end if;

      if Cmd.Raise_Except then
         raise Program_Error with "Raising forcibly";
      end if;

      if Cmd.Regenerate then
         Regenerate;
      end if;

      if Cmd.Respawn then
         Spawn.Updated_Alr_Without_Return;
      end if;

      if Cmd.Self_Compile then
         if Bootstrap.Session_State >= Detached then
            Bootstrap.Rebuild (Bootstrap.Session);
         else
            Bootstrap.Rebuild (Bootstrap.Standalone);
         end if;
      end if;
   end Execute;

   ----------------
   -- Regenerate --
   ----------------

   procedure Regenerate is
   begin
      Requires_Project;

      declare
         Guard : Folder_Guard := Enter_Project_Folder with Unreferenced;

         Ok    : Boolean := False;
         Deps  : constant Query.Instance :=
                   Query.Resolve (Root.Current.Dependencies.Evaluate (Platform.Properties),
                                  Ok,
                                  Query_Policy);
      begin
         if not Ok then
            Reportaise_Command_Failed ("Could not resolve dependencies");
         end if;

         Templates.Generate_Prj_Alr (Deps, Root.Current, Templates.Unknown);
         Templates.Generate_Agg_Gpr (Deps, Root.Current);
      end;
   end Regenerate;


   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Custom'Access,
                     "", "--custom",
                     "Execute current custom code");

      Define_Switch (Config,
                     Cmd.Raise_Except'Access,
                     "", "--raise",
                     "Raise an exception");

      Define_Switch (Config,
                     Cmd.Regenerate'Access,
                     "", "--regen",
                     "Regenerate alr/gpr meta files for current project");

      Define_Switch (Config,
                     Cmd.Respawn'Access,
                     "", "--respawn",
                     "Tries to respawn using rebuilt alr");

      Define_Switch (Config,
                     Cmd.Self_Compile'Access,
                     "", "--compile",
                     "Just self-compile.");
   end Setup_Switches;

end Alr.Commands.Dev;
