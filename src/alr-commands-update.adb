with Alr.Bootstrap;
with Alr.Checkout;
with Alr.Hardcoded;
with Alr.Platform;
with Alr.Query;
with Alr.Spawn;
with Alr.Templates;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.Commands.Update is

   ------------------------
   -- Checkout_If_Needed --
   ------------------------

   procedure Checkout_If_Needed is
   begin
      if not Is_Directory (Hardcoded.Alr_Src_Folder) then
         Spawn.Command ("git",
                        "clone " &
                          "-b " & Hardcoded.Alr_Branch & " " &
                          String (Hardcoded.Alr_Repo) & " " &
                          Hardcoded.Alr_Src_Folder);
      end if;
   end Checkout_If_Needed;

   -------------
   -- Upgrade --
   -------------

   procedure Upgrade is
      --  The part concerning only to the project
      Guard   : constant Folder_Guard := Enter_Project_Folder with Unreferenced;
   begin
      -- Requires_Full_Index;
      -- Not anymore, thanks to explicit with requirements

      Requires_Project;

      declare
         Success : Boolean;
         Needed  : constant Query.Instance :=
                     Query.Resolve (Root.Current.Dependencies.Evaluate (Platform.Properties),
                                    Success,
                                    Query_Policy);
      begin
         if not Success then
            Reportaise_Command_Failed ("Update failed");
         end if;
         Checkout.To_Folder (Needed);
         Templates.Generate_Agg_Gpr (Needed, Root.Current);
         Trace.Detail ("Update completed");
      end;
   end Upgrade;

   ----------------
   -- Update_Alr --
   ----------------

   procedure Update_Alr is
   begin
      if not Is_Directory (Hardcoded.Alr_Src_Folder) then
         Checkout_If_Needed;
      else
         declare
            Guard : constant Folder_Guard := OS_Lib.Enter_Folder (Hardcoded.Alr_Src_Folder)
              with Unreferenced;
         begin
            Spawn.Command ("git", "pull");
            Spawn.Command ("git", "submodule update --init --recursive");
         end;
      end if;
   end Update_Alr;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Online then
         Log ("Checking remote repositories:");
         Update_Alr;

         Bootstrap.Rebuild (Bootstrap.Standalone);     -- Update rolling alr

         if Bootstrap.Session_State >= Detached then -- And this session one
            Bootstrap.Rebuild (Bootstrap.Session);
         end if;

         --  And launch updated exec without online (or it would restart endlessly)
         Spawn.Alr (Cmd_Update);
      else
         if Session_State >= Outdated then
            Upgrade;
         else
            Trace.Detail ("No project to upgrade");
         end if;
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
   begin
      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.Online'Access,
         "-o", "--online", "Perform online catalog update before recomputing dependencies");
   end Setup_Switches;

end Alr.Commands.Update;
