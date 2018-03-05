with Alr.Bootstrap;
with Alr.Checkout;
with Alr.Hardcoded;
with Alr.Query;
with Alr.Spawn;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.Commands.Update is

   -------------
   -- Execute --
   -------------

   procedure Execute (From_Build : Boolean; Online : Boolean) is
      Cmd : Command;
   begin
      Cmd.From_Build := From_Build;
      Cmd.Online     := Online;

      Cmd.Execute;
   end Execute;


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
      Requires_Project;

      declare
         Success : Boolean;
         Needed  : constant Query.Instance := Query.Resolve (Project.Current.Depends
                                                              (Query.Platform_Properties),
                                                             Success,
                                                             Query_Policy);
      begin
         if not Success then
            Log ("Update failed");
            raise Command_Failed;
         end if;
         Checkout.To_Folder (Needed);
         Checkout.Generate_GPR_Builder (Needed, Project.Current);
         Log ("Update completed");
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
         Bootstrap.Rebuild_With_Current_Project;

         if Cmd.From_Build then
            Spawn.Alr (Cmd_Build);
         else
            Spawn.Alr (Cmd_Update);
         end if;
      else
         if Session_State >= Outdated then
            Upgrade;
         else
            Log ("Done");
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
