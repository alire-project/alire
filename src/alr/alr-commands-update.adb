with Alr.Checkout;
with Alr.Paths;
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
      if not Is_Directory (Paths.Alr_Src_Folder) then
         Spawn.Command ("git",
                        "clone " &
                          "-b " & Paths.Alr_Branch & " " &
                          String (Paths.Alr_Repo) & " " &
                          Paths.Alr_Src_Folder);
      end if;
   end Checkout_If_Needed;

   -------------
   -- Upgrade --
   -------------

   procedure Upgrade is
      --  The part concerning only to the project
   begin
      Requires_Full_Index;

      Requires_Project;

      declare
         Needed  : constant Query.Solution :=
                     Query.Resolve
                       (Root.Current.Dependencies.Evaluate (Platform.Properties),
                        Query_Policy);
      begin
         if not Needed.Valid then
            Reportaise_Command_Failed ("Update failed");
         end if;
         Checkout.To_Folder (Needed.Releases);
         Templates.Generate_Agg_Gpr (Needed.Releases, Root.Current);
         Trace.Detail ("Update completed");
      end;
   end Upgrade;

   ----------------
   -- Update_Alr --
   ----------------

   procedure Update_Alr is
   begin
      if not Is_Directory (Paths.Alr_Src_Folder) then
         Checkout_If_Needed;
      else
         declare
            Guard : Folder_Guard (Enter_Folder (Paths.Alr_Src_Folder))
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
         --  TODO: reload index
      end if;

      if Session_State > Outside then
         Upgrade;
      else
         Trace.Detail ("No project to upgrade");
      end if;
   end Execute;

   procedure Execute (Online : Boolean) is
      Cmd : Command := Command'(Online => Online);
   begin
      Execute (Cmd);
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
