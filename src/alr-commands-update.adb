with Alire.Index;
with Alire.OS_Lib; use Alire.OS_Lib;
with Alire.Query;

with Alr.Bootstrap;
with Alr.Checkout;

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
      if not Is_Directory (Bootstrap.Alr_Src_Folder) then
         Alire.OS_Lib.Spawn ("git",
                             "clone --recurse-submodules " &
                               "-b " & Bootstrap.Alr_Branch & " " &
                               String (Bootstrap.Alr_Repo) & " " &
                               Bootstrap.Alr_Src_Folder);
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
         Needed  : constant Alire.Index.Instance :=
                     Alire.Query.Resolve (Project.Current.Element.Depends, Success);
      begin
         if not Success then
            Log ("Update failed");
            raise Command_Failed;
         end if;
         Checkout.To_Folder (Needed);
         Checkout.Generate_GPR_Builder (Needed, Project.Current.Element);
      end;
   end Upgrade;

   ----------------
   -- Update_Alr --
   ----------------

   procedure Update_Alr is
   begin
      if not Is_Directory (Bootstrap.Alr_Src_Folder) then
         Checkout_If_Needed;
      else
         declare
            Guard : constant Folder_Guard :=
                      Enter_Folder (Bootstrap.Alr_Src_Folder)
              with Unreferenced;
         begin
            Alire.OS_Lib.Spawn ("git", "pull --recurse-submodules=yes");
            Alire.OS_Lib.Spawn ("git", "submodule update --recursive --remote");
         end;
      end if;
   end Update_Alr;

   ------------------
   -- Update_Index --
   ------------------

   procedure Update_Index is
   begin
      if not Is_Directory (Bootstrap.Alr_Src_Folder) then
         Checkout_If_Needed;
      else
         declare
            Guard : constant Folder_Guard :=
                      Enter_Folder (Bootstrap.Alr_Src_Folder)
              with Unreferenced;
         begin
            Alire.OS_Lib.Spawn ("git",
                                "submodule update --recursive --remote " &
                                "deps" / "alire");
         end;
      end if;
   end Update_Index;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Online then
         Log ("Checking remote repositories:");
         Update_Index;
         Update_Alr;
         Bootstrap.Rebuild_With_Current_Project;

         if Cmd.From_Build then
            Bootstrap.Respawn_With_Canonical ("build" & Current_Global_Switches);
         else
            Bootstrap.Respawn_With_Canonical ("update " & Current_Global_Switches);
         end if;
      else
         Upgrade;
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
