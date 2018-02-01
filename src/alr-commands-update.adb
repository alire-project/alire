with Alire.Index;
with Alire.OS_Lib; use Alire.OS_Lib;
with Alire.Query;

with Alr.Bootstrap;
with Alr.Checkout;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.Commands.Update is

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
            Alire.OS_Lib.Spawn ("git",
                                "pull --recurse-submodules=yes");
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
                                "submodule update --recursive " &
                                "deps" / "alire");
         end;
      end if;
   end Update_Index;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if not (Cmd.Alr or else Cmd.Index or else Cmd.Project) then
         Cmd.Full := True;
      end if;

      if Cmd.Full then
         Cmd.Alr     := True;
         Cmd.Index   := True;
         Cmd.Project := True;
      end if;

      if Cmd.Index then
         Update_Index;
      end if;

      if Cmd.Alr then
         Update_Alr;
      end if;

      if Cmd.Alr or else Cmd.Index then
         Bootstrap.Rebuild_With_Current_Project;
      end if;

      if Cmd.Project then
         if Cmd.Alr or else Cmd.Index then
            Bootstrap.Respawn_With_Canonical ("update --deps");
         else
            Upgrade;
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
         Cmd.Alr'Access,
         "", "--alr", "Update alr executable.");

      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.Index'Access,
         "", "--index", "Update projects database.");

      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.Project'Access,
         "", "--deps", "Update working project dependencies, if necessary.");

      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.Full'Access,
         "", "--full", "(Default) Update everything.");
   end Setup_Switches;

end Alr.Commands.Update;
