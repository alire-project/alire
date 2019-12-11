with AAA.Table_IO;

with Alire.Config;
with Alire.Features.Index;
with Alire.Index_On_Disk;
with Alire.Utils;

package body Alr.Commands.Index is

   --  Forward declarations

   procedure Add (Cmd : Command);

   procedure List;

   procedure Reset_Community;

   procedure Update_All;

   ---------
   -- Add --
   ---------

   procedure Add (Cmd : Command) is
      Before : constant String := Cmd.Bfr.all;

      Result : constant Alire.Outcome :=
                 Alire.Features.Index.Add
                   (Origin => Cmd.Add.all,
                    Name   => Cmd.Name.all,
                    Under  => Alire.Config.Indexes_Directory,
                    Before => Before);
   begin
      Trace.Debug ("Index before ID = " & Before);

      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
      end if;
   end Add;

   ------------
   -- Delete --
   ------------

   procedure Delete (Name : String) is
      Result  : Alire.Outcome;
      Indexes : constant Alire.Features.Index.Index_On_Disk_Set :=
                  Alire.Features.Index.Find_All
                    (Alire.Config.Indexes_Directory, Result);
      Found   : Boolean := False;
   begin
      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
         return;
      end if;

      --  Find matching index and delete
      for Index of Indexes loop
         if Index.Name = Name then
            Found := True;
            declare
               Result : constant Alire.Outcome := Index.Delete;
            begin
               if Result.Success then
                  exit;
               else
                  Reportaise_Command_Failed (Alire.Message (Result));
               end if;
            end;
         end if;
      end loop;

      if not Found then
         Reportaise_Command_Failed ("Given index not found: " & Name);
      end if;
   end Delete;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command) is
      Enabled : Natural := 0;
   begin
      --  Check no multi-action
      Enabled := Enabled + (if Cmd.Add.all /= "" then 1 else 0);
      Enabled := Enabled + (if Cmd.Del.all /= "" then 1 else 0);
      Enabled := Enabled + (if Cmd.List then 1 else 0);
      Enabled := Enabled + (if Cmd.Update_All then 1 else 0);
      Enabled := Enabled + (if Cmd.Rset then 1 else 0);

      if Enabled /= 1 then
         Reportaise_Wrong_Arguments ("Specify exactly one index subcommand");
      end if;

      --  Dispatch to selected action
      if Cmd.Add.all /= "" then
         if Cmd.Name.all = "" then
            Reportaise_Wrong_Arguments
              ("Must provide a local name for new index");
         end if;
         Add (Cmd);
      elsif Cmd.Del.all /= "" then
         Delete (Cmd.Del.all);
      elsif Cmd.List then
         List;
      elsif Cmd.Update_All then
         Update_All;
      elsif Cmd.Rset then
         Reset_Community;
      else
         Reportaise_Wrong_Arguments ("Specify an index subcommand");
      end if;
   end Execute;

   ----------
   -- List --
   ----------

   procedure List is
      use Alire;

      Result  : Alire.Outcome;
      Indexes : constant Alire.Features.Index.Index_On_Disk_Set :=
                  Alire.Features.Index.Find_All
                    (Alire.Config.Indexes_Directory, Result);

      Table : AAA.Table_IO.Table;
      Count : Natural := 0;
   begin
      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
         return;
      end if;

      Table
        .Append ("#").Append ("Name").Append ("URL").Append ("Path");

      if Alire.Log_Level = Alire.Trace.Debug then
         Table.Append ("Priority");
      end if;

      for Index of Indexes loop
         Count := Count + 1;
         Table.New_Row;
         Table
           .Append (Utils.Trim (Count'Img))
           .Append (Index.Name)
           .Append (Index.Origin)
           .Append (Index.Index_Directory);

         if Alire.Log_Level = Alire.Trace.Debug then
            Table.Append (Utils.Trim (Index.Priority'Img));
         end if;
      end loop;

      if Count > 0 then
         Table.Print;
      else
         Trace.Info ("No index configured.");
      end if;
   end List;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Add, remove, list and update indexes used by the current"
               & " alr configuration.")
      .New_Line
      .Append ("Updating applies only to repository-stored indexes, in which"
               & " case a pull operation will be performed on them."
               & " An index initially set up with a specific commit will"
               & " not be updated.")
     );

   ---------------------
   -- Reset_Community --
   ---------------------

   procedure Reset_Community is
      Result : constant Alire.Outcome :=
                 Alire.Features.Index.Add_Or_Reset_Community;
   begin
      if not Result.Success then
         Reportaise_Command_Failed (Result.Message);
      end if;
   end Reset_Community;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration) is
   begin
      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Add'Access,
         Long_Switch => "--add=",
         Argument    => "URL",
         Help        => "Add an index");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Bfr'Access,
         Long_Switch => "--before=",
         Argument    => "NAME",
         Help        => "Priority order (defaults to last)");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Del'Access,
         Long_Switch => "--del=",
         Argument    => "NAME",
         Help        => "Remove an index");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.List'Access,
         Long_Switch => "--list",
         Help        => "List configured indexes");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Name'Access,
         Long_Switch => "--name=",
         Argument    => "NAME",
         Help        => "User given name for the index");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Update_All'Access,
         Long_Switch => "--update-all",
         Help        => "Update configured indexes");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Rset'Access,
         Long_Switch => "--reset-community",
         Help        => "Add the community index, or reset any local changes");
   end Setup_Switches;

   ----------------
   -- Update_All --
   ----------------

   procedure Update_All is
      Result : constant Alire.Outcome :=
                 Alire.Features.Index.Update_All
                   (Alire.Config.Indexes_Directory);
   begin
      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
      end if;
   end Update_All;

end Alr.Commands.Index;
