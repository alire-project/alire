with AAA.Table_IO;

with Alire.Config;
with Alire.Features.Index;
with Alire.Index_On_Disk;
with Alire.Utils;

package body Alr.Commands.Index is

   --  Forward declarations

   procedure Add (Cmd : Command);

   procedure List;

   ---------
   -- Add --
   ---------

   procedure Add (Cmd : Command) is
      Result : constant Alire.Outcome :=
                 Alire.Features.Index.Add
                   (Cmd.Add.all,
                    Cmd.Name.all,
                    Alire.Config.Indexes_Directory);
   begin
      if not Result.Success then
         Reportaise_Command_Failed (Alire.Message (Result));
      end if;
   end Add;

   ------------
   -- Delete --
   ------------

   procedure Delete (Name : String) is
      Indexes : constant Alire.Features.Index.Index_On_Disk_Set :=
                  Alire.Features.Index.Find_All
                    (Alire.Config.Indexes_Directory);
      Cursor  : Alire.Features.Index.Sets.Cursor := Indexes.First;
      Found   : Boolean := False;
   begin
      --  Find matching index and delete
      for I in 1 .. Natural (Indexes.Length) loop
         if Indexes (Cursor).Name = Name then
            Found := True;
            declare
               Result : constant Alire.Outcome := Indexes (Cursor).Delete;
            begin
               if Result.Success then
                  exit;
               else
                  Reportaise_Command_Failed (Alire.Message (Result));
               end if;
            end;
         end if;

         Cursor := Alire.Features.Index.Sets.Next (Cursor);
      end loop;

      if not Found then
         Reportaise_Command_Failed ("Given index not found: " & Name);
      end if;
   end Delete;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
      Enabled : Natural := 0;
   begin
      --  Check no multi-action
      Enabled := Enabled + (if Cmd.Add.all /= "" then 1 else 0);
      Enabled := Enabled + (if Cmd.Del.all /= "" then 1 else 0);
      Enabled := Enabled + (if Cmd.List then 1 else 0);

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
      else
         Reportaise_Wrong_Arguments ("Specify an index subcommand");
      end if;
   end Execute;

   ----------
   -- List --
   ----------

   procedure List is
      use Alire;

      Table : AAA.Table_IO.Table;
      Count : Natural := 0;
   begin
      Table.Append ("#").Append ("Name").Append ("URL").Append ("Path");

      for Index of Features.Index.Find_All (Config.Indexes_Directory) loop
         Count := Count + 1;
         Table.New_Row;
         Table
           .Append (Utils.Trim (Count'Img))
           .Append (Index.Name)
           .Append (Index.Origin)
           .Append (Index.Index_Directory);
      end loop;

      if Count > 0 then
         Table.Print;
      else
         Trace.Info ("No index configured.");
      end if;
   end List;


   --------------------
   -- Setup_Switches --
   --------------------

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
         Output      => Cmd.Del'Access,
         Long_Switch => "--del=",
         Argument    => "id",
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
   end Setup_Switches;

end Alr.Commands.Index;
