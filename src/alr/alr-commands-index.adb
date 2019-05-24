with AAA.Table_IO;

with Alire.Config;
with Alire.Features.Index;
with Alire.Utils;

package body Alr.Commands.Index is

   --  Forward declarations

   procedure List;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.List then
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

      Table.Print;
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
         Output      => Cmd.List'Access,
         Long_Switch => "--list",
         Help        => "List configured indexes");
   end Setup_Switches;

end Alr.Commands.Index;
