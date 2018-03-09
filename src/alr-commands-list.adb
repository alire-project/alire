with Alire.Projects;

with Alr.Utils;

with Table_IO;

package body Alr.Commands.List is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
      use Alr.Utils;

      Table  : Table_IO.Table;
      Search : constant String :=
                 (if Num_Arguments = 1
                  then Utils.To_Lower_Case (Argument (1))
                  else "");
      Found : Natural := 0;
   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many search arguments");
      end if;

      declare
         Busy : Utils.Busy_Prompt := Utils.Busy_Activity ("Searching...");
      begin
         for Name in Alire.Projects.Names loop
            if Num_Arguments = 0 or else
              Contains (To_Lower_Case (Name'Img), Search) or else
              Contains (To_Lower_Case (Alire.Projects.Description (Name)), Search)
            then
               Found := Found + 1;
               Table.New_Row;
               Table.Append (To_Lower_Case (Name'Img));
               Table.Append (Alire.Projects.Description (Name));
            end if;
            Busy.Step;
         end loop;
      end;

      if Found = 0 then
         Put_Line ("No hits");
      else
         Table.Print (Separator => "  ");
      end if;
   end Execute;

end Alr.Commands.List;
