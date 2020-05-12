with Alire.Index;
with Alire.Utils.Tables;

with Alr.Utils;

with Simple_Logging;

package body Alr.Commands.List is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
      use Alr.Utils;

      Table  : Alire.Utils.Tables.Table;
      Search : constant String :=
                 (if Num_Arguments = 1
                  then Utils.To_Lower_Case (Argument (1))
                  else "");
      Found : Natural := 0;
   begin
      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Too many search arguments");
      end if;

      Requires_Full_Index;

      declare
         Busy : Simple_Logging.Ongoing :=
                  Simple_Logging.Activity ("Searching");
      begin
         for Crate of Alire.Index.All_Crates.all loop
            if Num_Arguments = 0 or else
              Contains (+Crate.Name, Search) or else
              Contains (+Crate.Name, Search)
            then
               Found := Found + 1;
               Table.New_Row;
               Table.Append (+Crate.Name);
               Table.Append (Crate.Description);
            end if;
            Busy.Step;
         end loop;
      end;

      if Found = 0 then
         Put_Line ("No hits");
      else
         Table.Print (Always, Separator => "  ");
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Shows the list of all indexed crates without further"
               & " details other than its common description. This command"
               & " is intended as a fast alternative to the 'search' command"
               & " when the information it provides is enough.")
     );

end Alr.Commands.List;
