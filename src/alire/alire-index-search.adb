with Alire.Utils.Tables;

package body Alire.Index.Search is

   use AAA.Strings;

   ------------------
   -- Print_Crates --
   ------------------

   procedure Print_Crates (Substring : String := "") is
      Table  : Utils.Tables.Table;
      Found  : Natural := 0;
      Lookup : constant String := AAA.Strings.To_Lower_Case (Substring);

      Busy   : Simple_Logging.Ongoing :=
                 Simple_Logging.Activity ("Searching");
   begin
      for Crate of Alire.Index.All_Crates.all loop
         if Lookup = "" or else
           Contains (To_Lower_Case (+Crate.Name), Lookup) or else
           Contains (To_Lower_Case (Crate.Description), Lookup)
         then
            Found := Found + 1;
            Table.New_Row;
            Table.Append (Crate.TTY_Name);
            Table.Append (Crate.TTY_Description);
         end if;
         Busy.Step;
      end loop;

      if Found = 0 then
         Trace.Always ("No hits");
      else
         Table.Print (Always, Separator => "  ");
      end if;
   end Print_Crates;

end Alire.Index.Search;
