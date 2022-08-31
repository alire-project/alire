with Alire.Utils.Tables;
with Alire.Utils.TTY;

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

   ----------------------
   -- Print_Dependents --
   ----------------------

   procedure Print_Dependents (Release : Releases.Release) is
      Table  : Utils.Tables.Table;
      Busy   : Simple_Logging.Ongoing := Simple_Logging.Activity ("Searching");
   begin
      Table.Append ("CRATE").Append ("VERSION").Append ("DEPENDENCY").New_Row;

      for Crate of Alire.Index.All_Crates.all loop
         Busy.Step ("Searching [" & Crate.Name.As_String & "]");

         Release_Loop :
         for Rel of reverse Crate.Releases loop
            for Dep of Rel.Flat_Dependencies loop
               if Release.Satisfies (Dep) then
                  Table
                    .Append (Utils.TTY.Name (Crate.Name))
                    .Append (TTY.Version (Rel.Version.Image))
                    .Append (TTY.Version (Dep.Versions.Synthetic_Image))
                    .New_Row;
                  exit Release_Loop;
               end if;
            end loop;
         end loop Release_Loop;
      end loop;

      Table.Print (Always, Separator => "  ");
   end Print_Dependents;

end Alire.Index.Search;
