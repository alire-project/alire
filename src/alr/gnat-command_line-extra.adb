with Alire.Utils;

with GNAT.Strings;

package body GNAT.Command_Line.Extra is

   function Verify_No_Duplicates (Config : Command_Line_Configuration)
                                  return Boolean
   is
      Seen : Alire.Utils.String_Sets.Set;
      --  We track already set switches in this set; any re-appearance is
      --  reported.

      function Insert (Switch : String) return Boolean is
         --  Return True if OK; False otherwise.
      begin
         Seen.Insert (Switch);
         --  Raises Constraint_Error when element already exists.

         return True;
      exception
         when Constraint_Error =>
            Alire.Trace.Error ("Redefined switch: " & Switch);
            return False;
      end Insert;

      use all type GNAT.Strings.String_Access;
   begin
      for Switch of Config.Switches.all loop
         --  Short version
         if Switch.Switch /= null and then
           Switch.Switch.all /= "" and then
           not Insert (Switch.Switch.all)
         then
            return False;
         end if;
         --  Long version
         if Switch.Long_Switch /= null and then
           Switch.Long_Switch.all /= "" and then
           not Insert (Switch.Long_Switch.all)
         then
            return False;
         end if;
      end loop;

      --  No duplication detected
      return True;
   end Verify_No_Duplicates;

end GNAT.Command_Line.Extra;
