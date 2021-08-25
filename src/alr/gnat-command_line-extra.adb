with AAA.Strings;
with GNAT.Strings;

package body GNAT.Command_Line.Extra is

   function Verify_No_Duplicates (A, B : Command_Line_Configuration)
                                  return Boolean
   is
      Seen : AAA.Strings.Set;
      --  We track already set switches in this set; any re-appearance is
      --  reported.

      ------------
      -- Insert --
      ------------

      function Insert (Switch : String) return Boolean is
         --  Return True if OK; False otherwise.
      begin
         Seen.Insert (Switch);
         --  Raises Constraint_Error when element already exists.

         return True;
      exception
         when Constraint_Error =>
            return False;
      end Insert;

      ------------
      -- Insert --
      ------------

      function Insert (Sw : Switch_Definition) return Boolean is
         use all type GNAT.Strings.String_Access;
      begin
         --  Short version
         if Sw.Switch /= null and then
           Sw.Switch.all /= "" and then
           not Insert (Sw.Switch.all)
         then
            return False;
         end if;
         --  Long version
         if Sw.Long_Switch /= null and then
           Sw.Long_Switch.all /= "" and then
           not Insert (Sw.Long_Switch.all)
         then
            return False;
         end if;

         return True;
      end Insert;
   begin
      if A = null
        or else
         A.Switches = null
        or else
         B = null
        or else
         B.Switches = null
      then
         return True;
      end if;

      for Switch of A.Switches.all loop
         if not Insert (Switch) then
            return False;
         end if;
      end loop;

      for Switch of B.Switches.all loop
         if not Insert (Switch) then
            return False;
         end if;
      end loop;

      --  No duplication detected
      return True;
   end Verify_No_Duplicates;

   ---------------------
   -- For_Each_Switch --
   ---------------------

   procedure For_Each_Switch
     (Config : Command_Line_Configuration;
      Callback : not null access procedure
        (Switch, Long_Switch, Argument, Help : String))
   is
      use all type GNAT.Strings.String_Access;
   begin
      for S of Config.Switches.all loop
         Callback
           ((if S.Switch /= null then S.Switch.all else ""),
            (if S.Long_Switch /= null then S.Long_Switch.all else ""),
            (if S.Argument /= null then S.Argument.all else "ARG"),
            (if S.Help /= null then S.Help.all else ""));
      end loop;
   end For_Each_Switch;

end GNAT.Command_Line.Extra;
