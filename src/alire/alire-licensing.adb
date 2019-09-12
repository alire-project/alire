package body Alire.Licensing is

   -----------------
   -- From_String --
   -----------------

   function From_String (Label : String) return Licenses is
      use Ada.Strings.Unbounded;

      ULabel : constant Unbounded_String := +Label;
   begin
      --  TODO: use a map to do this efficiently.
      for License in Licenses'Range loop
         if License_Labels (License) = ULabel then
            return License;
         end if;
      end loop;
      return Unknown;
   end From_String;

end Alire.Licensing;
