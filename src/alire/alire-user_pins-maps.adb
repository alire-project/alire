with Ada.Text_IO;

package body Alire.User_Pins.Maps is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (This : TOML_Adapters.Key_Queue) return Map is
      Result : Map;
   begin
      --  Each array entry may contain several pins (just like dependencies).
      --  We pass those one by one to the pin loader.

      for I in 1 .. This.Unwrap.Length loop
         declare
            Table : constant TOML.TOML_Value := This.Unwrap.Item (I);
         begin
            This.Assert (Table.Kind in TOML.TOML_Table,
                         "expected a table with <crate> = <override> but got: "
                         & Table.Kind'Image);

            for Key of Table.Keys loop
               declare
                  Crate : constant Crate_Name := +(+Key);
               begin
                  if Result.Contains (Crate) then
                     This.Checked_Error ("pin for crate " & (+Crate)
                                         & " is specified more than once");
                  end if;

                  --  Obtain a single pin

                  Result.Insert (Crate,
                                 User_Pins.From_TOML
                                   (This.Descend
                                      (Value   => Table.Get (Key),
                                       Context => +Key)));
               end;
            end loop;
         end;
      end loop;

      return Result;

   end From_TOML;

   -----------
   -- Print --
   -----------

   procedure Print (This : Map; Prefix : String := "") is
      use Ada.Text_IO;
   begin
      for I in This.Iterate loop
         Put_Line (Prefix
                   & This (I)
                   .To_Manifest_Line (Crate => Pin_Maps.Key (I)));
      end loop;
   end Print;

end Alire.User_Pins.Maps;
