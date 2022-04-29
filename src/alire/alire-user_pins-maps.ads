with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Errors;

with TOML;

package Alire.User_Pins.Maps is

   package Pin_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Crate_Name, Pin);

   type Map is new Pin_Maps.Map with null record;

   function From_TOML (This : TOML_Adapters.Key_Queue) return Map
     with Pre => This.Unwrap.Kind in TOML.TOML_Array
     or else raise Checked_Error
       with Errors.Set ("array expected but got a " & This.Unwrap.Kind'Image);

   procedure Print (This : Map; Prefix : String := "");
   --  Print to stdout each Pin.To_Manifest_Line

end Alire.User_Pins.Maps;
