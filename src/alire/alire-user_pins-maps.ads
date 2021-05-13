with Ada.Containers.Indefinite_Ordered_Maps;

package Alire.User_Pins.Maps is

   package Pin_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Crate_Name, Pin);

   type Map is new Pin_Maps.Map with null record;

   function From_TOML (This : TOML_Adapters.Key_Queue) return Map;

end Alire.User_Pins.Maps;
