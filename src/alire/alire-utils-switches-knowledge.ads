private with Ada.Containers.Indefinite_Ordered_Maps;

package Alire.Utils.Switches.Knowledge is

   function Get_Info (Sw : Switch) return String;

   procedure Register (Sw   : Switch;
                       Info : String);

   procedure Populate;
   --  Populate the switches knowledge database with built-in switches

private

   package Switch_Info_Maps
   is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => Switch,
      Element_Type => String);

   DB : Switch_Info_Maps.Map;

end Alire.Utils.Switches.Knowledge;
