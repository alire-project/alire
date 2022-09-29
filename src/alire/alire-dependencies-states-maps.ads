with Ada.Containers.Indefinite_Ordered_Maps;

package Alire.Dependencies.States.Maps is

   package State_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Crate_Name, State);

   type Map is
     new State_Maps.Map
     and Interfaces.Tomifiable with null record;

   function Including (Base  : Map;
                       State : States.State)
                       return Map;
   --  Add or replace a state -- no merging of versions takes place

   function Merging (Base : Map;
                     Dep  : Dependencies.Dependency)
                     return Map;
   --  When Dep is new in Base, add Dep as Unknown, Unsolved, Unpinned.
   --  Otherwise, "and" Dep versions without modifying the state.

   function From_TOML (From : TOML_Adapters.Key_Queue) return Map;

   overriding function To_TOML (This : Map) return TOML.TOML_Value;

   function Image_One_Line (This : Map) return String;

end Alire.Dependencies.States.Maps;
