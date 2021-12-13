with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Alire.Utils.Switches
  with Preelaborate
is

   subtype Switch is String;
   type Switch_Access is access all Switch;

   package Switch_Lists
   is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Switch);

   type Switch_List is new Switch_Lists.List with null record;
   Empty_List : constant Switch_List;

   function Append (L : Switch_List;
                    S : Switch) return Switch_List;

   function Append (L : Switch_List;
                    S : not null Switch_Access) return Switch_List;

   function Flatten (L         : Switch_List;
                     Separator : String := " ")
                     return String;

   type Profile_Kind is (Release, Validation, Development);
   type Switches_Categories is (Optimization,
                                Debug_Info,
                                Contracts,
                                Compile_Checks,
                                Runtime_Checks,
                                Style_Checks);

   type Optimization_Kind is (Performance, Size, Debug, Custom);
   type Debug_Info_Kind is (No, Yes, Custom);
   type Runtime_Checks_Kind is (None, Default, Overflow, Everything, Custom);
   type Compile_Checks_Kind is (None, Warnings, Errors, Custom);
   type Contracts_Kind is (No, Yes, Custom);
   type Style_Checks_Kind is (No, Yes, Custom);

   type Optimization_Switches (Kind : Optimization_Kind := Performance)
   is record
      case Kind is
         when Custom => List : Switch_List;
         when others => null;
      end case;
   end record;

   type Debug_Info_Switches (Kind : Debug_Info_Kind := No)
   is record
      case Kind is
         when Custom => List : Switch_List;
         when others => null;
      end case;
   end record;

   type Runtime_Checks_Switches (Kind : Runtime_Checks_Kind := None)
   is record
      case Kind is
         when Custom => List : Switch_List;
         when others => null;
      end case;
   end record;

   type Compile_Checks_Switches (Kind : Compile_Checks_Kind := None)
   is record
      case Kind is
         when Custom => List : Switch_List;
         when others => null;
      end case;
   end record;

   type Contracts_Switches (Kind : Contracts_Kind := No)
   is record
      case Kind is
         when Custom => List : Switch_List;
         when others => null;
      end case;
   end record;

   type Style_Checks_Switches (Kind : Style_Checks_Kind := No)
   is record
      case Kind is
         when Custom => List : Switch_List;
         when others => null;
      end case;
   end record;

   function Get_List (S : Optimization_Switches) return Switch_List;
   function Get_List (S : Debug_Info_Switches) return Switch_List;
   function Get_List (S : Runtime_Checks_Switches) return Switch_List;
   function Get_List (S : Compile_Checks_Switches) return Switch_List;
   function Get_List (S : Contracts_Switches) return Switch_List;
   function Get_List (S : Style_Checks_Switches) return Switch_List;

   type Switches_Configuration is record
      Optimization   : Optimization_Switches;
      Debug_Info     : Debug_Info_Switches;
      Runtime_Checks : Runtime_Checks_Switches;
      Compile_Checks : Compile_Checks_Switches;
      Contracts      : Contracts_Switches;
      Style_Checks   : Style_Checks_Switches;
   end record;

   function Get_List (C : Switches_Configuration) return Switch_List;

   Default_Release_Switches : constant Switches_Configuration
     := (Optimization   => (Kind => Performance),
         Debug_Info     => (Kind => No),
         Runtime_Checks => (Kind => Default),
         Compile_Checks => (Kind => None),
         Contracts      => (Kind => No),
         Style_Checks   => (Kind => No));

   Default_Validation_Switches : constant Switches_Configuration
     := (Optimization   => (Kind => Performance),
         Debug_Info     => (Kind => Yes),
         Runtime_Checks => (Kind => Everything),
         Compile_Checks => (Kind => Errors),
         Contracts      => (Kind => Yes),
         Style_Checks   => (Kind => Yes));

   Default_Development_Switches : constant Switches_Configuration
     := (Optimization   => (Kind => Debug),
         Debug_Info     => (Kind => Yes),
         Runtime_Checks => (Kind => Default),
         Compile_Checks => (Kind => Warnings),
         Contracts      => (Kind => Yes),
         Style_Checks   => (Kind => Yes));

private
   Empty_List : constant Switch_List :=
     (Switch_Lists.Empty_List with null record);
end Alire.Utils.Switches;
