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
                                Style_Checks,
                                Ada_Version,
                                Source_Encoding);

   type Optimization_Kind is (Performance, Size, Debug);

   type Debug_Info_Kind is (No, Yes);

   type Runtime_Checks_Kind is (None, Default, Overflow, Everything);

   type Compile_Checks_Kind is (None, Warnings, Errors);

   type Contracts_Kind is (No, Yes);

   type Style_Checks_Kind is (No, Yes);

   type Ada_Version_Kind is (Compiler_Default,
                             --  This value means that no switch will be added
                             --  and it's up to the compiler to decide which
                             --  Ada version will be used.

                             Ada83, Ada95, Ada05, Ada12, Ada2022,
                             GNAT_Extensions);

   type Source_Encoding_Kind is
     (Compiler_Default, --  Default behavior of GNAT, no switches set (Latin1)
      UTF_8             --  -gnatW8, UTF-8 sources, full literals & identifiers
     );

   type Optimization_Switches (Custom : Boolean := False)
   is record
      case Custom is
         when True  => List : Switch_List;
         when False => Value : Optimization_Kind;
      end case;
   end record;

   type Debug_Info_Switches (Custom : Boolean := False)
   is record
      case Custom is
         when True  => List : Switch_List;
         when False => Value : Debug_Info_Kind;
      end case;
   end record;

   type Runtime_Checks_Switches (Custom : Boolean := False)
   is record
      case Custom is
         when True  => List : Switch_List;
         when False => Value : Runtime_Checks_Kind;
      end case;
   end record;

   type Compile_Checks_Switches (Custom : Boolean := False)
   is record
      case Custom is
         when True  => List : Switch_List;
         when False => Value : Compile_Checks_Kind;
      end case;
   end record;

   type Contracts_Switches (Custom : Boolean := False)
   is record
      case Custom is
         when True  => List : Switch_List;
         when False => Value : Contracts_Kind;
      end case;
   end record;

   type Style_Checks_Switches (Custom : Boolean := False)
   is record
      case Custom is
         when True  => List : Switch_List;
         when False => Value : Style_Checks_Kind;
      end case;
   end record;

   type Ada_Version_Switches (Custom : Boolean := False)
   is record
      case Custom is
         when True  => List : Switch_List;
         when False => Value : Ada_Version_Kind;
      end case;
   end record;

   type Source_Encoding_Switches (Custom : Boolean := False)
   is record
      case Custom is
         when True  => List : Switch_List;
         when False => Value : Source_Encoding_Kind;
      end case;
   end record;

   function Get_List (S : Optimization_Switches) return Switch_List;
   function Get_List (S : Debug_Info_Switches) return Switch_List;
   function Get_List (S : Runtime_Checks_Switches) return Switch_List;
   function Get_List (S : Compile_Checks_Switches) return Switch_List;
   function Get_List (S : Contracts_Switches) return Switch_List;
   function Get_List (S : Style_Checks_Switches) return Switch_List;
   function Get_List (S : Ada_Version_Switches) return Switch_List;
   function Get_List (S : Source_Encoding_Switches) return Switch_List;

   type Switches_Configuration is record
      Optimization    : Optimization_Switches;
      Debug_Info      : Debug_Info_Switches;
      Runtime_Checks  : Runtime_Checks_Switches;
      Compile_Checks  : Compile_Checks_Switches;
      Contracts       : Contracts_Switches;
      Style_Checks    : Style_Checks_Switches;
      Ada_Version     : Ada_Version_Switches;
      Source_Encoding : Source_Encoding_Switches;
   end record;

   function Get_List (C : Switches_Configuration) return Switch_List;

   Default_Release_Switches : constant Switches_Configuration
     := (Optimization    => (Custom => False, Value => Performance),
         Debug_Info      => (Custom => False, Value => No),
         Runtime_Checks  => (Custom => False, Value => Default),
         Compile_Checks  => (Custom => False, Value => None),
         Contracts       => (Custom => False, Value => No),
         Style_Checks    => (Custom => False, Value => No),
         Ada_Version     => (Custom => False, Value => Compiler_Default),
         Source_Encoding => (Custom => False, Value => UTF_8));

   Default_Validation_Switches : constant Switches_Configuration
     := (Optimization    => (Custom => False, Value => Performance),
         Debug_Info      => (Custom => False, Value => Yes),
         Runtime_Checks  => (Custom => False, Value => Everything),
         Compile_Checks  => (Custom => False, Value => Errors),
         Contracts       => (Custom => False, Value => Yes),
         Style_Checks    => (Custom => False, Value => No),
         Ada_Version     => (Custom => False, Value => Compiler_Default),
         Source_Encoding => (Custom => False, Value => UTF_8));

   Default_Development_Switches : constant Switches_Configuration
     := (Optimization    => (Custom => False, Value => Debug),
         Debug_Info      => (Custom => False, Value => Yes),
         Runtime_Checks  => (Custom => False, Value => Default),
         Compile_Checks  => (Custom => False, Value => Warnings),
         Contracts       => (Custom => False, Value => No),
         Style_Checks    => (Custom => False, Value => No),
         Ada_Version     => (Custom => False, Value => Compiler_Default),
         Source_Encoding => (Custom => False, Value => UTF_8));

private
   Empty_List : constant Switch_List :=
     (Switch_Lists.Empty_List with null record);
end Alire.Utils.Switches;
