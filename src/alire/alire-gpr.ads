with Alire.Utils;

package Alire.GPR with Preelaborate is

   type Variable_Kinds is (Enumeration, Free_String, External);
   --  Enumeration: Name + all possible values
   --  Free_String: Name without value
   --  External   : Name=Value

   type Variable (<>) is tagged private;

   function Kind (V : Variable) return Variable_Kinds;

   function Name (V : Variable) return String;

   function Image (V : Variable) return String;

   function Free_Variable (Name : String) return Variable;

   subtype Value is String;

   type Value_Vector is new Utils.String_Vector with null record;

   function Enum_Variable (Name   : String;
                           Values : Value_Vector'Class) return Variable;
   -- Used to represent a Typed enum with its possible values: Name = Value1 | Value2 ...

   function External_Value (Name : String;
                            Value : String) return Variable;
   --  Used to represent a pair Name=Value

   function Values (V : Variable) return Value_Vector'Class
     with Pre => V.Kind = Enumeration;

   function External_value (V : Variable) return String
     with Pre => V.Kind = External;

   function "or" (L, R : Value) return Value_Vector;
   function "or" (L : Value_Vector; R : Value) return Value_Vector;

   --  A collection of Var=Arg conform a scenario:
   --  These are used to store -X command-line arguments

   type Scenario is tagged private;

   Empty_Scenario : constant Scenario;

   procedure Add_Argument (S : in out Scenario; Var : String; Val : String);

   function As_Command_Line (S : Scenario) return String;
   --  -Xvar1=val -Xvar2=val ...

   function Is_Empty (S : Scenario) return Boolean;

private

   type Variable (Kind : Variable_Kinds; Name_Len : Positive) is tagged record
      Name : String (1 .. Name_Len);
      case Kind is
         when Enumeration =>
            Values : Value_Vector;
         when External =>
            Value  : Value_Vector; -- Only one element
         when Free_String =>
            null;
      end case;
   end record;

   function Kind (V : Variable) return Variable_Kinds is (V.Kind);

   function Name (V : Variable) return String is (V.Name);

   function Free_Variable (Name : String) return Variable is (Free_String, Name'Length, Name);

   function Enum_Variable (Name   : String;
                           Values : Value_Vector'Class) return Variable is
     (Enumeration, Name'Length, Name, Value_Vector (Values));

   function External_Value (Name  : String;
                            Value : String) return Variable is
     (External, Name'Length, Name, To_Vector (Value, 1));

   function Values (V : Variable) return Value_Vector'Class is (V.Values);

   function External_Value (V : Variable) return String is (V.Value.First_Element);

   function "or" (L, R : Value) return Value_Vector is (L & R);
   function "or" (L : Value_Vector; R : Value) return Value_Vector is (L & R);

   type Scenario is new Utils.String_Vector with null record;

   function Is_Empty (S : Scenario) return Boolean is (Utils.String_Vector (S).Is_Empty);

   Empty_Scenario : constant Scenario := (Utils.String_Vectors.Empty_Vector with null record);

end Alire.GPR;
