with AAA.Strings;

private with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.OS_Lib; use Alire.OS_Lib.Operators;

with CLIC.Config;

package Alire.Settings is

   function DB return access constant CLIC.Config.Instance;

   type Level is (Global, Local);
   --  Ordering is important, as Globals are loaded first and overridden by any
   --  Local definition loaded later.

   subtype Setting_Int is Long_Long_Integer;

   ---------------
   -- Built-ins --
   ---------------

   type Builtin_Kind is (Stn_Int, Stn_Float, Stn_Bool,
                         Stn_String, Stn_Absolute_Path,
                         Stn_Existing_Absolute_Path,
                         Stn_Email, Stn_GitHub_Login);

   function Image (Kind : Builtin_Kind) return String;

   function Is_Builtin (Key : CLIC.Config.Config_Key) return Boolean;

   function Kind_Of_Builtin (Key : CLIC.Config.Config_Key) return Builtin_Kind
     with Pre => Is_Builtin (Key);

   type Builtin_Option is tagged private;

   function Key (This : Builtin_Option) return CLIC.Config.Config_Key;

   function Is_Empty (This : Builtin_Option) return Boolean
     with Post => Is_Empty'Result = (This.Get = "");
   --  True if undefined or equal to "" (considering its own default)

   function Get (This : Builtin_Option) return String;
   function Get (This : Builtin_Option) return Boolean;
   function Get (This : Builtin_Option) return Setting_Int;

   function Get_Int (This : Builtin_Option) return Setting_Int renames Get;

   procedure Set_Locally (This : Builtin_Option; Value : String);

   procedure Set_Globally (This : Builtin_Option; Value : String);

   procedure Set (This  : Builtin_Option;
                  Level : Settings.Level;
                  Value : String);

   procedure Set (This  : Builtin_Option;
                  Level : Settings.Level;
                  Value : Boolean);

   procedure Set (This  : Builtin_Option;
                  Level : Settings.Level;
                  Value : Setting_Int);

   procedure Unset (This  : Builtin_Option;
                    Level : Settings.Level);

   function New_Builtin (Key         : CLIC.Config.Config_Key;
                         Kind        : Builtin_Kind;
                         Def         : String := "";
                         Help        : String := "";
                         Public      : Boolean := True;
                         Global_Only : Boolean := False;
                         Check       : CLIC.Config.Check_Import := null)
                         return Builtin_Option
     with Pre => Help /= "" or else not Public;

   function New_Builtin (Key         : CLIC.Config.Config_Key;
                         Def         : Boolean;
                         Help        : String := "";
                         Public      : Boolean := True;
                         Global_Only : Boolean := False;
                         Check       : CLIC.Config.Check_Import := null)
                         return Builtin_Option
     with Pre => Help /= "" or else not Public;

private

   Settings_Loaded : Boolean := False;

   DB_Instance : aliased CLIC.Config.Instance;
   --  The Alire user settings database

   type Builtin_Option is tagged record
      Key         : Ada.Strings.Unbounded.Unbounded_String;
      Kind        : Builtin_Kind;
      Def         : Ada.Strings.Unbounded.Unbounded_String;
      Help        : Ada.Strings.Unbounded.Unbounded_String;
      Check       : CLIC.Config.Check_Import := null;
      Global_Only : Boolean := False;
      Public      : Boolean := True;
   end record;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Builtin_Option) return Boolean
   is (This.Get = "");

   ---------
   -- Key --
   ---------

   function Key (This : Builtin_Option) return CLIC.Config.Config_Key
   is (+This.Key);

   package Builtin_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (CLIC.Config.Config_Key,
                                             Builtin_Option);

   All_Builtins : Builtin_Maps.Map;

end Alire.Settings;
