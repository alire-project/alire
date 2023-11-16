with AAA.Strings;

private with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.OS_Lib; use Alire.OS_Lib.Operators;

with CLIC.Config;

package Alire.Config is

   function DB return access constant CLIC.Config.Instance;

   type Level is (Global, Local);
   --  Ordering is important, as Globals are loaded first and overridden by any
   --  Local definition loaded later.

   subtype Config_Int is Long_Long_Integer;

   ---------------
   -- Built-ins --
   ---------------

   type Builtin_Kind is (Cfg_Int, Cfg_Float, Cfg_Bool,
                         Cfg_String, Cfg_Absolute_Path,
                         Cfg_Existing_Absolute_Path,
                         Cfg_Email, Cfg_GitHub_Login);

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
   function Get (This : Builtin_Option) return Config_Int;

   procedure Set_Locally (This : Builtin_Option; Value : String);

   procedure Set_Globally (This : Builtin_Option; Value : String);

   procedure Set (This  : Builtin_Option;
                  Level : Config.Level;
                  Value : String);

   procedure Set (This  : Builtin_Option;
                  Level : Config.Level;
                  Value : Boolean);

   procedure Set (This  : Builtin_Option;
                  Level : Config.Level;
                  Value : Config_Int);

   procedure Unset (This  : Builtin_Option;
                    Level : Config.Level);

   function New_Builtin (Key    : CLIC.Config.Config_Key;
                         Kind   : Builtin_Kind;
                         Def    : String := "";
                         Help   : String := "";
                         Public : Boolean := True;
                         Check  : CLIC.Config.Check_Import := null)
                         return Builtin_Option
     with Pre => Help /= "" or not Public;

   function New_Builtin (Key    : CLIC.Config.Config_Key;
                         Def    : Boolean;
                         Help   : String := "";
                         Public : Boolean := True;
                         Check  : CLIC.Config.Check_Import := null)
                         return Builtin_Option
     with Pre => Help /= "" or not Public;

private

   Config_Loaded : Boolean := False;

   DB_Instance : aliased CLIC.Config.Instance;
   --  The Alire user configuration database

   type Builtin_Option is tagged record
      Key     : Ada.Strings.Unbounded.Unbounded_String;
      Kind    : Builtin_Kind;
      Def     : Ada.Strings.Unbounded.Unbounded_String;
      Help    : Ada.Strings.Unbounded.Unbounded_String;
      Check   : CLIC.Config.Check_Import := null;
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

end Alire.Config;
