with Alire.Config.Edit;

package body Alire.Config is

   ---------
   -- Get --
   ---------

   function Get (This : Builtin_Option) return String
   is (DB.Get_As_String (+This.Key, +This.Def));

   ---------
   -- Get --
   ---------

   function Get (This : Builtin_Option) return Boolean
   is (DB.Get (+This.Key, Boolean'Value (+This.Def)));

   -----------------
   -- Set_Locally --
   -----------------

   procedure Set_Locally (This : Builtin_Option; Value : String) is
   begin
      Edit.Set_Locally (+This.Key, Value, This.Check);
   end Set_Locally;

   ------------------
   -- Set_Globally --
   ------------------

   procedure Set_Globally (This : Builtin_Option; Value : String) is
   begin
      Edit.Set_Globally (+This.Key, Value, This.Check);
   end Set_Globally;

   ---------
   -- Set --
   ---------

   procedure Set (This  : Builtin_Option;
                  Level : Config.Level;
                  Value : String)
   is
   begin
      Edit.Set (Level, +This.Key, Value, This.Check);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This  : Builtin_Option;
                  Level : Config.Level;
                  Value : Boolean)
   is
   begin
      Edit.Set_Boolean (Level, +This.Key, Value);
   end Set;

   -----------
   -- Unset --
   -----------

   procedure Unset (This  : Builtin_Option;
                    Level : Config.Level)
   is
   begin
      Edit.Unset (Level, +This.Key);
   end Unset;

   -----------
   -- Image --
   -----------

   function Image (Kind : Builtin_Kind) return String
   is (case Kind is
          when Cfg_Int                    => "Integer",
          when Cfg_Float                  => "Float",
          when Cfg_Bool                   => "Boolean",
          when Cfg_String                 => "String",
          when Cfg_Absolute_Path          => "Absolute path",
          when Cfg_Existing_Absolute_Path => "Absolute path already existing",
          when Cfg_Email                  => "Email address",
          when Cfg_GitHub_Login           => "GitHub login");

   ----------------
   -- Is_Builtin --
   ----------------

   function Is_Builtin (Key : CLIC.Config.Config_Key) return Boolean
   is (All_Builtins.Contains (Key));

   ---------------------
   -- Kind_Of_Builtin --
   ---------------------

   function Kind_Of_Builtin (Key : CLIC.Config.Config_Key)
                             return Builtin_Kind
   is
   begin
      if All_Builtins.Contains (Key) then
         return All_Builtins (Key).Kind;
      end if;

      Raise_Checked_Error ("Kind is only valid for builtin config key");
   end Kind_Of_Builtin;

   -----------------
   -- New_Builtin --
   -----------------

   function New_Builtin (Key    : CLIC.Config.Config_Key;
                         Kind   : Builtin_Kind;
                         Def    : String := "";
                         Help   : String := "";
                         Public : Boolean := True;
                         Check  : CLIC.Config.Check_Import := null)
                         return Builtin_Option
   is
   begin
      return Result : constant Builtin_Option := (Key   => +Key,
                                                  Kind  => Kind,
                                                  Def   => +Def,
                                                  Help  => +Help,
                                                  Check => Check)
      do
         if Public then
            All_Builtins.Insert (Key, Result);
         end if;
      end return;
   end New_Builtin;

   -----------------
   -- New_Builtin --
   -----------------

   function New_Builtin (Key    : CLIC.Config.Config_Key;
                         Def    : Boolean;
                         Help   : String := "";
                         Public : Boolean := True;
                         Check  : CLIC.Config.Check_Import := null)
                         return Builtin_Option
   is (New_Builtin (Key   => Key,
                    Kind   => Cfg_Bool,
                    Def    => Def'Image,
                    Help   => Help,
                    Public => Public,
                    Check  => Check));

end Alire.Config;
