with TOML; use TOML;

package body Alire.Config is

   use Ada.Strings.Unbounded;

   function No_Config_Value return Config_Value
   is (Source => UStrings.Null_Unbounded_String,
       Value  => TOML.No_TOML_Value,
       Lvl    => Global);

   function Get (Key : Config_Key) return Config_Value;

   -----------
   -- Image --
   -----------

   function Image (F : TOML.Any_Float) return String is
   begin
      case F.Kind is
         when Regular =>
            return AAA.Strings.Trim (F.Value'Image);
         when NaN | Infinity =>
            return (if F.Positive then "" else "-") &
            (if F.Kind = NaN then "nan" else "inf");
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Val : TOML_Value) return String is
   begin
      case Val.Kind is
         when TOML_Boolean =>
            return (if Val.As_Boolean then "true" else "false");
         when TOML_Integer =>
            return AAA.Strings.Trim (Val.As_Integer'Img);
         when TOML_Float =>
            return Image (Val.As_Float);
         when TOML_String =>
            return Val.As_String;
         when others =>
            --  This should have been filtered during import
            raise Program_Error;
      end case;
   end Image;

   -------------
   -- Defined --
   -------------

   function Defined (Key : Config_Key) return Boolean is
   begin
      return Config_Map.Contains (+Key);
   end Defined;

   -------------------
   -- Get_As_String --
   -------------------

   function Get_As_String (Key : Config_Key) return String is
   begin
      if Defined (Key) then
         return Image (Get (Key).Value);
      else
         return "";
      end if;
   end Get_As_String;

   ---------
   -- Get --
   ---------

   function Get (Key : Config_Key) return Config_Value is

   begin
      if Defined (Key) then
         return Config_Map.Element (+Key);
      else
         return No_Config_Value;
      end if;
   end Get;

   --------------------------
   -- Get_With_Default_Gen --
   --------------------------

   function Get_With_Default_Gen (Key     : Config_Key;
                                  Default : Return_Type)
                                  return Return_Type
   is
      Val : constant Config_Value := Get (Key);
   begin
      if Val.Value.Is_Null then
         Trace.Detail ("Using default value for configuration '" & Key &
                         "': '" & Image (Default) & "'");
         return Default;

      elsif Val.Value.Kind /= Expected_TOML_Kind then
         Trace.Error ("Invalid type ('" & Val.Value.Kind'Img &
                        "') for configuration '" & Key & "'");
         Trace.Error ("in '" & To_String (Val.Source) & "'");
         Trace.Error (Type_Name & " expected");
         Trace.Error ("Using default: '" & Image (Default) & "'");
         return Default;

      else
         return TOML_As_Return_Type (Val.Value);
      end if;
   end Get_With_Default_Gen;

   ---------
   -- Get --
   ---------

   function Get (Key     : Config_Key;
                 Default : Boolean)
                 return Boolean
   is
      function Get_With_Default_Bool is new Get_With_Default_Gen
        (Boolean, TOML_Boolean, "Boolean", TOML.As_Boolean, Boolean'Image);

   begin
      return Get_With_Default_Bool (Key, Default);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Key     : Config_Key;
                 Default : String)
                 return String
   is
      function Id (Str : String) return String
      is (Str);

      function Get_With_Default_Str is new Get_With_Default_Gen
        (String, TOML_String, "String", TOML.As_String, Id);

   begin
      return Get_With_Default_Str (Key, Default);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Key     : Config_Key;
                 Default : TOML.Any_Integer)
                 return TOML.Any_Integer
   is
      function Get_With_Default_Int is new Get_With_Default_Gen
        (TOML.Any_Integer, TOML_Integer, "Integer", TOML.As_Integer,
         Any_Integer'Image);

   begin
      return Get_With_Default_Int (Key, Default);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Key     : Config_Key;
                 Default : TOML.Any_Float)
                 return TOML.Any_Float
   is
      function Get_With_Default_Int is new Get_With_Default_Gen
        (TOML.Any_Float, TOML_Float, "Float", TOML.As_Float, Image);
   begin
      return Get_With_Default_Int (Key, Default);
   end Get;

   -------------------
   -- To_TOML_Value --
   -------------------

   function To_TOML_Value (Str : String) return TOML_Value is
      Result : constant TOML.Read_Result := TOML.Load_String ("key=" & Str);
   begin
      if not Result.Success
        or else
         Result.Value.Kind /= TOML_Table
        or else
         not Result.Value.Has ("key")
      then

         --  Conversion failed

         --  Interpret as a string
         return Create_String (Str);
      else
         return Result.Value.Get ("key");
      end if;
   end To_TOML_Value;

end Alire.Config;
