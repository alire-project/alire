with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;

with GNAT.Regexp;

with Alire.Directories;
with Alire.Environment;
with Alire.Platform;
with Alire.Root;

with TOML.File_IO;

package body Alire.Config is

   use Ada.Strings.Unbounded;
   use TOML;

   type Config_Value is record
      Source : Unbounded_String;
      Value  : TOML.TOML_Value;
      Lvl    : Level;
   end record;

   No_Config_Value : constant Config_Value := (Source => Null_Unbounded_String,
                                               Value  => No_TOML_Value,
                                               Lvl    => Global);

   package Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Config_Value,
      Hash            => Hash,
      Equivalent_Keys => "=");

   Config_Map : Config_Maps.Map;

   procedure Import (Table  : TOML.TOML_Value;
                     Lvl    : Level;
                     Source : String;
                     Prefix : String := "");
   --  Import TOML Table in the Config_Map global variable

   function Get (Key : Config_Key) return Config_Value;

   function Image (Val : TOML_Value) return String;

   type String_Access is access String;
   Config_Path : String_Access;

   -----------
   -- Image --
   -----------

   function Image (F : TOML.Any_Float) return String is
   begin
      case F.Kind is
         when Regular =>
            return Utils.Trim (F.Value'Image);
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
            return Utils.Trim (Val.As_Integer'Img);
         when TOML_Float =>
            return Image (Val.As_Float);
         when TOML_String =>
            return Val.As_String;
         when others =>
            --  This should have been filtered during import
            raise Program_Error;
      end case;
   end Image;

   ----------
   -- Path --
   ----------

   function Path return String is
   begin
      if Config_Path /= null then -- Case with switch (TODO)
         return Config_Path.all;
      else
         return OS_Lib.Getenv (Environment.Config,
                               Platform.Default_Config_Folder);
      end if;
   end Path;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path (Path : String) is
   begin
      if Config_Path /= null then
         raise Constraint_Error with "Custom path already set";
      else
         Config_Path := new String'(Path);
      end if;
   end Set_Path;

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

   ----------
   -- List --
   ----------

   function List (Filter      : String := ".*";
                  Show_Origin : Boolean := False)
                  return String
   is
      use GNAT.Regexp;

      Re     : constant Regexp := Compile (Filter, Glob => True);

      Result : Unbounded_String;
   begin
      for C in Config_Map.Iterate loop
         declare
            Val : constant Config_Value := Config_Map (C);
            Key : constant String := To_String (Config_Maps.Key (C));
         begin
            if Match (Key, Re) then
               if Show_Origin then
                  Append (Result, Val.Source & " (" & Val.Lvl'Img & "): ");
               end if;

               Append (Result, Key & "=");
               Append (Result, Image (Val.Value));
               Append (Result, ASCII.LF);
            end if;
         end;
      end loop;
      return To_String (Result);
   end List;

   --------------
   -- Filepath --
   --------------

   function Filepath (Lvl : Level) return Absolute_Path is
   begin
      case Lvl is
         when Global =>
            return Alire.Config.Path / "config";
         when Local =>
            declare
               Candidate : constant String :=
                 Directories.Detect_Root_Path;
            begin
               if Candidate /= "" then
                  --  This file cannot have a .toml extension or the root
                  --  detection will not work.
                  return Candidate / "alire" / "config";
               else
                  Raise_Checked_Error
                    ("Can only be used in an Alire directory");
               end if;
            end;
      end case;
   end Filepath;

   ------------
   -- Import --
   ------------

   procedure Import (Table  : TOML_Value;
                     Lvl    : Level;
                     Source : String;
                     Prefix : String := "")
   is
   begin
      for Ent of Iterate_On_Table (Table) loop
         declare
            Key : constant String :=
              (if Prefix = "" then "" else Prefix & ".") &
              To_String (Ent.Key);

         begin
            if not Is_Valid_Config_Key (Key) then
               Trace.Error ("Invalid configuration key '" & Key & "' in " &
                           "'" & Source & "'");
            elsif Ent.Value.Kind = TOML_Table then

               --  Recursive call on the table
               Import (Ent.Value, Lvl, Source, Key);
            else

               Trace.Debug ("Load config key: '" & Key & "' = '" &
                              Ent.Value.Kind'Img & "'");

               if Ent.Value.Kind  not in  TOML_String | TOML_Float |
                                          TOML_Integer | TOML_Boolean
               then
                  Trace.Error ("Invalid type '" & Ent.Value.Kind'Img &
                                 "' for key '" & Key &
                                 "' in configuration file '" &
                                 Source & "'");
                  Trace.Error ("'" & Key & "' is ignored");
               elsif not Valid_Builtin (Key, Ent.Value) then
                  Trace.Error ("Invalid value for builtin key '" & Key &
                                 "' in configuration file '" &
                                 Source & "'");
                  Trace.Error ("'" & Key & "' is ignored");
               else
                  --  Insert the config value, potentially replacing a previous
                  --  definition.
                  Config_Map.Include (To_Unbounded_String (Key),
                                      (Source => To_Unbounded_String (Source),
                                       Value  => Ent.Value,
                                       Lvl    => Lvl));
               end if;
            end if;
         end;
      end loop;
   end Import;

   -----------------
   -- Load_Config --
   -----------------

   procedure Load_Config is

   begin
      for Lvl in Level loop

         if Lvl /= Local or else Root.Current.Exists then

            declare
               Config : constant TOML_Value :=
                 Load_Config_File (Filepath (Lvl));
            begin
               if not Config.Is_Null then
                  Import (Config, Lvl, Source => Filepath (Lvl));
               end if;
            end;
         end if;
      end loop;
   end Load_Config;

   ----------------------
   -- Load_Config_File --
   ----------------------

   function Load_Config_File (Path : Absolute_Path) return TOML_Value is
   begin

      if GNAT.OS_Lib.Is_Read_Accessible_File (Path) then
         declare
            Config : constant TOML.Read_Result :=
              TOML.File_IO.Load_File (Path);
         begin
            if Config.Success then
               if Config.Value.Kind /= TOML.TOML_Table then
                  Trace.Error ("Bad config file '" & Path &
                                 "': TOML table expected.");
               else
                  return Config.Value;
               end if;
            else
               Trace.Detail ("error while loading '" & Path & "':");
               Trace.Detail
                 (Ada.Strings.Unbounded.To_String (Config.Message));
            end if;
         end;
      else
         Trace.Detail ("Config file is not readable or doesn't exist: '" &
                         Path & "'");
      end if;

      return No_TOML_Value;
   end Load_Config_File;

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

   -----------
   -- Image --
   -----------

   function Image (Kind : Builtin_Kind) return String
   is (case Kind is
          when Cfg_Int           => "Integer",
          when Cfg_Float         => "Float",
          when Cfg_Bool          => "Boolean",
          when Cfg_String        => "String",
          when Cfg_Absolute_Path => "Absolute path",
          when Cfg_Email         => "Email address",
          when Cfg_GitHub_Login  => "GitHub login");

   ---------------------
   -- Kind_Of_Builtin --
   ---------------------

   function Kind_Of_Builtin (Key : Config_Key) return Builtin_Kind is
   begin
      for Ent of Builtins loop
         if To_String (Ent.Key) = Key then
            return Ent.Kind;
         end if;
      end loop;

      Raise_Checked_Error ("Kind is only valid for builtin config key");
   end Kind_Of_Builtin;

   -------------------
   -- Builtins_Info --
   -------------------

   function Builtins_Info return Alire.Utils.String_Vector is
      use Alire.Utils;
      Results : Alire.Utils.String_Vector;
   begin
      for Ent of Builtins loop
         Results.Append (String'("- " & To_String (Ent.Key) &
                           " [" & Image (Ent.Kind) & "]"));
         Results.Append (To_String (Ent.Help));
         Results.Append ("");
      end loop;
      return Results;
   end Builtins_Info;

   ------------------------
   -- Print_Builtins_Doc --
   ------------------------

   procedure Print_Builtins_Doc is
      use Ada.Text_IO;
   begin
      for Ent of Builtins loop
         Put (" - **`" & To_String (Ent.Key) & "`** ");
         Put_Line ("[" & Image (Ent.Kind) & "]:");
         Put_Line ("   " & To_String (Ent.Help));
         New_Line;
      end loop;
   end Print_Builtins_Doc;

   ----------------
   -- Is_Builtin --
   ----------------

   function Is_Builtin (Key : Config_Key) return Boolean
   is (for some Cfg of Builtins => To_String (Cfg.Key) = Key);

   -------------------
   -- Valid_Builtin --
   -------------------

   function Valid_Builtin (Key : Config_Key; Value : TOML_Value)
                           return Boolean
   is
   begin
      for Ent of Builtins loop
         if To_String (Ent.Key) = Key then
            case Ent.Kind is
            when Cfg_Int =>
               return Value.Kind = TOML_Integer;
            when Cfg_Float =>
               return Value.Kind = TOML_Float;
            when Cfg_Bool =>
               return Value.Kind = TOML_Boolean;
            when Cfg_String =>
               return Value.Kind = TOML_String;
            when Cfg_Absolute_Path =>
               return Value.Kind = TOML_String
                 and then Check_Absolute_Path (Value.As_String);
            when Cfg_Email =>
               return Value.Kind = TOML_String
                 and then Utils.Could_Be_An_Email (Value.As_String,
                                                   With_Name => False);
            when Cfg_GitHub_Login =>
               return Value.Kind = TOML_String
                 and then Utils.Is_Valid_GitHub_Username (Value.As_String);
            end case;
         end if;
      end loop;

      --  Not a builtin
      return True;
   end Valid_Builtin;

begin
   Load_Config;
end Alire.Config;
