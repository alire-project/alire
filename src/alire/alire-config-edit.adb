with Ada.Text_IO;
with Ada.Directories;

with Alire.Environment;
with Alire.Platform;

with GNAT.Regexp;

with TOML.File_IO;

package body Alire.Config.Edit is

   use Ada.Strings.Unbounded;
   use TOML;

   type String_Access is access String;
   Config_Path : String_Access;

   procedure Write_Config_File (Table : TOML_Value; Path : Absolute_Path)
     with Pre => Table.Kind = TOML_Table;

   procedure Remove_From_Table (Table : TOML_Value; Key : Config_Key)
     with Pre => Table.Kind = TOML_Table;

   procedure Add_In_Table (Table : TOML_Value;
                           Key   : Config_Key;
                           Val   : TOML_Value)
     with Pre => Table.Kind = TOML_Table;

   -----------------------
   -- Write_Config_File --
   -----------------------

   procedure Write_Config_File (Table : TOML_Value; Path : Absolute_Path) is
      use Ada.Text_IO;
      use Ada.Directories;
      File : File_Type;
   begin

      --  Create the directory for the config file, in case it doesn't exists
      Create_Path (Containing_Directory (Path));

      Create (File, Out_File, Path);
      Trace.Debug ("Write config: '" & TOML.Dump_As_String (Table) & "'");
      Put (File, TOML.Dump_As_String (Table));
      Close (File);
   end Write_Config_File;

   -----------------------
   -- Remove_From_Table --
   -----------------------

   procedure Remove_From_Table (Table : TOML_Value; Key : Config_Key) is
      Id   : constant String := Utils.Split (Key, '.', Raises => False);
      Leaf : constant Boolean := Id = Key;
   begin
      if not Table.Has (Id) then
         --  The key doesn't exist
         return;
      end if;

      if Leaf then
         Table.Unset (Id);
      else
         declare
            Sub : constant TOML_Value := Table.Get (Id);
         begin
            if Sub.Kind = TOML_Table then
               Remove_From_Table (Sub, Utils.Split (Key, '.', Utils.Tail));
            else
               raise Program_Error;
            end if;
         end;
      end if;
   end Remove_From_Table;

   ------------------
   -- Add_In_Table --
   ------------------

   procedure Add_In_Table (Table : TOML_Value;
                           Key   : Config_Key;
                           Val   : TOML_Value)
   is
      Id   : constant String := Utils.Split (Key, '.', Raises => False);
      Leaf : constant Boolean := Id = Key;
   begin
      if Leaf then
         Table.Set (Id, Val);
         return;
      end if;

      if not Table.Has (Id) then
         --  The subkey doesn't exist, create a table for it
         Table.Set (Id, Create_Table);
      end if;

      declare
         Sub : constant TOML_Value := Table.Get (Id);
      begin
         if Sub.Kind = TOML_Table then
            Add_In_Table (Sub, Utils.Split (Key, '.', Utils.Tail), Val);
         else
            Raise_Checked_Error ("Configuration key already defined");
         end if;
      end;
   end Add_In_Table;

   -----------
   -- Unset --
   -----------

   procedure Unset (Path : Absolute_Path; Key : Config_Key) is
      Table : constant TOML_Value := Load_Config_File (Path);
   begin

      if Table.Is_Null then
         --  The configuration file doesn't exist or is not valid
         return;
      end if;

      Remove_From_Table (Table, Key);
      Write_Config_File (Table, Path);
   end Unset;

   ---------
   -- Set --
   ---------

   procedure Set (Path : Absolute_Path; Key : Config_Key; Value : String) is
      Table : TOML_Value := Load_Config_File (Path);

      To_Add : constant TOML_Value := To_TOML_Value (Value);
   begin
      if To_Add.Is_Null then
         Raise_Checked_Error ("Invalid configuration value: '" & Value & "'");
      end if;

      if not Valid_Builtin (Key, To_Add) then
         Raise_Checked_Error ("Invalid value '" & Value &
                                "' for builtin configuration. " &
                                Image (Kind_Of_Builtin (Key)) &
                                " expected.");
      end if;

      if Table.Is_Null then
         --  The configuration file doesn't exist or is not valid. Create an
         --  empty table.
         Table := TOML.Create_Table;
      end if;

      Add_In_Table (Table, Key, To_Add);

      Write_Config_File (Table, Path);

      Load_Config; -- Reload with the new set value
   end Set;

   -----------------
   -- Set_Locally --
   -----------------

   procedure Set_Locally (Key : Config_Key; Value : String) is
   begin
      Set (Filepath (Local), Key, Value);
   end Set_Locally;

   ------------------
   -- Set_Globally --
   ------------------

   procedure Set_Globally (Key : Config_Key; Value : String) is
   begin
      Set (Filepath (Global), Key, Value);
   end Set_Globally;

   --------------
   -- Filepath --
   --------------

   function Filepath (Lvl : Level) return Absolute_Path is
   begin
      case Lvl is
         when Global =>
            return Alire.Config.Edit.Path / "config.toml";
         when Local =>
            declare
               Candidate : constant String :=
                 Directories.Detect_Root_Path;
            begin
               if Candidate /= "" then
                  --  This file cannot have a .toml extension or the root
                  --  detection will not work.
                  return Candidate / "alire" / "config.toml";
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

   ----------------
   -- Is_Builtin --
   ----------------

   function Is_Builtin (Key : Config_Key) return Boolean
   is (for some Cfg of Builtins => To_String (Cfg.Key) = Key);

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

   -----------------
   -- Load_Config --
   -----------------

   procedure Load_Config is
   begin
      Config_Map.Clear;

      for Lvl in Level loop

         if Lvl /= Local or else Directories.Detect_Root_Path /= "" then
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

      --  Set variables elsewhere

      Platform.Disable_Distribution_Detection :=
        Get (Keys.Distribution_Disable_Detection, False);
      if Platform.Disable_Distribution_Detection then
         Trace.Debug ("Distribution detection disabled by configuration");
      end if;

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

begin
   Load_Config;

end Alire.Config.Edit;
