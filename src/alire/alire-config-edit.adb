with Ada.Text_IO;

with Alire.Environment;
with Alire.Platform;
with Alire.Utils;

with CLIC.Config.Edit;
with CLIC.Config.Load;

package body Alire.Config.Edit is

   use Ada.Strings.Unbounded;
   use AAA.Strings;
   use TOML;

   type String_Access is access String;
   Config_Path : String_Access;

   -----------------
   -- Set_Locally --
   -----------------

   procedure Set_Locally (Key : CLIC.Config.Config_Key; Value : String) is
   begin
      if not CLIC.Config.Edit.Set (Filepath (Local), Key, Value) then
         Raise_Checked_Error ("Cannot set local config key");
      end if;

      --  Reload after change
      Load_Config;
   end Set_Locally;

   ------------------
   -- Set_Globally --
   ------------------

   procedure Set_Globally (Key : CLIC.Config.Config_Key; Value : String) is
   begin
      if not CLIC.Config.Edit.Set (Filepath (Global), Key, Value) then
         Raise_Checked_Error ("Cannot set global config key");
      end if;

      --  Reload after change
      Load_Config;
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

   ----------------
   -- Is_Builtin --
   ----------------

   function Is_Builtin (Key : CLIC.Config.Config_Key) return Boolean
   is (for some Cfg of Builtins => To_String (Cfg.Key) = Key);

   ---------------------
   -- Kind_Of_Builtin --
   ---------------------

   function Kind_Of_Builtin (Key : CLIC.Config.Config_Key)
                             return Builtin_Kind
   is
   begin
      for Ent of Builtins loop
         if To_String (Ent.Key) = Key then
            return Ent.Kind;
         end if;
      end loop;

      Raise_Checked_Error ("Kind is only valid for builtin config key");
   end Kind_Of_Builtin;

   -----------------
   -- Load_Config --
   -----------------

   procedure Load_Config is
   begin
      DB.Clear;

      for Lvl in Level loop
         if Lvl /= Local or else Directories.Detect_Root_Path /= "" then
            CLIC.Config.Load.From_TOML (C      => DB,
                                        Origin => Lvl'Img,
                                        Path   => Filepath (Lvl),
                                        Check  => Valid_Builtin'Access);
         end if;
      end loop;

      --  Set variables elsewhere

      Platform.Disable_Distribution_Detection :=
        DB.Get (Keys.Distribution_Disable_Detection, False);
      if Platform.Disable_Distribution_Detection then
         Trace.Debug ("Distribution detection disabled by configuration");
      end if;

   end Load_Config;

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

   function Valid_Builtin (Key : CLIC.Config.Config_Key;
                           Value : TOML_Value)
                           return Boolean
   is
      Result : Boolean := True;
   begin
      for Ent of Builtins loop
         if To_String (Ent.Key) = Key then
            case Ent.Kind is
            when Cfg_Int =>
               Result := Value.Kind = TOML_Integer;
            when Cfg_Float =>
               Result := Value.Kind = TOML_Float;
            when Cfg_Bool =>
               Result := Value.Kind = TOML_Boolean;
            when Cfg_String =>
               Result := Value.Kind = TOML_String;
            when Cfg_Absolute_Path =>
               Result := Value.Kind = TOML_String
                 and then Check_Absolute_Path (Value.As_String);
            when Cfg_Email =>
               Result := Value.Kind = TOML_String
                 and then Alire.Utils.Could_Be_An_Email (Value.As_String,
                                                         With_Name => False);
            when Cfg_GitHub_Login =>
               Result := Value.Kind = TOML_String
                 and then Utils.Is_Valid_GitHub_Username (Value.As_String);
            end case;

            exit;
         end if;
      end loop;

      if not Result then
         Trace.Error ("Invalid value '" & CLIC.Config.Image (Value)  &
                        "' for builtin configuration '" & Key & "'. " &
                        Image (Kind_Of_Builtin (Key)) &
                        " expected.");
      end if;

      return Result;
   end Valid_Builtin;

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

   -------------------
   -- Builtins_Info --
   -------------------

   function Builtins_Info return AAA.Strings.Vector is
      Results : AAA.Strings.Vector;
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
