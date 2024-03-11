with Ada.Text_IO;

with Alire.Environment;
with Alire.Features;
with Alire.Paths;
with Alire.Platforms.Folders;
with Alire.Platforms.Current;
with Alire.Settings.Builtins;
with Alire.Utils;
with Alire.Version.Semver;
with Alire.Warnings;

with CLIC.Config.Edit;
with CLIC.Config.Load;

package body Alire.Settings.Edit is

   use Ada.Strings.Unbounded;
   use AAA.Strings;
   use TOML;

   type String_Access is access String;
   Config_Path : String_Access;

   -----------------
   -- Set_Locally --
   -----------------

   procedure Set_Locally (Key   : CLIC.Config.Config_Key;
                          Value : String;
                          Check : CLIC.Config.Check_Import := null)
   is
   begin
      if not CLIC.Config.Edit.Set (Filepath (Local), Key, Value, Check) then
         Raise_Checked_Error ("Cannot set local config key");
      end if;

      --  Reload after change
      Load_Config;
   end Set_Locally;

   ------------------
   -- Set_Globally --
   ------------------

   procedure Set_Globally (Key   : CLIC.Config.Config_Key;
                          Value : String;
                           Check : CLIC.Config.Check_Import := null)
   is
   begin
      if not CLIC.Config.Edit.Set (Filepath (Global), Key, Value, Check) then
         Raise_Checked_Error ("Cannot set global config key");
      end if;

      --  Reload after change
      Load_Config;
   end Set_Globally;

   ---------
   -- Set --
   ---------

   procedure Set (Level : Settings.Level;
                  Key   : CLIC.Config.Config_Key;
                  Value : String;
                  Check : CLIC.Config.Check_Import := null)
   is
   begin
      case Level is
         when Local  => Set_Locally (Key, Value, Check);
         when Global => Set_Globally (Key, Value, Check);
      end case;
   end Set;

   -----------
   -- Unset --
   -----------

   procedure Unset (Level : Settings.Level;
                    Key   : CLIC.Config.Config_Key)
   is
   begin
      if CLIC.Config.Edit.Unset (Filepath (Level), Key, Quiet => True) then
         Trace.Debug ("Config key " & Key & " unset from " & Level'Image
                      & "configuration at " & Filepath (Level));
         Load_Config;
      else
         Trace.Debug ("Config key " & Key & " requested to be unset at level "
                      & Level'Image & " but it was already unset at "
                      & Filepath (Level));
      end if;
   end Unset;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (Level : Settings.Level;
                          Key   : CLIC.Config.Config_Key;
                          Value : Boolean;
                          Check : CLIC.Config.Check_Import := null)
   is
      function Set_Boolean_Impl is new CLIC.Config.Edit.Set_Typed
        (Boolean, TOML_Boolean, Boolean'Image);
   begin
      Assert (Set_Boolean_Impl (Filepath (Level), Key, Value, Check),
              "Cannot set config key '" & Key & "' at level " & Level'Image);
      --  Reload after change
      Load_Config;
   end Set_Boolean;

   --------------
   -- Filepath --
   --------------

   function Filepath (Lvl : Level) return Absolute_Path is
   begin
      case Lvl is
         when Global =>
            return Alire.Settings.Edit.Path / "config.toml";
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

   -----------------
   -- Load_Config --
   -----------------

   procedure Load_Config is
   begin
      DB_Instance.Clear;

      for Lvl in Level loop
         if Lvl /= Local or else Directories.Detect_Root_Path /= "" then
            CLIC.Config.Load.From_TOML (C      => DB_Instance,
                                        Origin => Lvl'Img,
                                        Path   => Filepath (Lvl),
                                        Check  => Valid_Builtin'Access);
         end if;
      end loop;

      Config_Loaded := True;

      --  Set variables elsewhere

      Platforms.Current.Disable_Distribution_Detection :=
        Settings.Builtins.Distribution_Disable_Detection.Get;
      if Platforms.Current.Disable_Distribution_Detection then
         Trace.Debug ("Distribution detection disabled by configuration");
      end if;
   end Load_Config;

   Default_Config_Path : constant Absolute_Path := Platforms.Folders.Config;

   ----------
   -- Path --
   ----------

   function Path return Absolute_Path is
      use type Version.Semver.Version;
      Unset : constant String := "unset";
      Msg   : constant String
        := "Environment variable " & Environment.Config
        & " is " & TTY.Error ("deprecated") & ". Use "
        & Environment.Settings & " instead.";
   begin
      --  Warn or fail depending on version
      if OS_Lib.Getenv (Environment.Config, Unset) /= Unset then
         if Version.Semver.Current < Features.Env_Alr_Config_Deprecated then
            Warnings.Warn_Once (Msg, Level => Warning);
         else
            Raise_Checked_Error (Msg);
         end if;
      end if;

      if Config_Path /= null then -- Case with switch (TODO)
         return Config_Path.all;
      else
         return OS_Lib.Getenv
           (Environment.Settings,
            Default =>
              OS_Lib.Getenv (Environment.Config,
                             Default_Config_Path));
      end if;
   end Path;

   ----------------
   -- Cache_Path --
   ----------------

   function Cache_Path return Absolute_Path
   is (if Builtins.Cache_Dir.Get /= "" then
          Builtins.Cache_Dir.Get
       elsif Path /= Default_Config_Path then
          Path / Paths.Cache_Folder_Inside_Working_Folder
       else
          Platforms.Folders.Cache);

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path (Path : Absolute_Path) is
   begin
      if Config_Path /= null then
         raise Constraint_Error with "Custom path already set";
      else
         Config_Path := new String'(Path);
      end if;
   end Set_Path;

   -----------------------
   -- Is_At_Default_Dir --
   -----------------------

   function Is_At_Default_Dir return Boolean
   is (Path = Platforms.Folders.Config);

   -------------------
   -- Valid_Builtin --
   -------------------

   function Valid_Builtin (Key : CLIC.Config.Config_Key;
                           Value : TOML_Value)
                           return Boolean
   is
      Result : Boolean := True;
   begin
      for Ent of All_Builtins loop
         if To_String (Ent.Key) = Key then

            --  Verify the type/specific constraints

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
            when Cfg_Existing_Absolute_Path =>
               Result := Value.Kind = TOML_String
                 and then Check_Absolute_Path (Value.As_String);
               if Result and then
                 not Directories.Is_Directory
                   (Directories.Full_Name (Value.As_String))
               then
                  Trace.Error
                    ("Given path '" & TTY.URL (Value.As_String)
                     & "' is not an existing directory, "
                     & "please create it beforehand or recheck it.");
                  return False;
               end if;
            when Cfg_Email =>
               Result := Value.Kind = TOML_String
                 and then Alire.Utils.Could_Be_An_Email (Value.As_String,
                                                         With_Name => False);
            when Cfg_GitHub_Login =>
               Result := Value.Kind = TOML_String
                 and then Utils.Is_Valid_GitHub_Username (Value.As_String);
            end case;

            exit when not Result;

            --  Apply the own builtin check if any.

            if Result and then Ent.Check not in null then
               if not Ent.Check (Key, Value) then
                  Trace.Error
                    ("Invalid value '" & CLIC.Config.Image (Value)  &
                       "' for builtin configuration '" & Key & "'. " &
                       "Specific builtin check failed.");
               end if;
            end if;

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

   -------------------
   -- Builtins_Info --
   -------------------

   function Builtins_Info return AAA.Strings.Vector is
      Results : AAA.Strings.Vector;
   begin
      for Ent of All_Builtins loop
         Results.Append (String'("- " & TTY.Bold (To_String (Ent.Key))
                         & " [" & TTY.Emph (Image (Ent.Kind)) & "]"
                         & "[Default:" & TTY.Terminal (To_String (Ent.Def))
                         & "]"));
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
      for Ent of All_Builtins loop
         Put (" - **`" & To_String (Ent.Key) & "`** ");
         Put ("[" & Image (Ent.Kind) & "]");
         Put_Line ("[Default:" & To_String (Ent.Def) & "]:");
         Put_Line ("   " & To_String (Ent.Help));
         New_Line;
      end loop;
   end Print_Builtins_Doc;

end Alire.Settings.Edit;
