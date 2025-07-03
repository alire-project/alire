with Ada.Directories;
with Ada.Text_IO;

with Alire.Environment;
with Alire.Features;
with Alire.Paths;
with Alire.Platforms.Folders;
with Alire.Platforms.Current;
with Alire.Settings.Builtins;
with Alire.Utils.Text_Files;
with Alire.Version;
with Alire.Warnings;

with CLIC.Config.Edit;
with CLIC.Config.Load;

package body Alire.Settings.Edit is

   use Ada.Strings.Unbounded;
   use AAA.Strings;
   use TOML;

   package Adirs renames Ada.Directories;

   type String_Access is access String;
   Settings_Path : String_Access;

   -----------------
   -- Set_Locally --
   -----------------

   procedure Set_Locally (Key   : CLIC.Config.Config_Key;
                          Value : String;
                          Check : CLIC.Config.Check_Import := null)
   is
   begin
      if not CLIC.Config.Edit.Set (Filepath (Local), Key, Value, Check) then
         Raise_Checked_Error ("Cannot set local settings key");
      end if;

      --  Reload after change
      Load_Settings;
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
         Raise_Checked_Error ("Cannot set global settings key");
      end if;

      --  Reload after change
      Load_Settings;
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
         Trace.Debug ("Setting key " & Key & " unset from " & Level'Image
                      & "configuration at " & Filepath (Level));
         Load_Settings;
      else
         Trace.Debug ("Setting key " & Key & " requested to be unset at level "
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
              "Cannot set setting key '" & Key & "' at level " & Level'Image);
      --  Reload after change
      Load_Settings;
   end Set_Boolean;

   --------------
   -- Filepath --
   --------------

   function Filepath (Lvl : Level) return Absolute_Path is

      type Old_New is (Old, Current);

      --------------
      -- Location --
      --------------

      function Location (Which : Old_New) return Absolute_Path is
         File : constant String :=
                  (case Which is
                      when Old     => "config.toml",
                      when Current => Paths.Settings_File_Name);
      begin
         case Lvl is
            when Global =>
               return Alire.Settings.Edit.Path / File;
            when Local =>
               declare
                  Candidate : constant String :=
                                Directories.Detect_Root_Path;
               begin
                  if Candidate /= "" then
                     return Candidate / "alire" / File;
                  else
                     Raise_Checked_Error
                       ("Can only be used in an Alire directory");
                  end if;
               end;
         end case;
      end Location;

   begin
      --  Migrate file on the spot if necessary. This file is transparent to
      --  users so we do not emit visible messages.

      if Directories.Is_File (Location (Old)) and then
        not Directories.Is_File (Location (Current))
      then
         Trace.Debug ("Migrating settings file: "
                      & Location (Old) & " --> " & Location (Current));
         Adirs.Copy_File (Source_Name => Location (Old),
                          Target_Name => Location (Current));

         begin
            --  Insert a comment in the old config.toml
            Utils.Text_Files.Replace_Lines
              (Location (Old),
               Empty_Vector
               & String'("# config.toml has been replaced by settings.toml"
                         & " after alr 2.0")
               & ""
               & Utils.Text_Files.Lines (Location (Old)));
         exception
            --  Ensure we don't break anything trying to leaving the clues
            when E : others =>
               Log_Exception (E);
               Trace.Debug ("Failed when leaving clue about settings.toml");
         end;
      end if;

      return Location (Current);
   end Filepath;

   -------------------
   -- Load_Settings --
   -------------------

   procedure Load_Settings is
   begin
      DB_Instance.Clear;

      for Lvl in Level loop
         if Lvl /= Local or else Directories.Detect_Root_Path /= "" then
            CLIC.Config.Load.From_TOML (C      => DB_Instance,
                                        Origin => Lvl'Img,
                                        Path   => Filepath (Lvl),
                                        Check  => Valid_Builtin_Check (Lvl));
         end if;
      end loop;

      Settings_Loaded := True;

      --  Set variables elsewhere

      Platforms.Current.Disable_Distribution_Detection :=
        Settings.Builtins.Distribution_Disable_Detection.Get;
      if Platforms.Current.Disable_Distribution_Detection then
         Trace.Debug ("Distribution detection disabled by configuration");
      end if;
   end Load_Settings;

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
         if Version.Current < Features.Config_Deprecated then
            Warnings.Warn_Once (Msg, Level => Warning);
         else
            Raise_Checked_Error (Msg);
         end if;
      end if;

      if Settings_Path /= null then -- Case with switch (TODO)
         return Settings_Path.all;
      else
         return OS_Lib.Getenv
           (Environment.Settings,
            Default =>
              OS_Lib.Getenv (Environment.Config,
                             Default_Config_Path));
      end if;
   end Path;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path (Path : Absolute_Path) is
   begin
      if Settings_Path /= null then
         raise Constraint_Error with "Custom path already set";
      else
         Settings_Path := new String'(Path);
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

   function Valid_Builtin (Key    : CLIC.Config.Config_Key;
                           Value  : TOML_Value;
                           Global : Boolean)
                           return Boolean
   is
      Result : Boolean := True;
   begin
      for Ent of All_Builtins loop
         if To_String (Ent.Key) = Key then

            --  Verify the type/specific constraints

            case Ent.Kind is
            when Stn_Int =>
               Result := Value.Kind = TOML_Integer;
            when Stn_Float =>
               Result := Value.Kind = TOML_Float;
            when Stn_Bool =>
               Result := Value.Kind = TOML_Boolean;
            when Stn_String =>
               Result := Value.Kind = TOML_String;
            when Stn_Absolute_Path =>
               Result := Value.Kind = TOML_String
                 and then Check_Absolute_Path (Value.As_String);
            when Stn_Existing_Absolute_Path =>
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
            when Stn_Email =>
               Result := Value.Kind = TOML_String
                 and then Alire.Utils.Could_Be_An_Email (Value.As_String,
                                                         With_Name => False);
            when Stn_GitHub_Login =>
               Result := Value.Kind = TOML_String
                 and then Utils.Is_Valid_GitHub_Username (Value.As_String);
            end case;

            --  Error if Global_Only being set locally

            if Result and then Ent.Global_Only and then not Global then
               Trace.Error
                 ("Configuration key '" & Key & "' must be set globally.");
               return False;
            end if;

            --  Apply the own builtin check if any.

            if Result and then Ent.Check not in null then
               if not Ent.Check (Key, Value) then
                  Trace.Error
                    ("Invalid value '" & CLIC.Config.Image (Value)  &
                       "' for builtin configuration '" & Key & "'. " &
                       "Specific builtin check failed.");
                  return False;
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

   --------------------------
   -- Valid_Global_Builtin --
   --------------------------

   function Valid_Global_Builtin
      (Key : CLIC.Config.Config_Key; Value : TOML_Value) return Boolean
   is (Valid_Builtin (Key, Value, Global => True));

   -------------------------
   -- Valid_Local_Builtin --
   -------------------------

   function Valid_Local_Builtin
      (Key : CLIC.Config.Config_Key; Value : TOML_Value) return Boolean
   is (Valid_Builtin (Key, Value, Global => False));

   -------------------------
   -- Valid_Builtin_Check --
   -------------------------

   function Valid_Builtin_Check (Lvl : Level) return CLIC.Config.Check_Import
   is (case Lvl is
       when Global => Valid_Global_Builtin'Access,
       when Local => Valid_Local_Builtin'Access);

   -------------------
   -- Builtins_Info --
   -------------------

   function Builtins_Info return AAA.Strings.Vector is
      Results : AAA.Strings.Vector;
   begin
      for Ent of All_Builtins loop
         if Ent.Public then
            Results.Append (String'("- " & TTY.Bold (To_String (Ent.Key))
                            & " [" & TTY.Emph (Image (Ent.Kind)) & "]"
                            & "[Default:" & TTY.Terminal (To_String (Ent.Def))
                            & "]"));
            Results.Append (To_String (Ent.Help));
            Results.Append ("");
         end if;
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
         if Ent.Public then
            Put (" - **`" & To_String (Ent.Key) & "`** ");
            Put ("[" & Image (Ent.Kind) & "]");
            Put_Line ("[Default:" & To_String (Ent.Def) & "]:");
            Put_Line ("   " & To_String (Ent.Help));
            New_Line;
         end if;
      end loop;
   end Print_Builtins_Doc;

end Alire.Settings.Edit;
