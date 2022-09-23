with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Text_IO;

with AAA.Enum_Tools;
with AAA.Strings; use AAA.Strings;

with Alire.Containers;
with Alire_Early_Elaboration;
with Alire.Solutions;
with Alire.Releases;
with Alire.Roots;
with Alire.Origins;
with Alire.Warnings;
with Alire.Config;
with Alire.Config.Edit;

with Alire.Properties.Build_Profiles;
with Alire.Properties.Build_Switches;
with Alire.Utils.Switches; use Alire.Utils.Switches;
with Alire.Utils.Switches.Knowledge;
with Alire.Directories;
with Alire.Platforms.Current;
with Alire.Utils.TTY;

with TOML; use TOML;

package body Alire.Crate_Configuration is

   --  Used in parsing profile lists from command-line or configuration
   Profile_Assign : constant Character := '=';
   Profile_Split  : constant Character := ',';

   Must_Be_Set : constant UString := +"(variable without default still unset)";

   function Builtin_Build_Profile is new Typedef_From_Enum
     (Alire.Utils.Switches.Profile_Kind,
      "Build_Profile",
      Lower_Case => True);

   use Config_Type_Definition_Holder;

   subtype Crate_Name_Set is Containers.Crate_Name_Sets.Set;

   --  The Host info types below could be Enums instead of Strings. This would
   --  have the advantage of providing users the entire list of potential
   --  values. However, using enums in Ada would have a very high risk of
   --  breaking the code if future versions of Alire introduce new values
   --  for the enums.
   --
   --  For instance if a crate has a case statement on Alire_Host_Os without a
   --  "when others" branch. The same valid code could trigger "error: missing
   --  case values" on a future version of Alire. This is not acceptable,
   --  therefore we decide not to use enums here.

   type Config_Settings_Array is array (Integer range <>) of Config_Setting;

   ---------------
   -- Host_Info --
   ---------------
   --  This is a function instead of a constant to avoid creating temporary
   --  files at elaboration time, which in turn removes some errors when
   --  running in a non-writable directory. Although we do not cache the result
   --  here, the individual costly detections are cached at Platforms.Current.
   function Host_Info return Config_Settings_Array
   is
     ((Type_Def => To_Holder (String_Typedef ("Alire_Host_OS")),
       Value    => TOML.Create_String
         (To_Lower_Case (Alire.Platforms.Current.Operating_System'Img)),
       Set_By   => +"built-in"),

      (Type_Def => To_Holder (String_Typedef ("Alire_Host_Arch")),
       Value    => TOML.Create_String
         (To_Lower_Case (Alire.Platforms.Current.Host_Architecture'Img)),
       Set_By   => +"built-in"),

      (Type_Def => To_Holder (String_Typedef ("Alire_Host_Distro")),
       Value    => TOML.Create_String
         (To_Lower_Case (Alire.Platforms.Current.Distribution'Img)),
       Set_By   => +"built-in")
     );

   package TIO renames Ada.Text_IO;

   -------------------
   -- Build_Profile --
   -------------------

   function Build_Profile (This  : Global_Config;
                           Crate : Crate_Name)
                           return Utils.Switches.Profile_Kind
   is (This.Profile_Map (Crate));

   -----------------------
   -- Build_Profile_Key --
   -----------------------

   function Build_Profile_Key (Crate : Crate_Name) return String
   is (To_Lower_Case (Crate.As_String & "." & Builtin_Build_Profile.Name));

   -----------------------
   -- Set_Build_Profile --
   -----------------------

   procedure Set_Build_Profile (This    : in out Global_Config;
                                Crate   : Crate_Name;
                                Profile : Profile_Kind)
   is
      Key : constant String := Build_Profile_Key (Crate);
      Val : Config_Setting  := This.Var_Map (+Key);

      Prev_Profile : constant Profile_Kind := This.Profile_Map (Crate);
   begin
      --  Update config value that holds the profile value
      Val.Value  := TOML.Create_String (To_Lower_Case (Profile'Image));
      Val.Set_By := +"library client";
      This.Var_Map (+Key) := Val;

      --  Update profile itself
      This.Profile_Map.Include (Crate, Profile);

      --  And its setter
      This.Setter_Map.Include (Crate, User);

      Trace.Log
        ("Build profile of " & Crate.As_String & " set by user: "
         & Prev_Profile'Image & " --> "
         & Profile'Image,
         Level => (if Prev_Profile = Profile then Debug else Detail));
   end Set_Build_Profile;

   ------------------------
   -- Is_Default_Profile --
   ------------------------

   function Is_Default_Profile (This  : Global_Config;
                                Crate : Crate_Name)
                                return Boolean
   is
   begin
      return This.Setter_Map (Crate) = Default;
   end Is_Default_Profile;

   ----------------------
   -- Is_Reserved_Name --
   ----------------------

   function Is_Reserved_Name (Type_Name : String) return Boolean
   is (AAA.Strings.Has_Prefix (Type_Name, "alire_") or else
       Type_Name = "crate_version" or else
       Type_Name = "crate_name" or else
       Type_Name = "ada_compiler_switches" or else
       Type_Name = "c_compiler_switches" or else
       Type_Name = "ada_linker_switches" or else
       Type_Name = "c_linker_switches");
   --  Return True if Type_Name is reserved for Alire internal use

   ----------------------------
   -- Make_Build_Profile_Map --
   ----------------------------

   procedure Make_Build_Profile_Map (This     : in out Global_Config;
                                     Root     : in out Alire.Roots.Root;
                                     Rel_Vect : Crate_Name_Set)
   is
      use Properties.Build_Profiles;

      -----------------
      -- Set_Profile --
      -----------------

      procedure Set_Profile (Crate : Crate_Name; P : Profile_Kind) is
      begin
         if This.Profile_Map.Contains (Crate) then
            This.Profile_Map.Replace (Crate, P);
         else
            Raise_Checked_Error ("Unknown crate in build profile: '" &
                                   String'(+Crate) & "'");
         end if;
      end Set_Profile;

   begin

      --  Populate map with crates in the solution
      for Crate of Rel_Vect loop
         This.Profile_Map.Insert (Crate,
                                  (if Crate = Root.Name
                                   then Default_Root_Build_Profile
                                   else Default_Deps_Build_Profile));
         This.Setter_Map.Insert (Crate, Default);
      end loop;

      for Prop of Root.Release.On_Platform_Properties
        (Root.Environment,
         Properties.Build_Profiles.Variable'Tag)
      loop
         declare
            Prof : constant Properties.Build_Profiles.Variable
              := Properties.Build_Profiles.Variable (Prop);
         begin

            if Prof.Has_Wildcard then

               --  If wildcard is defined, apply it to all crates
               declare
                  Wildcard_Profile : constant Profile_Kind
                    := Prof.Wildcard;
               begin
                  for Cursor in This.Profile_Map.Iterate loop
                     This.Profile_Map.Replace_Element
                       (Cursor, Wildcard_Profile);
                     This.Setter_Map.Replace (Profile_Maps.Key (Cursor),
                                              Manifest);
                  end loop;
               end;
            end if;

            declare
               use Properties.Build_Profiles.Profile_Selection_Maps;
               Sel : constant Profile_Selection_Maps.Map
                 := Prof.Selection;
            begin
               for Cursor in Sel.Iterate loop
                  Set_Profile (Key (Cursor), Element (Cursor));
                  This.Setter_Map.Replace (Profile_Selection_Maps.Key (Cursor),
                                           Manifest);
               end loop;
            end;
         end;
      end loop;

      for Cursor in This.Profile_Map.Iterate loop
         --  Set build_Mode value in configuration variables
         This.Set_Value
           (Profile_Maps.Key (Cursor),
            (Name => +Builtin_Build_Profile.Name,
             Value => TOML.Create_String
               (To_Lower_Case (Profile_Maps.Element (Cursor)'Img))));
      end loop;

   end Make_Build_Profile_Map;

   -----------------------
   -- Make_Switches_Map --
   -----------------------

   procedure Make_Switches_Map (This     : in out Global_Config;
                                Root     : in out Alire.Roots.Root;
                                Rel_Vect : Crate_Name_Set)
   is
   begin
      for Crate of Rel_Vect loop
         declare
            Rel : constant Releases.Release := Root.Release (Crate);

            Profile : constant Profile_Kind
              := This.Profile_Map.Element (Crate);

            Config : Alire.Utils.Switches.Switches_Configuration
              := (case Profile is
                     when Release => Default_Release_Switches,
                     when Validation => Default_Validation_Switches,
                     when Development => Default_Development_Switches);

            Modif : Properties.Build_Switches.Profile_Modifier;
         begin

            --  Get switches modifier from the release
            for Prop of Rel.On_Platform_Properties
              (Root.Environment,
               Properties.Build_Switches.Variable'Tag)
            loop
               declare
                  Prof : constant Properties.Build_Switches.Variable
                    := Properties.Build_Switches.Variable (Prop);
               begin
                  Modif := Prof.Modifier;
               end;
            end loop;

            Properties.Build_Switches.Apply (Config, Modif.Wildcard);

            case Profile is
               when Release =>
                  Properties.Build_Switches.Apply (Config, Modif.Release);
               when Validation =>
                  Properties.Build_Switches.Apply (Config, Modif.Validation);
               when Development =>
                  Properties.Build_Switches.Apply (Config, Modif.Development);
            end case;

            This.Switches_Map.Insert (Rel.Name, Get_List (Config));
         end;
      end loop;
   end Make_Switches_Map;

   ----------
   -- Load --
   ----------

   procedure Load (This : in out Global_Config;
                   Root : in out Alire.Roots.Root)
   is
      Solution : constant Solutions.Solution := Root.Solution;

      Rel_Vect : constant Crate_Name_Set := Root.Nonabstract_Crates;
   begin

      if not Solution.Is_Complete then
         Warnings.Warn_Once ("Generating possibly incomplete configuration"
                             & " because of missing dependencies");
      end if;

      for Crate of Rel_Vect loop
         This.Load_Definitions (Root, Crate);
      end loop;

      Make_Build_Profile_Map (This, Root, Rel_Vect);

      Make_Switches_Map (This, Root, Rel_Vect);

      for Create of Rel_Vect loop
         This.Load_Settings (Root, Create);
      end loop;

      Use_Default_Values (This);

      Trace.Debug ("Build profiles loaded");
   end Load;

   ----------
   -- Name --
   ----------

   function Name (C : Config_Maps.Cursor) return Crate_Name is
   begin
      return To_Name (AAA.Strings.Head (+Config_Maps.Key (C), "."));
   end Name;

   ------------------------
   -- Is_Config_Complete --
   ------------------------
   --  Say if all variables in configuration are set, for all or one crate
   function Is_Config_Complete (This  : Global_Config;
                                Crate : String := "")
                                return Boolean
   is
      use Config_Maps;
   begin
      for C in This.Var_Map.Iterate loop
         if (Crate = "" or else Name (C).As_String = Crate)
           and then Element (C).Set_By = Must_Be_Set
         then
            return False;
         end if;
      end loop;
      return True;
   end Is_Config_Complete;

   ---------------------
   -- Ensure_Complete --
   ---------------------

   procedure Ensure_Complete (This : Global_Config) is
   begin
      --  The reporting of unset variables is done when the configuration is
      --  loaded, so no need to duplicate it here.

      if not This.Is_Config_Complete then
         Raise_Checked_Error
           ("Configuration variables without a default remain unset");
      end if;
   end Ensure_Complete;

   ---------------------------
   -- Generate_Config_Files --
   ---------------------------

   procedure Generate_Config_Files (This : Global_Config;
                                    Root : in out Alire.Roots.Root)
   is
      use Alire.Directories;
      use Alire.Origins;

      Solution : constant Solutions.Solution := Root.Solution;

      function Get_Config_Entry (Rel : Releases.Release) return Config_Entry is
      begin
         for Prop of Rel.On_Platform_Properties (Root.Environment,
                                                 Config_Entry'Tag)
         loop
            return Config_Entry (Prop);
         end loop;

         --  No Config_Entry found, return the default Config_Entry
         declare
            Ret : Config_Entry;
         begin
            return Ret;
         end;
      end Get_Config_Entry;
   begin

      if not Solution.Is_Complete then
         Warnings.Warn_Once ("Generating possibly incomplete configuration"
                             & " because of missing dependencies");
      end if;

      Trace.Detail ("Generating crate config files");

      This.Save_Last_Build_Profiles;

      for Crate of Root.Nonabstract_Crates loop
         declare
            Rel : constant Releases.Release := Root.Release (Crate);
         begin
            --  We don't create config files for external releases, since they
            --  are not sources built by Alire.
            if Rel.Origin.Kind /= Alire.Origins.External then

               --  Check completeness before generating anything

               if not This.Is_Config_Complete (Rel.Name_Str) then
                  Warnings.Warn_Once
                    ("Skipping generation of incomplete configuration files "
                     & "for crate " & Utils.TTY.Name (Rel.Name_Str));
               else
                  declare
                     Ent : constant Config_Entry := Get_Config_Entry (Rel);

                     Conf_Dir : constant Absolute_Path :=
                                 Root.Release_Base (Rel.Name) / Ent.Output_Dir;

                     Version_Str : constant String := Rel.Version.Image;
                  begin
                     if not Ent.Disabled then
                        Ada.Directories.Create_Path (Conf_Dir);

                        if Ent.Generate_Ada then
                           This.Generate_Ada_Config
                             (Rel.Name,
                              Conf_Dir / (+Rel.Name & "_config.ads"),
                              Version_Str);
                        end if;

                        if Ent.Generate_GPR then
                           This.Generate_GPR_Config
                             (Rel.Name,
                              Conf_Dir / (+Rel.Name & "_config.gpr"),
                              (if Ent.Auto_GPR_With
                               then Root.Direct_Withs (Rel)
                               else AAA.Strings.Empty_Set),
                              Version_Str);
                        end if;

                        if Ent.Generate_C then
                           This.Generate_C_Config
                             (Rel.Name,
                              Conf_Dir / (+Rel.Name & "_config.h"),
                              Version_Str);
                        end if;
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;
   end Generate_Config_Files;

   -------------------------
   -- Generate_Ada_Config --
   -------------------------

   procedure Generate_Ada_Config (This     : Global_Config;
                                  Crate    : Crate_Name;
                                  Filepath : Absolute_Path;
                                  Version  : String)
   is
      File : TIO.File_Type;

      Crate_Mixed : constant String := To_Mixed_Case (+Crate);
   begin

      TIO.Create (File, TIO.Out_File, Filepath);

      TIO.Put_Line
        (File, "--  Configuration for " & (+Crate) & " generated by Alire");

      TIO.Put_Line (File, "pragma Restrictions (No_Elaboration_Code);");
      TIO.Put_Line (File, "pragma Style_Checks (Off);");
      TIO.New_Line (File);
      TIO.Put_Line (File, "package " & Crate_Mixed & "_Config is");
      TIO.Put_Line (File, "   pragma Pure;");
      TIO.New_Line (File);

      TIO.Put_Line (File, "   Crate_Version : constant String := """ &
                      Version & """;");
      TIO.Put_Line (File, "   Crate_Name : constant String := """ &
                    (+Crate) & """;");

      for Elt of Host_Info loop
         TIO.New_Line (File);
         TIO.Put_Line (File,
                       Elt.Type_Def.Element.To_Ada_Declaration (Elt.Value));
      end loop;

      for C in This.Var_Map.Iterate loop
         declare
            Elt : constant Config_Maps.Constant_Reference_Type :=
              This.Var_Map.Constant_Reference (C);

            Type_Def : Config_Type_Definition renames Elt.Type_Def.Element;

            Key : constant String := To_String (Config_Maps.Key (C));
         begin
            if Elt.Set_By /= Must_Be_Set then
               if Elt.Value = TOML.No_TOML_Value then
                  Raise_Checked_Error
                    ("Configuration variable should be set at this point." &
                       " Missing call to Use_Default_Values()?");
               end if;

               if Name (C) = Crate then
                  TIO.New_Line (File);
                  TIO.Put_Line (File, Type_Def.To_Ada_Declaration (Elt.Value));
               end if;
            else
               Trace.Debug
                 ("Skipping Ada generation of unset variable " & Key);
            end if;
         end;
      end loop;
      TIO.New_Line (File);
      TIO.Put_Line (File, "end " & Crate_Mixed & "_Config;");
      TIO.Close (File);
   end Generate_Ada_Config;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Global_Config) return Boolean
   is (not This.Profile_Map.Is_Empty);
   --  Because at a minimum it must contain the root crate profile

   ---------------------
   -- Must_Regenerate --
   ---------------------

   function Must_Regenerate (This : Global_Config) return Boolean
   is
      use type Profile_Maps.Map;
   begin
      return This.Profile_Map /= Last_Build_Profiles;
   end Must_Regenerate;

   ---------------------------
   -- Pretty_Print_Switches --
   ---------------------------

   procedure Pretty_Print_Switches (File : TIO.File_Type;
                                    L      : Switch_List;
                                    Indent : Natural)
   is
      Indent_Str : constant String (1 .. Indent) := (others => ' ');
      First : Boolean := True;
   begin

      Alire.Utils.Switches.Knowledge.Populate;

      TIO.Put_Line (File, Indent_Str & "(");
      for Sw of L loop
         TIO.Put (File, Indent_Str & " ");
         if not First then
            TIO.Put (File, ",");
         else
            TIO.Put (File, " ");
            First := False;
         end if;
         TIO.Put (File, """" & Sw & """");

         declare
            Info : constant String := Utils.Switches.Knowledge.Get_Info (Sw);
         begin
            if Info'Length /= 0 then
               TIO.Put (File, " -- " & Info);
            end if;
         end;

         TIO.New_Line (File);
      end loop;

      TIO.Put_Line (File, Indent_Str & ");");
   end Pretty_Print_Switches;

   -------------------------
   -- Generate_GPR_Config --
   -------------------------

   procedure Generate_GPR_Config (This     : Global_Config;
                                  Crate    : Crate_Name;
                                  Filepath : Absolute_Path;
                                  Withs    : AAA.Strings.Set;
                                  Version  : String)
   is
      File : TIO.File_Type;

      Crate_Mixed : constant String := To_Mixed_Case (+Crate);
   begin

      TIO.Create (File, TIO.Out_File, Filepath);

      TIO.Put_Line
        (File, "--  Configuration for " & (+Crate) & " generated by Alire");

      for W of Withs loop
         TIO.Put_Line (File, "with """ & W & """;");
      end loop;

      TIO.Put_Line (File, "abstract project " & Crate_Mixed & "_Config is");

      TIO.Put_Line (File, "   Crate_Version := """ & Version & """;");
      TIO.Put_Line (File, "   Crate_Name := """ & (+Crate) & """;");

      for Elt of Host_Info loop
         TIO.New_Line (File);
         TIO.Put_Line (File,
                       Elt.Type_Def.Element.To_GPR_Declaration (Elt.Value));
      end loop;

      TIO.Put_Line (File,
         "   Ada_Compiler_Switches := External_As_List (""ADAFLAGS"", "" "");"
         );

      TIO.Put_Line (File,
         "   Ada_Compiler_Switches := Ada_Compiler_Switches &");

      Pretty_Print_Switches (File,
                             This.Switches_Map.Element (Crate),
                             Indent => 10);

      for C in This.Var_Map.Iterate loop
         declare
            Elt : constant Config_Maps.Constant_Reference_Type :=
              This.Var_Map.Constant_Reference (C);

            Type_Def : Config_Type_Definition renames Elt.Type_Def.Element;

            Key : constant String := To_String (Config_Maps.Key (C));
         begin
            if Elt.Set_By /= Must_Be_Set then
               if Elt.Value = TOML.No_TOML_Value then
                  Raise_Checked_Error
                    ("Configuration variable should be set at this point." &
                       " Missing call to Use_Default_Values()?");
               end if;

               if Name (C) = Crate then
                  TIO.New_Line (File);
                  TIO.Put_Line (File, Type_Def.To_GPR_Declaration (Elt.Value));
               end if;
            else
               Trace.Debug
                 ("Skipping GPR generation of unset variable " & Key);
            end if;
         end;
      end loop;

      TIO.New_Line (File);
      TIO.Put_Line (File, "end " & Crate_Mixed & "_Config;");
      TIO.Close (File);
   end Generate_GPR_Config;

   -----------------------
   -- Generate_C_Config --
   -----------------------

   procedure Generate_C_Config (This     : Global_Config;
                                Crate    : Crate_Name;
                                Filepath : Absolute_Path;
                                Version  : String)
   is
      File : TIO.File_Type;

      Crate_Upper : constant String := To_Upper_Case (+Crate);
   begin

      TIO.Create (File, TIO.Out_File, Filepath);

      TIO.Put_Line
        (File, "/* Configuration for " & (+Crate) & " generated by Alire */");
      TIO.Put_Line (File, "#ifndef " & Crate_Upper & "_CONFIG_H");
      TIO.Put_Line (File, "#define " & Crate_Upper & "_CONFIG_H");
      TIO.New_Line (File);

      TIO.Put_Line (File, "#define CRATE_VERSION """ & Version & """");
      TIO.Put_Line (File, "#define CRATE_NAME """ & (+Crate) & """");

      for Elt of Host_Info loop
         TIO.New_Line (File);
         TIO.Put_Line (File,
                       Elt.Type_Def.Element.To_C_Declaration (Elt.Value));
      end loop;

      for C in This.Var_Map.Iterate loop
         declare
            Elt : constant Config_Maps.Constant_Reference_Type :=
              This.Var_Map.Constant_Reference (C);

            Type_Def : Config_Type_Definition renames Elt.Type_Def.Element;

            Key : constant String := To_String (Config_Maps.Key (C));
         begin
            if Elt.Set_By /= Must_Be_Set then
               if Elt.Value = TOML.No_TOML_Value then
                  Raise_Checked_Error
                    ("Configuration variable should be set at this point." &
                       " Missing call to Use_Default_Values()?");
               end if;

               if Name (C) = Crate then
                  TIO.New_Line (File);
                  TIO.Put_Line (File, Type_Def.To_C_Declaration (Elt.Value));
               end if;
            else
               Trace.Debug
                 ("Skipping C generation of unset variable " & Key);
            end if;
         end;
      end loop;
      TIO.New_Line (File);
      TIO.Put_Line (File, "#endif");
      TIO.Close (File);
   end Generate_C_Config;

   --------------------
   -- Add_Definition --
   --------------------

   procedure Add_Definition (This     : in out Global_Config;
                             Crate    : Crate_Name;
                             Type_Def : Config_Type_Definition)
   is
      Type_Name_Lower : constant String := To_Lower_Case (Type_Def.Name);

      Name : constant Unbounded_String := +(+Crate & "." & Type_Name_Lower);
   begin

      if Is_Reserved_Name (Type_Name_Lower) then
         Raise_Checked_Error
           ("Configuration variable name '" & (+Name) &
              "' is reserved for Alire internal use");
      end if;

      if This.Var_Map.Contains (Name) then
         Raise_Checked_Error
           ("Configuration variable '" & (+Name) & "' already defined");
      end if;

      declare
         Setting : Config_Setting;
      begin
         Setting.Type_Def.Replace_Element (Type_Def);
         Setting.Value := TOML.No_TOML_Value;
         This.Var_Map.Insert (Name, Setting);
      end;
   end Add_Definition;

   ----------------------
   -- Load_Definitions --
   ----------------------

   procedure Load_Definitions (This  : in out Global_Config;
                               Root  : in out Roots.Root;
                               Crate : Crate_Name)
   is

      Rel : constant Releases.Release := Root.Release (Crate);

   begin

      This.Add_Definition (Crate, Builtin_Build_Profile);

      for Prop of Rel.On_Platform_Properties (Root.Environment,
                                              Config_Type_Definition'Tag)
      loop
         This.Add_Definition (Crate, Config_Type_Definition (Prop));
      end loop;
   end Load_Definitions;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (This  : in out Global_Config;
                        Crate : Crate_Name;
                        Val   : Assignment)
   is
      Val_Name_Lower : constant String := To_Lower_Case (+Val.Name);
      Crate_Str : constant String := +Crate;
      Name : constant Unbounded_String := (+Crate_Str) & "." & Val_Name_Lower;
   begin

      --  TODO check if setting configuration of a dependency

      if not This.Var_Map.Contains (Name) then
         Raise_Checked_Error
           ("Unknown configuration variable '" & (+Name) & "'");
      end if;

      declare
         Ref : constant Config_Maps.Reference_Type :=
           This.Var_Map.Reference (Name);
      begin

         if not Valid (Ref.Type_Def.Element, Val.Value) then
            Raise_Checked_Error
              ("Invalid value from '" & Crate_Str &
                 "'" & " for type " & Image (Ref.Type_Def.Element));
         end if;

         if Ref.Value /= No_TOML_Value and then Ref.Value /= Val.Value then
            Raise_Checked_Error
              ("Conflicting value for configuration variable '" &
               (+Name) & "' from '" & (+Ref.Set_By) & "' and '"
               & (+Crate) & "'.");
         else
            Ref.Value  := Val.Value;
            Ref.Set_By := +(+Crate);
         end if;
      end;
   end Set_Value;

   -------------------
   -- Load_Settings --
   -------------------

   procedure Load_Settings (This  : in out Global_Config;
                            Root  : in out Roots.Root;
                            Crate : Crate_Name)
   is

      Rel : constant Releases.Release := Root.Release (Crate);

   begin

      for Prop of Rel.On_Platform_Properties (Root.Environment,
                                              Config_Value_Assignment'Tag)
      loop
         declare
            List : Config_Value_Assignment renames
              Config_Value_Assignment (Prop);
         begin
            for Elt of List.List loop
               This.Set_Value (To_Name (+List.Crate), Elt);
            end loop;
         end;
      end loop;

   end Load_Settings;

   ------------------------
   -- Use_Default_Values --
   ------------------------

   procedure Use_Default_Values (Conf : in out Global_Config)
   is
      Config_Fault : Boolean := False;
   begin
      for C in Conf.Var_Map.Iterate loop
         declare
            Elt : constant Config_Maps.Reference_Type :=
              Conf.Var_Map.Reference (C);

            Key : constant String := To_String (Config_Maps.Key (C));
         begin
            if Elt.Value = TOML.No_TOML_Value then
               if Elt.Type_Def.Element.Default /= No_TOML_Value then
                  Elt.Value := Elt.Type_Def.Element.Default;
                  Elt.Set_By := +"default value";
               else
                  Config_Fault := True;
                  Elt.Set_By := Must_Be_Set;

                  Warnings.Warn_Once
                    ("Configuration variable '" & Key &
                       "' not set and has no default value.");
               end if;
            end if;
         end;
      end loop;
      if Config_Fault then
         Trace.Detail ("Configuration variables without values exist");
      end if;
   end Use_Default_Values;

   -------------------------
   -- Last_Build_Profiles --
   -------------------------

   function Last_Build_Profiles return Profile_Maps.Map is
      Str : constant String := Config.DB.Get ("last_build_profile", "");
      Profiles : Parsed_Profiles;
   begin
      Profiles := Parse_Profiles (Str, Accept_Wildcards => False);
      return Profiles.Profiles;
   exception
      when E : others =>
         --  This may happen at most once when first migrating from 1.2
         Log_Exception (E);
         Trace.Debug ("Unexpected format in profiles string: " & Str);
         return Profile_Maps.Empty_Map;
   end Last_Build_Profiles;

   --------------------
   -- Parse_Profiles --
   --------------------

   function Parse_Profiles (Img              : String;
                            Accept_Wildcards : Boolean) return Parsed_Profiles
   is

      ----------------
      -- To_Profile --
      ----------------

      function To_Profile (Img : String) return Profile_Kind is
         function Is_Valid is new AAA.Enum_Tools.Is_Valid (Profile_Kind);
      begin
         if Is_Valid (Img) then
            return Profile_Kind'Value (Img);
         else
            Raise_Checked_Error ("Invalid profile value: " & TTY.Error (Img)
                                 & " in profile list: " & Parse_Profiles.Img);
            raise Program_Error; -- Unreachable
         end if;
      end To_Profile;

      Pairs     : constant Vector := Split (Img, Profile_Split);

      Crates_Seen   : Crate_Name_Set;
      Wildcard_Seen : Natural := 0;
      Result        : Parsed_Profiles;
   begin
      if Img = "" then
         return Result;
      end if;

      for Pair of Pairs loop
         declare
            Crate   : constant String := Head (Pair, Profile_Assign);
            Profile : constant String := Tail (Pair, Profile_Assign);
         begin
            if Crate in "*" | "%" then
               if Accept_Wildcards then
                  Wildcard_Seen := Wildcard_Seen + 1;
                  if Wildcard_Seen > 1 then
                     Raise_Checked_Error
                       ("Only one of '*' or '%' allowed but profiles were: "
                        & Img);
                  end if;

                  Result.Default_Apply :=
                    (if Crate = "*" then To_All else To_Unset);
                  Result.Default_Profile := To_Profile (Profile);
               else
                  Raise_Checked_Error
                    ("Wildcards not allowed but profiles were: " & Img);
               end if;

            else
               if Crates_Seen.Contains (+Crate) then
                  Raise_Checked_Error
                    ("Duplicated crate " & Utils.TTY.Name (Crate)
                     & "in profile list: " & Img);
               else
                  Crates_Seen.Insert (+Crate);
                  Result.Profiles.Insert (+Crate, To_Profile (Profile));
               end if;
            end if;
         end;
      end loop;

      return Result;
   end Parse_Profiles;

   ------------------------------
   -- Save_Last_Build_Profiles --
   ------------------------------

   procedure Save_Last_Build_Profiles (This : Global_Config) is
      Profiles : Vector;
      use Profile_Maps;
   begin

      --  We store the profiles in the same format as they're given by users in
      --  the command line for no particular reason:
      --  last_build_profile=crate1=profile1,crate2=profile2,...

      for I in This.Profile_Map.Iterate loop
         Profiles.Append
           (String'(Key (I).As_String & Profile_Assign & Element (I)'Image));
      end loop;

      Config.Edit.Set_Locally ("last_build_profile",
                               Profiles.Flatten (Profile_Split));
   end Save_Last_Build_Profiles;

end Alire.Crate_Configuration;
