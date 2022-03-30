with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;

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

with TOML; use TOML;

package body Alire.Crate_Configuration is

   use Config_Type_Definition_Holder;

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
   Host_Info : constant array (Integer range <>) of Config_Setting :=
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

   package Crate_Name_Vect
   is new Ada.Containers.Indefinite_Vectors (Natural, Crate_Name);

   -----------------------
   -- Make_Release_Vect --
   -----------------------

   function Make_Release_Vect (Root : in out Alire.Roots.Root)
                               return Crate_Name_Vect.Vector
   is
      Result : Crate_Name_Vect.Vector;

      procedure Filter (This : in out Alire.Roots.Root;
                      Solution : Solutions.Solution;
                      State : Solutions.Dependency_State)
      is
         pragma Unreferenced (This, Solution);
      begin
         if State.Has_Release and then not State.Is_Provided then
            Result.Append (State.Crate);
         end if;
      end Filter;

   begin
      Root.Traverse (Filter'Access);
      return Result;
   end Make_Release_Vect;

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
                                     Rel_Vect : Crate_Name_Vect.Vector)
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
                                   then Root_Build_Profile
                                   else Default_Deps_Build_Profile));
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
               end loop;
            end;
         end;
      end loop;

      for Cursor in This.Profile_Map.Iterate loop
         --  Set build_Mode value in configuration variables
         This.Set_Value
           (Profile_Maps.Key (Cursor),
            (Name => +Builtin_Build_Profile.Name,
             Value => TOML.Create_String (Profile_Maps.Element (Cursor)'Img)));
      end loop;

   end Make_Build_Profile_Map;

   -----------------------
   -- Make_Switches_Map --
   -----------------------

   procedure Make_Switches_Map (This     : in out Global_Config;
                                Root     : in out Alire.Roots.Root;
                                Rel_Vect : Crate_Name_Vect.Vector)
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

      Rel_Vect : constant Crate_Name_Vect.Vector := Make_Release_Vect (Root);
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
   end Load;

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

      Set_Last_Build_Profile (Root_Build_Profile);

      for Crate of Make_Release_Vect (Root) loop
         declare
            Rel : constant Releases.Release := Root.Release (Crate);
         begin
            --  We don't create config files for external releases, since they
            --  are not sources built by Alire.
            if Rel.Origin.Kind /= Alire.Origins.External then

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

   begin

      TIO.Create (File, TIO.Out_File, Filepath);

      TIO.Put_Line
        (File, "--  Configuration for " & (+Crate) & " generated by Alire");

      TIO.Put_Line (File, "package " & (+Crate) & "_Config is");
      TIO.Put_Line (File, "   pragma Pure;");
      TIO.Put_Line (File, "   pragma No_Elaboration_Code_All;");

      TIO.Put_Line (File, "   Crate_Version : constant String := """ &
                      Version & """;");
      TIO.Put_Line (File, "   Crate_Name : constant String := """ &
                    (+Crate) & """;");

      for Elt of Host_Info loop
         TIO.New_Line (File);
         TIO.Put_Line (File,
                       Elt.Type_Def.Element.To_Ada_Declaration (Elt.Value));
      end loop;

      for C in This.Map.Iterate loop
         declare
            Elt : constant Config_Maps.Constant_Reference_Type :=
              This.Map.Constant_Reference (C);

            Type_Def : Config_Type_Definition renames Elt.Type_Def.Element;

            Key : constant String := To_String (Config_Maps.Key (C));
         begin
            if Elt.Value = TOML.No_TOML_Value then
               Raise_Checked_Error
                 ("Configuration variable should be set at this point." &
                    " Missing call to Use_Default_Values()?");
            end if;

            if AAA.Strings.Has_Prefix (Key, +Crate & ".") then
               TIO.New_Line (File);
               TIO.Put_Line (File, Type_Def.To_Ada_Declaration (Elt.Value));
            end if;
         end;
      end loop;
      TIO.New_Line (File);
      TIO.Put_Line (File, "end " & (+Crate) & "_Config;");
      TIO.Close (File);
   end Generate_Ada_Config;

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

   begin

      TIO.Create (File, TIO.Out_File, Filepath);

      TIO.Put_Line
        (File, "--  Configuration for " & (+Crate) & " generated by Alire");

      for W of Withs loop
         TIO.Put_Line (File, "with """ & W & """;");
      end loop;

      TIO.Put_Line (File, "abstract project " & (+Crate) & "_Config is");

      TIO.Put_Line (File, "   Crate_Version := """ & Version & """;");
      TIO.Put_Line (File, "   Crate_Name := """ & (+Crate) & """;");

      for Elt of Host_Info loop
         TIO.New_Line (File);
         TIO.Put_Line (File,
                       Elt.Type_Def.Element.To_GPR_Declaration (Elt.Value));
      end loop;

      TIO.Put_Line (File, "   Ada_Compiler_Switches := " &
                      "External_As_List (""ADAFLAGS"", "" "") &");

      Pretty_Print_Switches (File,
                             This.Switches_Map.Element (Crate),
                             Indent => 10);

      for C in This.Map.Iterate loop
         declare
            Elt : constant Config_Maps.Constant_Reference_Type :=
              This.Map.Constant_Reference (C);

            Type_Def : Config_Type_Definition renames Elt.Type_Def.Element;

            Key : constant String := To_String (Config_Maps.Key (C));
         begin
            if Elt.Value = TOML.No_TOML_Value then
               Raise_Checked_Error
                 ("Configuration variable should be set at this point." &
                    " Missing call to Use_Default_Values()?");
            end if;

            if AAA.Strings.Has_Prefix (Key, +Crate & ".") then
               TIO.New_Line (File);
               TIO.Put_Line (File, Type_Def.To_GPR_Declaration (Elt.Value));
            end if;
         end;
      end loop;

      TIO.New_Line (File);
      TIO.Put_Line (File, "end " & (+Crate) & "_Config;");
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

      Crate_Upper : constant String :=
        Ada.Characters.Handling.To_Upper (+Crate);

   begin

      TIO.Create (File, TIO.Out_File, Filepath);

      TIO.Put_Line
        (File, "/* Configuration for " & (+Crate) & " generated by Alire */");
      TIO.Put_Line (File, "#ifndef " & Crate_Upper & "_CONFIG_H");
      TIO.Put_Line (File, "#define " & Crate_Upper & "_CONFIG_H");

      TIO.Put_Line (File, "#define CRATE_VERSION """ & Version & """");
      TIO.Put_Line (File, "#define CRATE_NAME """ & (+Crate) & """");

      for Elt of Host_Info loop
         TIO.New_Line (File);
         TIO.Put_Line (File,
                       Elt.Type_Def.Element.To_C_Declaration (Elt.Value));
      end loop;

      for C in This.Map.Iterate loop
         declare
            Elt : constant Config_Maps.Constant_Reference_Type :=
              This.Map.Constant_Reference (C);

            Type_Def : Config_Type_Definition renames Elt.Type_Def.Element;

            Key : constant String := To_String (Config_Maps.Key (C));
         begin
            if Elt.Value = TOML.No_TOML_Value then
               Raise_Checked_Error
                 ("Configuration variable should be set at this point." &
                    " Missing call to Use_Default_Values()?");
            end if;

            if AAA.Strings.Has_Prefix (Key, +Crate & ".") then
               TIO.New_Line (File);
               TIO.Put_Line (File, Type_Def.To_C_Declaration (Elt.Value));
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
      Type_Name_Lower : constant String :=
        Ada.Characters.Handling.To_Lower (Type_Def.Name);

      Name : constant Unbounded_String := +(+Crate & "." & Type_Name_Lower);
   begin

      if Is_Reserved_Name (Type_Name_Lower) then
         Raise_Checked_Error
           ("Configuration variable name '" & (+Name) &
              "' is reserved for Alire internal use");
      end if;

      if This.Map.Contains (Name) then
         Raise_Checked_Error
           ("Configuration variable '" & (+Name) & "' already defined");
      end if;

      declare
         Setting : Config_Setting;
      begin
         Setting.Type_Def.Replace_Element (Type_Def);
         Setting.Value := TOML.No_TOML_Value;
         This.Map.Insert (Name, Setting);
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
      --  Add built-in definition
      This.Add_Definition (Crate,
                           Properties.Configurations.Builtin_Build_Profile);

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
      Val_Name_Lower : constant String :=
        Ada.Characters.Handling.To_Lower (+Val.Name);
      Crate_Str : constant String := +Crate;
      Name : constant Unbounded_String := (+Crate_Str) & "." & Val_Name_Lower;
   begin

      --  TODO check if setting configuration of a dependency

      if not This.Map.Contains (Name) then
         Raise_Checked_Error
           ("Unknown configuration variable '" & (+Name) & "'");
      end if;

      declare
         Ref : constant Config_Maps.Reference_Type :=
           This.Map.Reference (Name);
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
            Ref.Value := Val.Value;
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

   procedure Use_Default_Values (Conf : in out Global_Config) is
   begin
      for C in Conf.Map.Iterate loop
         declare
            Elt : constant Config_Maps.Reference_Type :=
              Conf.Map.Reference (C);

            Key : constant String := To_String (Config_Maps.Key (C));
         begin
            if Elt.Value = TOML.No_TOML_Value then
               if Elt.Type_Def.Element.Default /= No_TOML_Value then
                  Elt.Value := Elt.Type_Def.Element.Default;
                  Elt.Set_By := +"default value";
               else
                  Raise_Checked_Error
                    ("Configuration variable '" & Key &
                       " not set and has no default value.");
               end if;
            end if;
         end;
      end loop;
   end Use_Default_Values;

   ------------------------
   -- Last_Build_Profile --
   ------------------------

   function Last_Build_Profile return Utils.Switches.Profile_Kind is
      Str : constant String := Config.DB.Get ("last_build_profile",
                                             Default_Root_Build_Profile'Img);
   begin
      return Profile_Kind'Value (Str);
   exception
      when Constraint_Error =>
         return Default_Root_Build_Profile;
   end Last_Build_Profile;

   ----------------------------
   -- Set_Last_Build_Profile --
   ----------------------------

   procedure Set_Last_Build_Profile (P : Utils.Switches.Profile_Kind) is
   begin
      Config.Edit.Set_Locally ("last_build_profile", P'Img);
   end Set_Last_Build_Profile;

end Alire.Crate_Configuration;
