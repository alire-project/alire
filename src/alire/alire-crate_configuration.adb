with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Characters.Handling;

with Alire_Early_Elaboration;
with Alire.Solutions;
with Alire.Releases;
with Alire.Roots;
with Alire.Origins;
with Alire.Warnings;

with Alire.Properties.Build_Profile;
with Alire.Utils.Switches; use Alire.Utils.Switches;
with Alire.Utils.Switches.Knowledge;
with Alire.Directories;

with TOML; use TOML;

package body Alire.Crate_Configuration is

   package TIO renames Ada.Text_IO;

   function Is_Reserved_Name (Type_Name : String) return Boolean
   is (Type_Name = "crate_version");
   --  Return True if Type_Name is reserved for Alire internal use

   ----------------------------
   -- Make_Build_Profile_Map --
   ----------------------------

   procedure Make_Build_Profile_Map (This    : in out Global_Config;
                                     Root     : Alire.Roots.Root;
                                     Solution : Solutions.Solution)
   is
      use Properties.Build_Profile;

      -----------------
      -- Set_Profile --
      -----------------

      procedure Set_Profile (Crate : Crate_Name; P : Profile_Kind) is
      begin
         if This.Profile_Map.Contains (Crate) then
            This.Profile_Map.Replace (Crate, P);
         else
            Raise_Checked_Error ("Unknow crate in build profile: '" &
                                   String'(+Crate) & "'");
         end if;
      end Set_Profile;

   begin

      --  Populate map with crates in the solution
      for Rel of Solution.Releases.Including (Root.Release) loop
         This.Profile_Map.Insert (Rel.Name,
                                  (if Rel.Name = Root.Name
                                   then Development
                                   else Release));
      end loop;

      for Prop of Root.Release.On_Platform_Properties
        (Root.Environment,
         Properties.Build_Profile.Variable'Tag)
      loop
         declare
            Prof : constant Properties.Build_Profile.Variable
              := Properties.Build_Profile.Variable (Prop);
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
               use Properties.Build_Profile.Profile_Selection_Maps;
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

   ----------------------
   -- Make_Swiches_Map --
   ----------------------

   procedure Make_Swiches_Map (This     : in out Global_Config;
                               Root     : Alire.Roots.Root;
                               Solution : Solutions.Solution)
   is
   begin
      for Rel of Solution.Releases.Including (Root.Release) loop
         declare
            Profile : constant Profile_Kind
              := This.Profile_Map.Element (Rel.Name);

            List : Alire.Utils.Switches.Switch_List;
         begin
            case Profile is
               when Release =>
                  List := Get_List (Default_Release_Switches);
               when Validation =>
                  List := Get_List (Default_Validation_Switches);
               when Development =>
                  List := Get_List (Default_Development_Switches);
            end case;

            This.Switches_Map.Insert (Rel.Name, List);
         end;
      end loop;
   end Make_Swiches_Map;

   ----------
   -- Load --
   ----------

   procedure Load (This : in out Global_Config;
                   Root : in out Alire.Roots.Root)
   is
      Solution : constant Solutions.Solution := Root.Solution;

   begin

      if not Solution.Is_Complete then
         Warnings.Warn_Once ("Generating possibly incomplete configuration"
                             & " because of missing dependencies");
      end if;

      for Rel of Solution.Releases.Including (Root.Release) loop
         This.Load_Definitions (Root  => Root,
                                Crate => Rel.Name);
      end loop;

      Make_Build_Profile_Map (This, Root, Solution);

      Make_Swiches_Map (This, Root, Solution);

      for Rel of Solution.Releases.Including (Root.Release) loop
         This.Load_Settings (Root  => Root,
                             Crate => Rel.Name);
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

      for Rel of Solution.Releases.Including (Root.Release) loop

         --  We don't create config files for external releases, since they are
         --  not sources built by Alire.
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

      TIO.Put_Line (File, "   Crate_Version : constant String := """ &
                      Version & """;");

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

      Trace.Always ("Add_Defintion: " & (+Name));

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

end Alire.Crate_Configuration;
