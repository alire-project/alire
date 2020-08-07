with Ada.Strings.Fixed;

with Alire.Crates;
with Alire.Defaults;
with Alire.Requisites.Booleans;
with Alire.TOML_Load;
with Alire.Utils.YAML;
with Alire.Properties.Bool;

with GNAT.IO; -- To keep preelaborable

with Semantic_Versioning.Basic;
with Semantic_Versioning.Extended;

package body Alire.Releases is

   package Semver renames Semantic_Versioning;

   use all type Alire.Properties.Labeled.Labels;

   --------------------
   -- All_Properties --
   --------------------

   function All_Properties (R : Release;
                            P : Alire.Properties.Vector)
                            return Alire.Properties.Vector
   is (Materialize (R.Properties, P));

   ------------------------
   -- Default_Properties --
   ------------------------

   function Default_Properties return Conditional.Properties
   is (Conditional.For_Properties.New_Value
       (New_Label (Description,
                   Defaults.Description)) and
       Conditional.For_Properties.New_Value
       (New_Label (Maintainer,
                   Defaults.Maintainer)) and
       Conditional.For_Properties.New_Value
       (New_Label (Maintainers_Logins,
                   Defaults.Maintainer_Login)));

   ---------------
   -- Extending --
   ---------------

   function Extending
     (Base         : Release;
      Dependencies : Conditional.Dependencies := Conditional.No_Dependencies;
      Properties   : Conditional.Properties   := Conditional.No_Properties;
      Available    : Alire.Requisites.Tree    := Requisites.No_Requisites)
      return Release
   is
      use all type Conditional.Dependencies;
      use all type Requisites.Tree;
   begin
      return Extended : Release := Base do
         Extended.Dependencies := Base.Dependencies and Dependencies;
         Extended.Properties   := Base.Properties   and Properties;
         Extended.Available    := Base.Available    and Available;
      end return;
   end Extending;

   ----------------
   -- Forbidding --
   ----------------

   function Forbidding (Base      : Release;
                        Forbidden : Conditional.Forbidden_Dependencies)
                        return Release is
   begin
      return Extended : Release := Base do
         Extended.Forbidden := Forbidden;
      end return;
   end Forbidding;

   --------------
   -- Renaming --
   --------------

   function Renaming (Base     : Release;
                      Provides : Crate_Name) return Release is
   begin
      return Renamed : Release := Base do
         Renamed.Alias := +(+Provides);
      end return;
   end Renaming;

   ---------------
   -- Replacing --
   ---------------

   function Replacing (Base   : Release;
                       Origin : Origins.Origin) return Release is
   begin
      return Replaced : Release := Base do
         Replaced.Origin := Origin;
      end return;
   end Replacing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing
     (Base         : Release;
      Dependencies : Conditional.Dependencies := Conditional.No_Dependencies)
      return Release is
   begin
      return Replaced : Release := Base do
         Replaced.Dependencies := Dependencies;
      end return;
   end Replacing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing
     (Base         : Release;
      Properties   : Conditional.Properties   := Conditional.No_Properties)
      return Release is
   begin
      return Replaced : Release := Base do
         Replaced.Properties := Properties;
      end return;
   end Replacing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing
     (Base         : Release;
      Available    : Alire.Requisites.Tree    := Requisites.No_Requisites)
      return Release is
   begin
      return Replaced : Release := Base do
         Replaced.Available := Available;
      end return;
   end Replacing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing
     (Base               : Release;
      Notes              : Description_String := "")
      return Release
   is
      New_Notes   : constant Description_String := (if Notes = ""
                                                    then Base.Notes
                                                    else Notes);
   begin

      return Replacement : constant Release
        (Base.Name.Length, New_Notes'Length) :=
        (Prj_Len   => Base.Name.Length,
         Notes_Len => New_Notes'Length,
         Name      => Base.Name,
         Notes     => New_Notes,

         Alias        => Base.Alias,
         Version      => Base.Version,
         Origin       => Base.Origin,
         Dependencies => Base.Dependencies,
         Forbidden    => Base.Forbidden,
         Properties   => Base.Properties,
         Available    => Base.Available)
      do
         null;
      end return;
   end Replacing;

   ---------------
   -- Retagging --
   ---------------

   function Retagging (Base    : Release;
                       Version : Semantic_Versioning.Version) return Release is
   begin
      return Upgraded : Release := Base do
         Upgraded.Version := Version;
      end return;
   end Retagging;

   ---------------
   -- Upgrading --
   ---------------

   function Upgrading (Base    : Release;
                       Version : Semantic_Versioning.Version;
                       Origin  : Origins.Origin) return Release is
   begin
      return Upgraded : Release := Base do
         Upgraded.Version := Version;
         Upgraded.Origin  := Origin;
      end return;
   end Upgrading;

   -----------------
   -- New_Release --
   -----------------

   function New_Release (Name         : Crate_Name;
                         Version      : Semantic_Versioning.Version;
                         Origin       : Origins.Origin;
                         Notes        : Description_String;
                         Dependencies : Conditional.Dependencies;
                         Properties   : Conditional.Properties;
                         Available    : Alire.Requisites.Tree)
                         return Release
   is (Prj_Len      => Name.Length,
       Notes_Len    => Notes'Length,
       Name         => Name,
       Alias        => +"",
       Version      => Version,
       Origin       => Origin,
       Notes        => Notes,
       Dependencies => Dependencies,
       Forbidden    => Conditional.For_Dependencies.Empty,
       Properties   => Properties,
       Available    => Available);

   -----------------------
   -- New_Empty_Release --
   -----------------------

   function New_Empty_Release (Name : Crate_Name) return Release
   is (New_Working_Release (Name         => Name,
                            Properties   => Conditional.No_Properties));

   -------------------------
   -- New_Working_Release --
   -------------------------

   function New_Working_Release
     (Name         : Crate_Name;
      Origin       : Origins.Origin           := Origins.New_Filesystem ("..");
      Dependencies : Conditional.Dependencies :=
        Conditional.For_Dependencies.Empty;
      Properties   : Conditional.Properties   :=
        Default_Properties)
      return         Release is
     (Prj_Len      => Name.Length,
      Notes_Len    => 0,
      Name         => Name,
      Alias        => +"",
      Version      => +"0.0.0",
      Origin       => Origin,
      Notes        => "",
      Dependencies => Dependencies,
      Forbidden    => Conditional.For_Dependencies.Empty,
      Properties   => Properties,
      Available    => Requisites.Trees.Empty_Tree -- empty evaluates to True
     );

   -------------------------
   -- On_Platform_Actions --
   -------------------------

   function On_Platform_Actions (R : Release;
                                 P : Alire.Properties.Vector;
                                 Moments : Moment_Array := (others => True))
                                 return Alire.Properties.Vector
   is
      use Alire.Properties.Actions;
   begin
      return Filtered : Alire.Properties.Vector do
         for Prop of R.On_Platform_Properties
           (P, Alire.Properties.Actions.Action'Tag)
         loop
            if Moments (Action'Class (Prop).Moment) then
               Filtered.Append (Prop);
            end if;
         end loop;
      end return;
   end On_Platform_Actions;

   ----------------------------
   -- On_Platform_Properties --
   ----------------------------

   function On_Platform_Properties
     (R             : Release;
      P             : Alire.Properties.Vector;
      Descendant_Of : Ada.Tags.Tag := Ada.Tags.No_Tag)
      return Alire.Properties.Vector
   is
      use Ada.Tags;
   begin
      if Descendant_Of = No_Tag then
         return Materialize (R.Properties, P);
      else
         declare
            Props : constant Alire.Properties.Vector :=
              R.On_Platform_Properties (P);
         begin
            return Result : Alire.Properties.Vector do
               for P of Props loop
                  if Is_Descendant_At_Same_Level (P'Tag, Descendant_Of) then
                     Result.Append (P);
                  end if;
               end loop;
            end return;
         end;
      end if;
   end On_Platform_Properties;

   ----------------------
   -- Props_To_Strings --
   ----------------------

   function Props_To_Strings (Props : Alire.Properties.Vector;
                    Label : Alire.Properties.Labeled.Labels)
                    return Utils.String_Vector is
      --  Extract values of a particular label
      Filtered : constant Alire.Properties.Vector :=
                   Alire.Properties.Labeled.Filter (Props, Label);
   begin
      return Strs : Utils.String_Vector do
         for P of Filtered loop
            Strs.Append (Alire.Properties.Labeled.Label (P).Value);
         end loop;
      end return;
   end Props_To_Strings;

   -----------------
   -- Environment --
   -----------------

   function Environment (R : Release;
                         P : Alire.Properties.Vector) return Env_Maps.Map
   is
      package Env renames Alire.Properties.Environment;
   begin
      return Map : Env_Maps.Map do
         for Prop of R.On_Platform_Properties (P, Env.Variable'Tag) loop
            Map.Insert (Env.Variable (Prop).Name,
                        Env.Variable (Prop));
         end loop;
      end return;
   end Environment;

   -----------------
   -- Executables --
   ----------------

   function Executables (R : Release;
                         P : Alire.Properties.Vector)
                         return Utils.String_Vector
   is
      Exes : constant Utils.String_Vector :=
        Props_To_Strings (R.All_Properties (P), Executable);
   begin
      if OS_Lib.Exe_Suffix /= "" then
         declare
            With_Suffix : Utils.String_Vector;
         begin
            for I in Exes.Iterate loop
               With_Suffix.Append (Exes (I) & OS_Lib.Exe_Suffix);
            end loop;
            return With_Suffix;
         end;
      end if;
      return Exes;
   end Executables;

   -------------------
   -- Project_Files --
   -------------------

   function Project_Files (R         : Release;
                           P         : Alire.Properties.Vector;
                           With_Path : Boolean)
                           return Utils.String_Vector
   is
      use Utils;

      With_Paths : Utils.String_Vector :=
        Props_To_Strings (R.All_Properties (P), Project_File);
      Without    : Utils.String_Vector;
   begin
      if With_Paths.Is_Empty
        and then
         R.Origin.Kind not in Origins.External | Origins.System
      then
         --  Default project file if no one is specified by the crate. Only if
         --  the create is not external nor system.
         With_Paths.Append (String'((+R.Name) & ".gpr"));
      end if;

      if With_Path then
         return With_Paths;
      else
         for File of With_Paths loop

            --  Basename
            Without.Append (Split (Text => File,
                                   Separator => '/',
                                   Side      => Tail,
                                   From      => Tail,
                                   Raises    => False));

         end loop;

         return Without;
      end if;
   end Project_Files;

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (R         : Release;
                           P         : Alire.Properties.Vector)
                           return      Utils.String_Set
   is
      use Utils;
      use Ada.Strings;
      Files : constant String_Vector :=
        Project_Files (R, P, With_Path => True);
   begin
      return Paths : String_Set do
         for File of Files loop
            if Contains (File, "/") then
               Paths.Include
                 (File (File'First .. Fixed.Index (File, "/", Backward)));
            else

               --  The project file is at the root of the release
               Paths.Include ("");
            end if;
         end loop;
      end return;
   end Project_Paths;

   -------------------
   -- Auto_GPR_With --
   -------------------

   function Auto_GPR_With (R : Release) return Boolean is
      Vect : constant Alire.Properties.Vector :=
        Conditional.Enumerate (R.Properties).Filter
        (Alire.TOML_Keys.Auto_GPR_With);
   begin
      if not Vect.Is_Empty then
         return Alire.Properties.Bool.Property (Vect.First_Element).Value;
      else
         --  The default is to enable auto-gpr-with
         return True;
      end if;
   end Auto_GPR_With;

   ------------------------
   -- Labeled_Properties --
   ------------------------

   function Labeled_Properties_Vector (R     : Release;
                                       P     : Alire.Properties.Vector;
                                       Label : Alire.Properties.Labeled.Labels)
                                       return Alire.Properties.Vector is
   begin
      return Alire.Properties.Labeled.Filter (R.All_Properties (P), Label);
   end Labeled_Properties_Vector;

   ------------------------
   -- Labeled_Properties --
   ------------------------

   function Labeled_Properties (R     : Release;
                                P     : Alire.Properties.Vector;
                                Label : Alire.Properties.Labeled.Labels)
                                return Utils.String_Vector
   is
   begin
      return Props_To_Strings (R.All_Properties (P), Label);
   end Labeled_Properties;

   -----------
   -- Print --
   -----------

   procedure Print (R : Release) is
      use GNAT.IO;
   begin
      --  MILESTONE
      Put_Line (R.Milestone.TTY_Image & ": " & R.TTY_Description);

      if R.Provides /= R.Name then
         Put_Line ("Provides: " & (+R.Provides));
      end if;

      if R.Notes /= "" then
         Put_Line ("Notes: " & R.Notes);
      end if;

      --  ORIGIN
      Put_Line ("Origin: " & R.Origin.Image);

      --  AVAILABILITY
      if not R.Available.Is_Empty then
         Put_Line ("Available when: " & R.Available.Image);
      end if;

      --  PROPERTIES
      if not R.Properties.Is_Empty then
         Put_Line ("Properties:");
         R.Properties.Print ("   ", False);
      end if;

      --  DEPENDENCIES
      if not R.Dependencies.Is_Empty then
         Put_Line ("Dependencies (direct):");
         R.Dependencies.Print ("   ",
                               Sorted => True,
                               And_Or => R.Dependencies.Contains_ORs);
      end if;
   end Print;

   --------------
   -- Property --
   --------------

   function Property (R   : Release;
                      Key : Alire.Properties.Labeled.Labels)
                      return String
   is
      use Alire.Properties.Labeled;
      Target : constant Alire.Properties.Vector :=
                 Filter (R.Properties, Key);
   begin
      if Target.Length not in 1 then
         raise Constraint_Error with
           "Unexpected property count:" & Target.Length'Img;
      end if;

      return Label (Target.First_Element).Value;
   end Property;

   -----------------------
   -- Property_Contains --
   -----------------------

   function Property_Contains (R : Release; Str : String) return Boolean is
      use Utils;

      Search : constant String := To_Lower_Case (Str);
   begin
      for P of Conditional.Enumerate (R.Properties) loop
         declare
            Text : constant String :=
                     To_Lower_Case
                       ((if Utils.Contains (P.Image, ":")
                        then Utils.Tail (P.Image, ':')
                        else P.Image));
         begin
            if Utils.Contains (Text, Search) then
               return True;
            end if;
         end;
      end loop;

      return False;
   end Property_Contains;

   -------------------
   -- From_Manifest --
   -------------------

   function From_Manifest (File_Name : Any_Path) return Release
   is
     (From_TOML
        (TOML_Adapters.From
             (TOML_Load.Load_File (File_Name),
              "Loading release from manifest: " & File_Name)));

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return Release is
   begin
      From.Assert_Key (TOML_Keys.Name,        TOML.TOML_String);

      return This : Release := New_Empty_Release
        (Name       => +From.Unwrap.Get (TOML_Keys.Name).As_String)
      do
         Assert (This.From_TOML (From));
      end return;
   end From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (This : in out Release;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is
      package Labeled renames Alire.Properties.Labeled;
   begin
      Trace.Debug ("Loading release " & This.Milestone.Image);

      --  Origin

      declare
         Result : constant Outcome := This.Origin.From_TOML (From);
      begin
         if not Result.Success then
            return Result;
         end if;
      end;

      --  Properties

      TOML_Load.Load_Crate_Section
        (Crates.Release_Section,
         From,
         This.Properties,
         This.Dependencies,
         This.Available);

      --  Consolidate/validate some properties as fields:

      Assert (This.Name_Str = This.Property (Labeled.Name),
              "Mismatched name property and given name at release creation");

      This.Version := Semver.New_Version (This.Property (Labeled.Version));

      --  Check for remaining keys, which must be erroneous:
      return From.Report_Extra_Keys;
   end From_TOML;

   -------------------
   -- To_Dependency --
   -------------------

   function To_Dependency (R : Release) return Conditional.Dependencies is
     (Conditional.For_Dependencies.New_Value
        (Alire.Dependencies.New_Dependency
             (R.Name,
              Semver.Extended.To_Extended
                (Semver.Basic.Exactly (R.Version)))));

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (R : Release) return TOML.TOML_Value is
      package APL renames Alire.Properties.Labeled;
      use all type Alire.Properties.Labeled.Cardinalities;
      use all type Alire.Requisites.Tree;
      use TOML_Adapters;
      Root : constant TOML.TOML_Value := R.Properties.To_TOML;
   begin
      --  Name
      Root.Set (TOML_Keys.Name, +R.Name_Str);

      --  Version
      Root.Set (TOML_Keys.Version, +Semver.Image (R.Version));

      --  Alias/Provides
      if UStrings.Length (R.Alias) > 0 then
         Root.Set (TOML_Keys.Provides, +(+R.Alias));
      end if;

      --  Notes
      if R.Notes'Length > 0 then
         Root.Set (TOML_Keys.Notes, +R.Notes);
      end if;

      --  Ensure atoms are atoms and arrays are arrays
      for Label in APL.Cardinality'Range loop
         if Root.Has (APL.Key (Label)) then
            case APL.Cardinality (Label) is
               when Unique   =>
                  pragma Assert
                    (Root.Get
                       (APL.Key (Label)).Kind in TOML.Atom_Value_Kind);
               when Multiple =>
                  Root.Set
                    (APL.Key (Label),
                     TOML_Adapters.To_Array
                       (Root.Get (APL.Key (Label))));
            end case;
         end if;
      end loop;

      --  Origin
      Root.Set (TOML_Keys.Origin, R.Origin.To_TOML);

      --  Dependencies, wrapped as an array
      if not R.Dependencies.Is_Empty then
         declare
            Dep_Array : constant TOML.TOML_Value := TOML.Create_Array;
         begin
            Dep_Array.Append (R.Dependencies.To_TOML);
            Root.Set (TOML_Keys.Depends_On, Dep_Array);
         end;
      end if;

      --  Forbidden
      if not R.Forbidden.Is_Empty then
         Root.Set (TOML_Keys.Forbidden, R.Forbidden.To_TOML);
      end if;

      --  Available
      if R.Available.Is_Empty or else
         R.Available = Alire.Requisites.Booleans.Always_True
      then
         null; -- Do nothing, do not pollute .toml file
      else
         Root.Set (TOML_Keys.Available, R.Available.To_TOML);
      end if;

      return Root;
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (R : Release) return String is

      function Props_To_YAML
      is new Utils.YAML.To_YAML (Alire.Properties.Property'Class,
                                 Alire.Properties.Vectors,
                                 Alire.Properties.Vector);

   begin
      return
        "crate: " & Utils.YAML.YAML_Stringify (R.Name_Str) & ASCII.LF &
        "authors: " & Props_To_YAML (R.Author) & ASCII.LF &
        "maintainers: " & Props_To_YAML (R.Maintainer) & ASCII.LF &
        "licenses: " & Props_To_YAML (R.License) & ASCII.LF &
        "websites: " & Props_To_YAML (R.Website) & ASCII.LF &
        "tags: " & Props_To_YAML (R.Tag) & ASCII.LF &
        "version: " & Utils.YAML.YAML_Stringify (R.Version_Image) & ASCII.LF &
        "short_description: " & Utils.YAML.YAML_Stringify (R.Description) &
        ASCII.LF &
        "dependencies: " & R.Dependencies.To_YAML;
   end To_YAML;

   -------------
   -- Version --
   -------------

   function Version (R : Release) return Semantic_Versioning.Version is
     (R.Version);

   --------------
   -- Whenever --
   --------------

   function Whenever (R : Release;
                      P : Alire.Properties.Vector)
                      return Release
   is (Prj_Len      => R.Prj_Len,
       Notes_Len    => R.Notes_Len,
       Name         => R.Name,
       Alias        => R.Alias,
       Version      => R.Version,
       Origin       => R.Origin,
       Notes        => R.Notes,
       Dependencies => R.Dependencies.Evaluate (P),
       Forbidden    => R.Forbidden.Evaluate (P),
       Properties   => R.Properties.Evaluate (P),
       Available    => (if R.Available.Check (P)
                        then Requisites.Booleans.Always_True
                        else Requisites.Booleans.Always_False));

   ----------------------
   -- Long_Description --
   ----------------------

   function Long_Description (R : Release) return String is
      Descr : constant Alire.Properties.Vector :=
        Conditional.Enumerate (R.Properties).Filter
        (Alire.TOML_Keys.Long_Descr);
   begin
      if not Descr.Is_Empty then
         --  Image returns "Description: Blah" so we have to cut.
         return Utils.Tail (Descr.First_Element.Image, ' ');
      else
         return "";
      end if;
   end Long_Description;

end Alire.Releases;
