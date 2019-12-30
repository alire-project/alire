with AAA.Table_IO;
with Ada.Strings.Fixed;

--  with Alire.Platform;
with Alire.Defaults;
with Alire.Platforms;
with Alire.Requisites.Booleans;
with Alire.TOML_Load;
with Alire.Utils.YAML;

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

   --------------
   -- Renaming --
   --------------

   function Renaming (Base     : Release;
                      Provides : Crates.Named'Class) return Release is
      (Base.Renaming (Provides.Project));

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
      Project            : Alire.Crate_Name   := "";
      Notes              : Description_String := "")
      return Release
   is
      New_Project : constant Alire.Crate_Name := (if Project = ""
                                                  then Base.Project
                                                  else Project);
      New_Notes   : constant Description_String := (if Notes = ""
                                                    then Base.Notes
                                                    else Notes);
   begin

      return Replacement : constant Release
        (New_Project'Length, New_Notes'Length) :=
        (Prj_Len   => New_Project'Length,
         Notes_Len => New_Notes'Length,
         Project   => New_Project,
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

   function New_Release (Project            : Crate_Name;
                         Version            : Semantic_Versioning.Version;
                         Origin             : Origins.Origin;
                         Notes              : Description_String;
                         Dependencies       : Conditional.Dependencies;
                         Properties         : Conditional.Properties;
                         Private_Properties : Conditional.Properties;
                         Available          : Alire.Requisites.Tree)
                         return Release
   is (Prj_Len      => Project'Length,
       Notes_Len    => Notes'Length,
       Project      => Project,
       Alias        => +"",
       Version      => Version,
       Origin       => Origin,
       Notes        => Notes,
       Dependencies => Dependencies,
       Forbidden    => Conditional.For_Dependencies.Empty,
       Properties   => Properties,
       Available    => Available);

   -------------------------
   -- New_Working_Release --
   -------------------------

   function New_Working_Release
     (Project      : Crate_Name;
      Origin       : Origins.Origin := Origins.New_Filesystem ("..");
      Dependencies : Conditional.Dependencies :=
        Conditional.For_Dependencies.Empty;
      Properties   : Conditional.Properties   :=
        Conditional.For_Properties.Empty)
      return         Release is
     (Prj_Len      => Project'Length,
      Notes_Len    => 0,
      Project      => Project,
      Alias        => +"",
      Version      => +"0.0.0",
      Origin       => Origin,
      Notes        => "",
      Dependencies => Dependencies,
      Forbidden    => Conditional.For_Dependencies.Empty,
      Properties   => (if Properties = Conditional.For_Properties.Empty
                       then Default_Properties
                       else Properties),
      Available    => Requisites.Booleans.Always_True);

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
      if With_Paths.Is_Empty then
         With_Paths.Append (String'((+R.Project) & ".gpr"));
      end if;

      if With_Path then
         return With_Paths;
      else
         for File of With_Paths loop
            --  Has path or not
            if Tail (File, '/') = "" then
               Without.Append (File); -- As is
            else
               Without.Append (Tail (File, '/'));
            end if;
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
            end if;
         end loop;
      end return;
   end Project_Paths;

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
      Put_Line (R.Milestone.Image & ": " & R.Description);

      if R.Provides /= R.Project then
         Put_Line ("Provides: " & (+R.Provides));
      end if;

      if R.Notes /= "" then
         Put_Line ("Notes: " & R.Notes);
      end if;

      --  ORIGIN
      if R.Origin.Is_Native then
         Put_Line ("Origin (native package):");
         declare
            Table : AAA.Table_IO.Table;
         begin
            for Dist in Platforms.Distributions loop
               if R.Origin.Package_Name (Dist) /= Origins.Unavailable.Image
               then
                  Table.New_Row;
                  Table.Append ("   ");
                  Table.Append (Utils.To_Mixed_Case (Dist'Img) & ":");
                  Table.Append (R.Origin.Package_Name (Dist));
               end if;
            end loop;
            Table.Print;
         end;
      else
         Put_Line ("Origin: " & R.Origin.Image);
      end if;

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
         R.Dependencies.Print ("   ", R.Dependencies.Contains_ORs);
      end if;
   end Print;

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

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out Release;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is
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

      declare
         Result : constant Outcome :=
                    TOML_Load.Load_Crate_Section
                      (Crates.Release_Section,
                       From,
                       This.Properties,
                       This.Dependencies,
                       This.Available);
      begin
         if not Result.Success then
            return Result;
         end if;
      end;

      --  Check for remaining keys, which must be erroneous:
      return From.Report_Extra_Keys;
   end From_TOML;

   -------------------
   -- To_Dependency --
   -------------------

   function To_Dependency (R : Release) return Conditional.Dependencies is
     (Conditional.For_Dependencies.New_Value
        (Alire.Dependencies.New_Dependency
             (R.Project,
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
      Root    : constant TOML.TOML_Value := TOML.Create_Table;
      Relinfo :          TOML.TOML_Value := TOML.Create_Table;
   begin

      --  TODO: move generation of the [general] crate part to Alire.Crates.

      --  General properties
      declare
         General : constant TOML.TOML_Value := R.Properties.To_TOML;
      begin

         --  Alias/Provides
         if UStrings.Length (R.Alias) > 0 then
            General.Set (TOML_Keys.Provides, +(+R.Alias));
         end if;

         --  Notes
         if R.Notes'Length > 0 then
            General.Set (TOML_Keys.Notes, +R.Notes);
         end if;

         --  Ensure atoms are atoms and arrays are arrays
         for Label in APL.Cardinality'Range loop
            if General.Has (APL.Key (Label)) then
               case APL.Cardinality (Label) is
                  when Unique   =>
                     pragma Assert
                       (General.Get
                          (APL.Key (Label)).Kind in TOML.Atom_Value_Kind);
                  when Multiple =>
                     General.Set
                       (APL.Key (Label),
                        TOML_Adapters.To_Array
                          (General.Get (APL.Key (Label))));
               end case;
            end if;
         end loop;

         --  Final assignment, always have general section.
         Root.Set (TOML_Keys.General, General);
      end;

      --  Origin
      Relinfo := TOML.Merge (Relinfo, R.Origin.To_TOML);

      --  Dependencies
      if not R.Dependencies.Is_Empty then
         Relinfo.Set (TOML_Keys.Dependency, R.Dependencies.To_TOML);
      end if;

      --  Forbidden
      if not R.Forbidden.Is_Empty then
         Relinfo.Set (TOML_Keys.Forbidden, R.Forbidden.To_TOML);
      end if;

      --  Available
      if R.Available.Is_Empty or else
         R.Available = Alire.Requisites.Booleans.Always_True
      then
         null; -- Do nothing, do not pollute .toml file
      else
         Relinfo.Set (TOML_Keys.Available, R.Available.To_TOML);
      end if;

      --  Version release
      Root.Set (R.Version_Image, Relinfo);

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
        "crate: " & Utils.YAML.YAML_Stringify (R.Project_Str) & ASCII.LF &
        "authors: " & Props_To_YAML (R.Author) & ASCII.LF &
        "maintainers: " & Props_To_YAML (R.Maintainer) & ASCII.LF &
        "licenses: " & Props_To_YAML (R.License) & ASCII.LF &
        "websites: " & Props_To_YAML (R.Website) & ASCII.LF &
        "tags: " & Props_To_YAML (R.Tag) & ASCII.LF &
        "version: " & Utils.YAML.YAML_Stringify (R.Version_Image) & ASCII.LF &
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
   is
   begin
      return Solid : constant Release (R.Prj_Len, R.Notes_Len) :=
        (Prj_Len      => R.Prj_Len,
         Notes_Len    => R.Notes_Len,
         Project      => R.Project,
         Alias        => R.Alias,
         Version      => R.Version,
         Origin       => R.Origin,
         Notes        => R.Notes,
         Dependencies => R.Dependencies.Evaluate (P),
         Forbidden    => R.Forbidden.Evaluate (P),
         Properties   => R.Properties.Evaluate (P),
         Available    => (if R.Available.Check (P)
                          then Requisites.Booleans.Always_True
                          else Requisites.Booleans.Always_False))
      do
         null;
      end return;
   end Whenever;

end Alire.Releases;
