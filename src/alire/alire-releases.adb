with AAA.Table_IO;

--  with Alire.Platform;
with Alire.Defaults;
with Alire.Platforms;
with Alire.Requisites.Booleans;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

with GNAT.IO; -- To keep preelaborable

package body Alire.Releases is

   use all type Alire.Properties.Labeled.Labels;

   --------------------
   -- All_Properties --
   --------------------

   function All_Properties (R : Release;
                            P : Alire.Properties.Vector) return Alire.Properties.Vector is
      (Materialize (R.Properties and R.Priv_Props, P));

   ------------------------
   -- Default_Properties --
   ------------------------

   function Default_Properties return Conditional.Properties is
     (Conditional.For_Properties.New_Value
        (New_Label (Maintainer,
                    Defaults.Maintainer)));

   ---------------
   -- Extending --
   ---------------

   function Extending (Base               : Release;
                       Dependencies       : Conditional.Dependencies := Conditional.For_Dependencies.Empty;
                       Properties         : Conditional.Properties   := Conditional.For_Properties.Empty;
                       Private_Properties : Conditional.Properties   := Conditional.For_Properties.Empty;
                       Available          : Alire.Requisites.Tree    := Requisites.Trees.Empty_Tree)
                       return Release
   is
      use all type Conditional.Dependencies;
      use all type Requisites.Tree;
   begin
      return Extended : Release := Base do
         Extended.Dependencies := Base.Dependencies and Dependencies;
         Extended.Properties   := Base.Properties   and Properties;
         Extended.Priv_Props   := Base.Priv_Props   and Private_Properties;
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
                      Provides : Alire.Project) return Release is
   begin
      return Renamed : Release := Base do
         Renamed.Alias := +(+Provides);
      end return;
   end Renaming;

   --------------
   -- Renaming --
   --------------

   function Renaming (Base     : Release;
                      Provides : Projects.Named'Class) return Release is
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

   function Replacing (Base         : Release;
                       Dependencies : Conditional.Dependencies) return Release is
   begin
      return Replaced : Release := Base do
         Replaced.Dependencies := Dependencies;
      end return;
   end Replacing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing (Base               : Release;
                       Project            : Alire.Project      := "";
                       Notes              : Description_String := "") return Release
   is
      New_Project : constant Alire.Project := (if Project = ""
                                               then Base.Project
                                               else Project);
      New_Notes   : constant Description_String := (if Notes = ""
                                                    then Base.Notes
                                                    else Notes);
   begin

      return Replacement : constant Release (New_Project'Length, New_Notes'Length) :=
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
         Priv_Props   => Base.Priv_Props,
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

   function New_Release (Project            : Alire.Project;
                         Version            : Semantic_Versioning.Version;
                         Origin             : Origins.Origin;
                         Notes              : Description_String;
                         Dependencies       : Conditional.Dependencies;
                         Properties         : Conditional.Properties;
                         Private_Properties : Conditional.Properties;
                         Available          : Alire.Requisites.Tree) return Release is
     (Prj_Len      => Project'Length,
      Notes_Len    => Notes'Length,
      Project      => Project,
      Alias        => +"",
      Version      => Version,
      Origin       => Origin,
      Notes        => Notes,
      Dependencies => Dependencies,
      Forbidden    => Conditional.For_Dependencies.Empty,
      Properties   => Properties,
      Priv_Props   => Private_Properties,
      Available    => Available);

   -------------------------
   -- New_Working_Release --
   -------------------------

   function New_Working_Release
     (Project      : Alire.Project;
      Origin       : Origins.Origin := Origins.New_Filesystem (".");
      Dependencies : Conditional.Dependencies := Conditional.For_Dependencies.Empty;
      Properties   : Conditional.Properties   := Conditional.For_Properties.Empty)
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
      Priv_Props   => Conditional.For_Properties.Empty,
      Available    => Requisites.Booleans.Always_True);

   ----------------------------
   -- On_Platform_Properties --
   ----------------------------

   function On_Platform_Properties (R             : Release;
                                    P             : Alire.Properties.Vector;
                                    Descendant_Of : Ada.Tags.Tag := Ada.Tags.No_Tag)
                                    return Alire.Properties.Vector
   is
      use Ada.Tags;
   begin
      if Descendant_Of = No_Tag then
         return Materialize (R.Properties, P) and Materialize (R.Priv_Props, P);
      else
         declare
            Props : constant Alire.Properties.Vector := R.On_Platform_Properties (P);
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
   begin
      return Exes : Utils.String_Vector :=
        Props_To_Strings (R.All_Properties (P), Executable)
      do
         if OS_Lib.Exe_Suffix /= "" then
            for I in Exes.Iterate loop
               Exes (I) := Exes (I) & OS_Lib.Exe_Suffix;
            end loop;
         end if;
      end return;
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

      With_Paths : Utils.String_Vector := Props_To_Strings (R.All_Properties (P), Project_File);
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
      Files : constant String_Vector := Project_Files (R, P, With_Path => True);
   begin
      return Paths : String_Set do
         for File of Files loop
            if Contains (File, "/") then
               Paths.Include (Head (File, '/'));
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

   procedure Print (R : Release; Private_Too : Boolean := False) is
      use GNAT.IO;
   begin
      --  MILESTONE
      Put_Line (R.Milestone.Image & ": " & Projects.Descriptions (R.Project));

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
               if R.Origin.Package_Name (Dist) /= Origins.Unavailable.Image then
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

      --  PRIVATE PROPERTIES
      if Private_Too and then not R.Properties.Is_Empty then
         Put_Line ("Private properties:");
         R.Priv_Props.Print ("   ", False);
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
      for P of Enumerate (R.Properties and R.Priv_Props) loop
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

   -------------
   -- To_TOML --
   -------------

   function To_TOML (R : Release) return TOML.TOML_Value is
      use TOML_Adapters;
      Root    : constant TOML.TOML_Value := TOML.Create_Table;
      Relinfo :          TOML.TOML_Value := TOML.Create_Table;
   begin
      -- General properties
      declare
         General : constant TOML.TOML_Value := R.Properties.To_Toml;
      begin
         -- Description
         if Projects.Descriptions.Contains (R.Project) then
            General.Set (TOML_Keys.Description, +Projects.Descriptions (R.Project));
         else
            General.Set (TOML_Keys.Description, +Defaults.Description);
         end if;

         -- Alias/Provides
         if UStrings.Length (R.Alias) > 0 then
            General.Set (TOML_Keys.Provides, +(+R.Alias));
         end if;

         -- Notes
         if R.Notes'Length > 0 then
            General.Set (TOML_Keys.Notes, +R.Notes);
         end if;

         -- Final assignment
         if General.Is_Present then
            Root.Set (TOML_Keys.General, General);
         end if;
      end;

      Relinfo := TOML.Merge (Relinfo, R.Origin.To_TOML);

      Root.Set (R.Version_Image, Relinfo);

      -- TODO:
      --   Dependencies
      --   Forbidden
      --   Available

      return Root;
   end To_TOML;

   -------------
   -- Version --
   -------------

   function Version (R : Release) return Semantic_Versioning.Version is
     (R.Version);

   --------------
   -- Whenever --
   --------------

   function Whenever (R : Release; P : Alire.Properties.Vector) return Release is
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
         Priv_Props   => R.Priv_Props.Evaluate (P),
         Available    => (if R.Available.Check (P)
                          then Requisites.Booleans.Always_True
                          else Requisites.Booleans.Always_False))
      do
         null;
      end return;
   end Whenever;

end Alire.Releases;
