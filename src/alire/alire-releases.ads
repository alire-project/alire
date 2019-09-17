with Ada.Tags;

with Alire.Actions;
with Alire.Conditional;
with Alire.Dependencies;
with Alire.Interfaces;
with Alire.Milestones;
with Alire.Origins;
with Alire.Projects;
with Alire.Properties;
with Alire.Properties.Labeled;
with Alire.Properties.Licenses;
with Alire.Requisites;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;
with Alire.Utils;
with Alire.Versions;

with Semantic_Versioning;

with TOML;

private with Alire.OS_Lib;

package Alire.Releases with Preelaborate is

--     subtype Dependency_Vector is Dependencies.Vectors.Vector;

   type Release (<>) is
     new Versions.Versioned
     and Interfaces.Tomifiable
     and Interfaces.Detomifiable
     and Interfaces.Yamlable
   with private;

   function "<" (L, R : Release) return Boolean;

   function New_Release (Project            : Alire.Project;
                         Version            : Semantic_Versioning.Version;
                         Origin             : Origins.Origin;
                         Notes              : Description_String;
                         Dependencies       : Conditional.Dependencies;
                         Properties         : Conditional.Properties;
                         Private_Properties : Conditional.Properties;
                         Available          : Alire.Requisites.Tree)
                         return Release;

   function New_Working_Release
     (Project      : Alire.Project;
      Origin       : Origins.Origin := Origins.New_Filesystem ("..");

      Dependencies : Conditional.Dependencies :=
        Conditional.For_Dependencies.Empty;

      Properties   : Conditional.Properties   :=
        Conditional.For_Properties.Empty
     )

      return         Release;
   --  For working project releases that may have incomplete information

   function Extending
     (Base         : Release;
      Dependencies : Conditional.Dependencies := Conditional.No_Dependencies;
      Properties   : Conditional.Properties   := Conditional.No_Properties;
      Available    : Alire.Requisites.Tree    := Requisites.No_Requisites)
      return Release;
   --  Takes a release and merges given fields

   function Renaming (Base     : Release;
                      Provides : Alire.Project) return Release;

   function Renaming (Base     : Release;
                      Provides : Projects.Named'Class) return Release;
   --  Fills-in the "provides" field
   --  During resolution, a project that has a renaming will act as the
   --  "Provides" project, so both projects cannot be selected simultaneously.

   function Replacing (Base               : Release;
                       Project            : Alire.Project      := "";
                       Notes              : Description_String := "")
                       return Release;
   --  Takes a release and replaces the given fields

   function Replacing
     (Base         : Release;
      Dependencies : Conditional.Dependencies := Conditional.No_Dependencies)
      return Release;

   function Replacing
     (Base         : Release;
      Properties   : Conditional.Properties   := Conditional.No_Properties)
      return Release;

   function Replacing
     (Base         : Release;
      Available    : Alire.Requisites.Tree    := Requisites.No_Requisites)
      return Release;

   function Replacing (Base   : Release;
                       Origin : Origins.Origin) return Release;

   function Retagging (Base    : Release;
                       Version : Semantic_Versioning.Version) return Release;
   --  Keep all data but version

   function Upgrading (Base    : Release;
                       Version : Semantic_Versioning.Version;
                       Origin  : Origins.Origin) return Release;
   --  Takes a release and replaces version and origin

   function Forbidding (Base      : Release;
                        Forbidden : Conditional.Forbidden_Dependencies)
                        return Release;
   --  Add forbidden dependencies to a release

   function Whenever (R : Release; P : Properties.Vector) return Release;
   --  Materialize conditions in a Release once the whatever properties are
   --  known. At present dependencies, properties, and availability.

   overriding function Project (R : Release) return Alire.Project;

   function Project_Str (R : Release) return String is (+R.Project);

   function Project_Base (R : Release) return String;
   --  Project up to first dot, if any; which is needed for extension projects
   --  in templates and so on.

   function Provides (R : Release) return Alire.Project;
   --  The actual project name to be used during dependency resolution
   --  (But nowhere else)

   function Forbids (R : Release;
                     P : Alire.Properties.Vector)
     return Conditional.Dependencies;

   function Is_Extension (R : Release) return Boolean;

   function Notes   (R : Release) return Description_String;
   --  Specific to release
   overriding
   function Version (R : Release) return Semantic_Versioning.Version;

   function Depends (R : Release) return Conditional.Dependencies;
   function Dependencies (R : Release) return Conditional.Dependencies
                          renames Depends;

   function Properties (R : Release) return Conditional.Properties;

   function Depends (R : Release;
                     P : Alire.Properties.Vector)
                     return Conditional.Dependencies;
   --  Not really conditional anymore, but still a potential tree
   function Dependencies (R : Release;
                          P : Alire.Properties.Vector)
                          return Conditional.Dependencies renames Depends;

   function Origin  (R : Release) return Origins.Origin;
   function Available (R : Release) return Requisites.Tree;

   function Default_Executable (R : Release) return String;
   --  We encapsulate here the fixing of platform extension

   function Executables (R : Release;
                         P : Alire.Properties.Vector)
                         return Utils.String_Vector;
   --  Only explicity declared ones
   --  Under some conditions (usually current platform)

   function Project_Paths (R : Release;
                           P : Alire.Properties.Vector)
                           return Utils.String_Set;
   --  Deduced from Project_Files

   function Project_Files (R         : Release;
                           P         : Alire.Properties.Vector;
                           With_Path : Boolean)
                           return Utils.String_Vector;
   --  with relative path on demand

   function Unique_Folder (R : Release) return Folder_String;

   --  NOTE: property retrieval functions do not distinguish between
   --  public/private, since that's merely informative for the users.

   function On_Platform_Actions (R : Release;
                                 P : Alire.Properties.Vector)
                                 return Alire.Properties.Vector;
   --  Get only Action properties for the platform

   function On_Platform_Properties
     (R             : Release;
      P             : Alire.Properties.Vector;
      Descendant_Of : Ada.Tags.Tag := Ada.Tags.No_Tag)
      return Alire.Properties.Vector;
   --  Return properties that apply to R under platform properties P

   function Labeled_Properties_Vector
     (R     : Release;
      P     : Alire.Properties.Vector;
      Label : Alire.Properties.Labeled.Labels)
      return Alire.Properties.Vector;
   --  Return properties of Labeled class for a particular label

   function Labeled_Properties
     (R     : Release;
      P     : Alire.Properties.Vector;
      Label : Alire.Properties.Labeled.Labels)
      return Utils.String_Vector;
   --  Get all values for a given property for a given platform properties

   function Author (R : Release) return Alire.Properties.Vector;

   function License (R : Release) return Alire.Properties.Vector;

   function Maintainer (R : Release) return Alire.Properties.Vector;

   function Milestone (R : Release) return Milestones.Milestone;

   function Website (R : Release) return Alire.Properties.Vector with
     Post => Natural (Website'Result.Length) <= 1;
   --  Website is optional and unique in the index spec.

   procedure Print (R : Release);
   --  Dump info to console

   --  Search helpers

   function Property_Contains (R : Release; Str : String) return Boolean;
   --  True if some property contains the given string

   function Satisfies (R   : Release;
                       Dep : Alire.Dependencies.Dependency)
                       return Boolean;
   --  Ascertain if this release is a valid candidate for Dep

   overriding
   function From_TOML (This : in out Release;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome;
   --  Fill in the release-specific parts. This expects the common information
   --  from [general] to be already present in the release, since From points
   --  to the release proper.

   overriding
   function To_TOML (R : Release) return TOML.TOML_Value;

   overriding
   function To_YAML (R : Release) return String;

   function Version_Image (R : Release) return String;

private

   use Semantic_Versioning;

   function Materialize is new Conditional.For_Properties.Materialize
     (Alire.Properties.Vector, Alire.Properties.Append);

   function All_Properties (R : Release;
                            P : Alire.Properties.Vector)
                            return Alire.Properties.Vector;
   --  Properties that R has un der platform properties P

   use Alire.Properties;
   function Comment
   is new Alire.Properties.Labeled.Cond_New_Label
     (Alire.Properties.Labeled.Comment);

   type Release (Prj_Len,
                 Notes_Len : Natural) is
     new Versions.Versioned
     and Interfaces.Tomifiable
     and Interfaces.Detomifiable
     and Interfaces.Yamlable
   with record
      Project      : Alire.Project (1 .. Prj_Len);
      Alias        : UString; -- I finally gave up on constraints
      Version      : Semantic_Versioning.Version;
      Origin       : Origins.Origin;
      Notes        : Description_String (1 .. Notes_Len);
      Dependencies : Conditional.Dependencies;
      Forbidden    : Conditional.Dependencies;
      Properties   : Conditional.Properties;
      Available    : Requisites.Tree;
   end record;

   use all type Conditional.Properties;

   function "<" (L, R : Release) return Boolean
   is (L.Project < R.Project
         or else
       (L.Project = R.Project and then L.Version < R.Version)
         or else
       (L.Project = R.Project
           and then
        L.Version = R.Version
           and then
        Build (L.Version) < Build (R.Version)
       )
      );

   function Is_Extension (R : Release) return Boolean
   is (R.Project_Base'Length < R.Project'Length);

   overriding
   function Project (R : Release) return Alire.Project
   is (R.Project);

   function Project_Base (R : Release) return String
   is (Utils.Head (+R.Project, Extension_Separator));

   function Provides (R : Release) return Alire.Project
   is (if UStrings.Length (R.Alias) = 0
       then R.Project
       else +(+R.Alias));

   function Notes (R : Release) return Description_String
   is (R.Notes);

   function Depends (R : Release) return Conditional.Dependencies
   is (R.Dependencies);

   function Depends (R : Release;
                     P : Alire.Properties.Vector)
                     return Conditional.Dependencies
   is (R.Dependencies.Evaluate (P));

   function Forbids (R : Release;
                     P : Alire.Properties.Vector)
                     return Conditional.Dependencies
   is (R.Forbidden.Evaluate (P));

   function Properties (R : Release) return Conditional.Properties
   is (R.Properties);

   function Origin  (R : Release) return Origins.Origin
   is (R.Origin);

   function Available (R : Release) return Requisites.Tree
   is (R.Available);

   function Milestone (R : Release) return Milestones.Milestone
   is (Milestones.New_Milestone (R.Project, R.Version));

   function Default_Executable (R : Release) return String
   is (Utils.Replace (+R.Project, ":", "_") & OS_Lib.Exe_Suffix);

   function License (R : Release) return Alire.Properties.Vector
   is (Conditional.Enumerate (R.Properties).Filter
       (Alire.Properties.Licenses.License'Tag));

   function Author (R : Release) return Alire.Properties.Vector
   is (Conditional.Enumerate (R.Properties).Filter
       (Alire.TOML_Keys.Author));

   function Maintainer (R : Release) return Alire.Properties.Vector
   is (Conditional.Enumerate (R.Properties).Filter
       (Alire.TOML_Keys.Maintainer));

   function Website (R : Release) return Alire.Properties.Vector
   is (Conditional.Enumerate (R.Properties).Filter
       (Alire.TOML_Keys.Website));

   use all type Origins.Kinds;
   function Unique_Folder (R : Release) return Folder_String
   is (Utils.Head (+R.Project, Extension_Separator) & "_" &
         Utils.Head (Utils.Head (Image (R.Version), '-'), '+') & "_" &
         --  Remove patch/build strings that may violate folder valid chars
       (case R.Origin.Kind is
           when Filesystem     => "filesystem",
           when Native         => "native",
           when Source_Archive => "archive",
           when Git | Hg       => (if R.Origin.Commit'Length <= 8
                                   then R.Origin.Commit
                                   else R.Origin.Commit
                                     (R.Origin.Commit'First ..
                                        R.Origin.Commit'First + 7)),
           when SVN            => R.Origin.Commit));

   function On_Platform_Actions (R : Release;
                                 P : Alire.Properties.Vector)
                                 return Alire.Properties.Vector
   is (R.On_Platform_Properties (P, Actions.Action'Tag));

   function Satisfies (R   : Release;
                       Dep : Alire.Dependencies.Dependency)
                       return Boolean
   is (R.Project = Dep.Project and then Satisfies (R.Version, Dep.Versions));

   function Version_Image (R : Release) return String
   is (Semantic_Versioning.Image (R.Version));

end Alire.Releases;
