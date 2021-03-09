with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Tags;

with Alire.Conditional;
with Alire.Dependencies.Containers;
with Alire.Interfaces;
with Alire.Manifest;
with Alire.Milestones;
with Alire.Origins;
with Alire.Properties.Actions;
with Alire.Properties.Environment;
with Alire.Properties.Labeled;
with Alire.Properties.Licenses;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;
with Alire.Utils;

with Semantic_Versioning;

with TOML;

private with Alire.OS_Lib;
private with Alire.Utils.TTY;

package Alire.Releases is

--     subtype Dependency_Vector is Dependencies.Vectors.Vector;

   type Release (<>) is new Interfaces.Yamlable with private;

   function "<" (L, R : Release) return Boolean;

   function Default_Properties return Conditional.Properties;
   --  Returns the values in Defaults already wrapped as properties

   function New_Release (Name         : Crate_Name;
                         Version      : Semantic_Versioning.Version;
                         Origin       : Origins.Origin;
                         Notes        : Description_String;
                         Dependencies : Conditional.Dependencies;
                         Properties   : Conditional.Properties;
                         Available    : Conditional.Availability)
                         return Release;

   function New_Empty_Release (Name : Crate_Name) return Release;
   --  Equivalent to calling New_Working_Release with default values BUT with
   --  empty properties (i.e., defaults are not used).

   function New_Working_Release
     (Name         : Crate_Name;
      Origin       : Origins.Origin     := Origins.New_Filesystem (".");

      Dependencies : Conditional.Dependencies :=
        Conditional.For_Dependencies.Empty;

      Properties   : Conditional.Properties   :=
        Default_Properties
     )
      return         Release;
   --  For working releases that may have incomplete information. Note that the
   --  default properties are used by default.

   function Renaming (Base     : Release;
                      Provides : Crate_Name) return Release;
   --  Fills-in the "provides" field
   --  During resolution, a release that has a renaming will act as the
   --  "Provides" release, so both releases cannot be selected simultaneously.

   function Replacing (Base    : Release;
                       Notes   : Description_String := "")
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

   function Name (R : Release) return Crate_Name;

   function Name_Str (R : Release) return String is (+R.Name);

   function TTY_Name (R : Release) return String;

   function Description (R : Release) return Description_String;
   --  Returns the description for the crate, which is also stored as a
   --  property of the release.

   function TTY_Description (R : Release) return String;
   --  Colorized description

   function Long_Description (R : Release) return String;
   --  Returns the long description for the crate, which is also stored as a
   --  property of the release.

   function Provides (R : Release) return Crate_Name;
   --  The actual name to be used during dependency resolution (but nowhere
   --  else).

   function Forbidden (R : Release) return Conditional.Dependencies;
   --  Get all forbidden dependencies in platform-independen fashion

   function Forbidden (R : Release;
                       P : Alire.Properties.Vector)
                       return Conditional.Dependencies;
   --  Get platform-specific forbidden dependencies

   function Notes   (R : Release) return Description_String;
   --  Specific to release

   function Version (R : Release) return Semantic_Versioning.Version;

   function Dependencies (R : Release) return Conditional.Dependencies;
   --  Retrieve dependencies as-is from the TOML description

   function Dependencies (R : Release;
                          P : Alire.Properties.Vector)
                          return Conditional.Dependencies;
   --  Retrieve only the dependencies that apply on platform P

   function Flat_Dependencies
     (R : Release;
      P : Alire.Properties.Vector := Alire.Properties.No_Properties)
      return Alire.Dependencies.Containers.List;
   --  Remove any and/or nodes and return dependencies as a simple list. This
   --  is useful whenever you need to inspect all direct dependencies, no
   --  matter how they will be solved. If P is not empty, this function
   --  also works for platform-dependent dependencies only.

   function Property (R   : Release;
                      Key : Alire.Properties.Labeled.Labels)
                      return String;
   --  Return a property that must exist, be unique, platform-independent, and
   --  and atomic label: description, version... Raise otherwise.

   function Properties (R : Release) return Conditional.Properties;

   function Origin  (R : Release) return Origins.Origin;

   function Available (R : Release) return Conditional.Availability;
   function Is_Available (R : Release;
                          P : Alire.Properties.Vector) return Boolean;
   --  Evaluate R.Available under platform properties P

   function Default_Executable (R : Release) return String;
   --  We encapsulate here the fixing of platform extension

   package Env_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Alire.Properties.Environment.Variable,
      "<",    Alire.Properties.Environment."=");

   function Environment (R : Release;
                         P : Alire.Properties.Vector) return Env_Maps.Map;
   --  Retrieve env vars that are set by this release, key is the var name

   function Executables (R : Release;
                         P : Alire.Properties.Vector)
                         return Utils.String_Vector;
   --  Only explicitly declared ones
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

   type Moment_Array is array (Alire.Properties.Actions.Moments) of Boolean;
   --  Used to select which actions to retrieve

   function On_Platform_Actions (R : Release;
                                 P : Alire.Properties.Vector;
                                 Moments : Moment_Array := (others => True))
                                 return Alire.Properties.Vector;
   --  Get only Action properties for the platform that apply at specific
   --  moments.

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

   function Tag (R : Release) return Alire.Properties.Vector;

   function Config_Variables (R : Release) return Alire.Properties.Vector;
   function Config_Settings (R : Release) return Alire.Properties.Vector;

   function Auto_GPR_With (R : Release) return Boolean;

   procedure Print (R : Release);
   --  Dump info to console

   --  Search helpers

   function Property_Contains (R : Release; Str : String) return Boolean;
   --  True if some property contains the given string

   function Satisfies (R   : Release;
                       Dep : Alire.Dependencies.Dependency)
                       return Boolean;
   --  Ascertain if this release is a valid candidate for Dep

   function To_Dependency (R : Release) return Conditional.Dependencies;
   --  Return the dependency that represents this very release (crate=version),
   --  wrapped as a dependency tree with a single value.

   function From_Manifest (File_Name : Any_Path;
                           Source    : Manifest.Sources;
                           Strict    : Boolean)
                           return Release;

   function From_TOML (From   : TOML_Adapters.Key_Queue;
                       Source : Manifest.Sources;
                       Strict : Boolean;
                       File   : Any_Path := "")
                       return Release
     with Pre => Source not in Manifest.Local or else File /= "";
   --  Load a release from a TOML table. We require the manifest file for local
   --  manifests to be able to construct a local filesystem origin.

   function To_TOML (R      : Release;
                     Format : Manifest.Sources)
                     return TOML.TOML_Value;
   --  Convert the manifest to TOML. This is done currently only for a concrete
   --  platform, hence R.Whenever should have been already called.

   overriding
   function To_YAML (R : Release) return String;

   procedure To_File (R        : Release;
                      Filename : String;
                      Format   : Manifest.Sources);
   --  Directly write the release manifest to a file

   function Version_Image (R : Release) return String;

   function Has_Property (R : Release; Key : String) return Boolean;
   --  Say if the property exists in any branch of the property tree. Key is
   --  case-insensitive.

   function Check_Caret_Warning (This : Release) return Boolean;
   --  Check if this release contains a ^0.x dependency, and warn about it.
   --  Returns whether a warning was emitted.

   procedure Deploy
     (This            : Release;
      Env             : Alire.Properties.Vector;
      Parent_Folder   : String;
      Was_There       : out Boolean;
      Perform_Actions : Boolean := True);
   --  Deploy the sources of this release under the given Parent_Folder

private

   use Semantic_Versioning;

   function Materialize is new Conditional.For_Properties.Materialize
     (Alire.Properties.Vector, Alire.Properties.Append);

   function All_Properties (R : Release;
                            P : Alire.Properties.Vector)
                            return Alire.Properties.Vector;
   --  Properties that R has under platform properties P

   type Release (Prj_Len,
                 Notes_Len : Natural)
   is new Interfaces.Yamlable
   with record
      Name         : Crate_Name (Prj_Len);
      Alias        : UString; -- I finally gave up on constraints
      Version      : Semantic_Versioning.Version;
      Origin       : Origins.Origin;
      Notes        : Description_String (1 .. Notes_Len);
      Dependencies : Conditional.Dependencies;
      Forbidden    : Conditional.Dependencies;
      Properties   : Conditional.Properties;
      Available    : Conditional.Availability;
   end record;

   function From_TOML (This   : in out Release;
                       From   :        TOML_Adapters.Key_Queue;
                       Source :        Manifest.Sources;
                       Strict :        Boolean;
                       File   :        Any_Path := "")
                       return Outcome
     with Pre => Source not in Manifest.Local or else File /= "";
   --  Fill in an already existing release. We require the manifest file
   --  location for local releases to be able to construct a local file origin.

   use all type Conditional.Properties;

   function "<" (L, R : Release) return Boolean
   is (L.Name < R.Name
         or else
       (L.Name = R.Name and then L.Version < R.Version)
         or else
       (L.Name = R.Name
           and then
        L.Version = R.Version
           and then
        Build (L.Version) < Build (R.Version)
       )
      );

   function Name (R : Release) return Crate_Name
   is (R.Name);

   function TTY_Name (R : Release) return String
   is (Utils.TTY.Name (+R.Name));

   function Provides (R : Release) return Crate_Name
   is (if UStrings.Length (R.Alias) = 0
       then R.Name
       else +(+R.Alias));

   function Notes (R : Release) return Description_String
   is (R.Notes);

   function Dependencies (R : Release) return Conditional.Dependencies
   is (R.Dependencies);

   function Dependencies (R : Release;
                          P : Alire.Properties.Vector)
                          return Conditional.Dependencies
   is (R.Dependencies.Evaluate (P));

   function Forbidden (R : Release) return Conditional.Dependencies
   is (R.Forbidden);

   function Forbidden (R : Release;
                       P : Alire.Properties.Vector)
                       return Conditional.Dependencies
   is (R.Forbidden.Evaluate (P));

   function Properties (R : Release) return Conditional.Properties
   is (R.Properties);

   function Origin  (R : Release) return Origins.Origin
   is (R.Origin);

   function Available (R : Release) return Conditional.Availability
   is (R.Available);

   function Is_Available (R : Release;
                          P : Alire.Properties.Vector) return Boolean
   is (R.Available.Is_Available (P));

   function Description (R : Release) return Description_String
   --  Image returns "Description: Blah" so we have to cut.
   is (Utils.Tail
       (Conditional.Enumerate (R.Properties).Filter
        (Alire.TOML_Keys.Description).First_Element.Image, ' '));

   function TTY_Description (R : Release) return String
   is (Utils.TTY.Description (R.Description));

   function Milestone (R : Release) return Milestones.Milestone
   is (Milestones.New_Milestone (R.Name, R.Version));

   function Default_Executable (R : Release) return String
   is (Utils.Replace (+R.Name, ":", "_") & OS_Lib.Exe_Suffix);

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

   function Tag (R : Release) return Alire.Properties.Vector
   is (Conditional.Enumerate (R.Properties).Filter
       (Alire.TOML_Keys.Tag));

   function Config_Variables (R : Release) return Alire.Properties.Vector
   is (Conditional.Enumerate (R.Properties).Filter
       (Alire.TOML_Keys.Config_Vars));

   function Config_Settings (R : Release) return Alire.Properties.Vector
   is (Conditional.Enumerate (R.Properties).Filter
       (Alire.TOML_Keys.Config_Values));

   use all type Origins.Kinds;
   function Unique_Folder (R : Release) return Folder_String
   is (Utils.Head (+R.Name, Extension_Separator) & "_" &
         Utils.Head (Utils.Head (Image (R.Version), '-'), '+') & "_" &
         --  Remove patch/build strings that may violate folder valid chars
       (case R.Origin.Kind is
           when External       => "external",
           when Filesystem     => "filesystem",
           when System         => "system",
           when Source_Archive => R.Origin.Short_Unique_Id,
           when Git | Hg       => R.Origin.Short_Unique_Id,
           when SVN            => R.Origin.Commit));

   function Satisfies (R   : Release;
                       Dep : Alire.Dependencies.Dependency)
                       return Boolean
   is (R.Name = Dep.Crate and then Dep.Versions.Contains (R.Version));

   function Version_Image (R : Release) return String
   is (Semantic_Versioning.Image (R.Version));

end Alire.Releases;
