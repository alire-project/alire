with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Tags;

with AAA.Strings;

with Alire.Conditional;
with Alire.Containers;
with Alire.Dependencies.Containers;
with Alire.GPR;
with Alire.Interfaces;
with Alire.Manifest;
with Alire.Milestones;
with Alire.Origins;
with Alire.Platforms.Current;
with Alire.Properties.Actions;
with Alire.Properties.Environment;
with Alire.Properties.Labeled;
with Alire.Properties.Licenses;
with Alire.Provides;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;
with Alire.User_Pins.Maps;
with Alire.Utils;

with Semantic_Versioning;

with TOML;

private with Alire.OS_Lib;
private with CLIC.TTY;
private with Alire.Utils.TTY;

package Alire.Releases is

   type Release (<>) is new Interfaces.Yamlable with private;

   function "<" (L, R : Release) return Boolean;
   --  Sorts by name, version, and build within same version

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

   function Providing (Base    : Release;
                       Targets : Containers.Crate_Name_Sets.Set)
                       return Release;
   --  Add an equivalence to Target=Base.Version for all Target of Targets
   --  (which may be empty).

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

   function Forbidden (R : Release) return Conditional.Dependencies;
   --  Get all forbidden dependencies in platform-independent fashion

   function Forbidden (R : Release;
                       P : Alire.Properties.Vector)
                       return Conditional.Forbidden_Dependencies;
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

   function Dependency_On (R     : Release;
                           Crate : Crate_Name;
                           P     : Alire.Properties.Vector :=
                             Alire.Properties.No_Properties)
                           return Alire.Dependencies.Containers.Optional;
   --  If R.Flat_Dependencies contains Crate, that dependency will be returned,
   --  Empty otherwise.

   function Depends_On (R : Release;
                        Crate : Crate_Name;
                           P     : Alire.Properties.Vector :=
                          Alire.Properties.No_Properties)
                        return Boolean
   is (R.Dependency_On (Crate, P).Has_Element);

   function Flat_Dependencies
     (R : Release;
      P : Alire.Properties.Vector := Alire.Properties.No_Properties)
      return Alire.Dependencies.Containers.List;
   --  Remove any and/or nodes and return dependencies as a simple list. This
   --  is useful whenever you need to inspect all direct dependencies, no
   --  matter how they will be solved. If P is not empty, this function
   --  also works for platform-dependent dependencies only.

   function Provides (R : Release) return Provides.Equivalences;

   function Provides (R : Release; Target : Crate_Name) return Boolean;
   --  Say if one of this release Provides milestones is for Target, in
   --  addition to R.Name = Target.

   function Provides (R : Release; Target : Release) return Boolean;
   --  Check whether R and Target have the same name or provide the same name

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
                         P : Alire.Properties.Vector :=
                           Platforms.Current.Properties)
                         return AAA.Strings.Vector;
   --  Only explicitly declared ones
   --  Under some conditions (usually current platform)

   type Externals_Info is record
      Declared : GPR.Name_Vector; -- The crate uses these vars
      Modified : GPR.Name_Vector; -- The crate modifies these vars
   end record;

   function GPR_Externals (R : Release;
                                     P : Alire.Properties.Vector :=
                                       Platforms.Current.Properties)
                                     return Externals_Info;
   --  Returns a list of all variables that can influence the build via
   --  GPR externals or environment variables (the `gpr-externals` and
   --  gpr-set-externals tables in the manifest).

   function Pins (R : Release) return User_Pins.Maps.Map;

   function Project_Paths (R : Release;
                           P : Alire.Properties.Vector)
                           return AAA.Strings.Set;
   --  Deduced from Project_Files

   function Project_Files (R         : Release;
                           P         : Alire.Properties.Vector;
                           With_Path : Boolean)
                           return AAA.Strings.Vector;
   --  With relative path on demand. Will always return at least the default
   --  project file when nothing is declared in the manifest for regular
   --  crates, but nothing for system/binary/external.

   function Deployment_Folder (R : Release) return Folder_String;
   --  The folder under which the release origin will be deployed

   function Base_Folder (R : Release) return Relative_Path;
   --  Deployment_Folder / (if R in monorepo, rel_path to it, else "")

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
      return AAA.Strings.Vector;
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

   function Property_Contains (R : Release; Str : String)
                               return AAA.Strings.Set;
   --  Return a set with the names of properties that contain the given string

   function Satisfies (R   : Release;
                       Dep : Alire.Dependencies.Dependency'Class)
                       return Boolean;
   --  Ascertain if this release is a valid candidate for Dep

   function To_Dependency (R : Release) return Conditional.Dependencies;
   --  Return the dependency that represents this very release (crate=version),
   --  wrapped as a dependency tree with a single value.

   function From_Manifest (File_Name : Any_Path;
                           Source    : Manifest.Sources;
                           Strict    : Boolean;
                           Root_Path : Any_Path := "")
                           return Release
     with Pre => Source in Manifest.Index or else Root_Path in Absolute_Path;
   --  When loading a manifest for a workspace, it may contain pins that we
   --  must resolve relative to Root_Path.

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
      Create_Manifest : Boolean := False;
      Include_Origin  : Boolean := False;
      Mark_Completion : Boolean := True);
   --  Deploy the sources of this release under the given Parent_Folder. If
   --  Create_Manifest, any packaged manifest will be moved out of the way
   --  and an authoritative manifest will be generated from index information.
   --  The created manifest may optionally Include_Origin information. When
   --  Mark_Completion, a trace file will be created in ./alire/copy_complete
   --  so future inspections of the folder can ensure the operation wasn't
   --  interrupted. No actions for the release are run at this time.

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
      Version      : Semantic_Versioning.Version;
      Origin       : Origins.Origin;
      Notes        : Description_String (1 .. Notes_Len);
      Equivalences : Alire.Provides.Equivalences;
      Dependencies : Conditional.Dependencies;
      Pins         : User_Pins.Maps.Map;
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

   function Sort_Compilers (L, R : Release) return Boolean;
   --  For the special case of crates providing a compiler, we prefer the
   --  native compilers before the cross-compilers.

   function Standard_Sorting (L, R : Release) return Boolean
   is (R.Name < L.Name -- So when going from newest to oldest the order is OK
         or else
       (L.Name = R.Name and then L.Version < R.Version)
         or else
       (L.Name = R.Name
           and then
        L.Version = R.Version
           and then
        Build (L.Version) < Build (R.Version)));

   function Name (R : Release) return Crate_Name
   is (R.Name);

   function TTY_Name (R : Release) return String
   is (Utils.TTY.Name (+R.Name));

   function Notes (R : Release) return Description_String
   is (R.Notes);

   function Dependencies (R : Release) return Conditional.Dependencies
   is (R.Dependencies);

   function Dependencies (R : Release;
                          P : Alire.Properties.Vector)
                          return Conditional.Dependencies
   is (R.Dependencies.Evaluate (P));

   function Provides (R : Release) return Alire.Provides.Equivalences
   is (R.Equivalences);

   function Provides (R : Release; Target : Crate_Name) return Boolean
   is (R.Name = Target
       or else
         (for some Mil of R.Equivalences => Mil.Crate = Target));

   function Provides (R : Release; Target : Release) return Boolean
   is (R.Provides (Target.Name)
       or else
       Target.Provides (R.Name)
       or else
         (for some Mil_1 of R.Equivalences =>
             Mil_1.Crate = Target.Name
             or else
            (for some Mil_2 of Target.Equivalences =>
                Mil_2.Crate = R.Name
                or else
                Mil_1.Crate = Mil_2.Crate)));

   function Forbidden (R : Release) return Conditional.Dependencies
   is (R.Forbidden);

   function Forbidden (R : Release;
                       P : Alire.Properties.Vector)
                       return Conditional.Forbidden_Dependencies
   is (R.Forbidden.Evaluate (P));

   function Properties (R : Release) return Conditional.Properties
   is (R.Properties);

   function Origin  (R : Release) return Origins.Origin
   is (R.Origin);

   function Available (R : Release) return Conditional.Availability
   is (R.Available);

   function Is_Available (R : Release;
                          P : Alire.Properties.Vector) return Boolean
   is (R.Available.Is_Available (P)
       and then R.Origin.Is_Available (P));

   function Description (R : Release) return Description_String
   --  Image returns "Description: Blah" so we have to cut.
   is (AAA.Strings.Tail
       (Conditional.Enumerate (R.Properties).Filter
        (Alire.TOML_Keys.Description).First_Element.Image, ' '));

   function TTY_Description (R : Release) return String
   is (CLIC.TTY.Description (R.Description));

   function Milestone (R : Release) return Milestones.Milestone
   is (Milestones.New_Milestone (R.Name, R.Version));

   function Default_Executable (R : Release) return String
   is (AAA.Strings.Replace (+R.Name, ":", "_") & OS_Lib.Exe_Suffix);

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

   function Satisfies (R   : Release;
                       Dep : Alire.Dependencies.Dependency'Class)
                       return Boolean
   is ((R.Name = Dep.Crate and then Dep.Versions.Contains (R.Version))
       or else
       R.Equivalences.Satisfies (Dep));

   function Version_Image (R : Release) return String
   is (Semantic_Versioning.Image (R.Version));

   function Pins (R : Release) return User_Pins.Maps.Map
   is (R.Pins);

end Alire.Releases;
