with Ada.Tags;

with Alire.Actions;
with Alire.Conditional;
with Alire.Dependencies;
with Alire.Interfaces;
with Alire.Milestones;
with Alire.Origins;
with Alire.Crates;
with Alire.Properties;
with Alire.Properties.Labeled;
with Alire.Properties.Licenses;
with Alire.Requisites;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;
with Alire.Utils;

with Semantic_Versioning;

with TOML;

private with Alire.OS_Lib;

package Alire.Releases with Preelaborate is

--     subtype Dependency_Vector is Dependencies.Vectors.Vector;

   type Release (<>) is
     new Interfaces.Tomifiable
     and Interfaces.Detomifiable
     and Interfaces.Yamlable
   with private;

   function "<" (L, R : Release) return Boolean;

   function New_Release (Name         : Crate_Name;
                         Version      : Semantic_Versioning.Version;
                         Origin       : Origins.Origin;
                         Notes        : Description_String;
                         Dependencies : Conditional.Dependencies;
                         Properties   : Conditional.Properties;
                         Available    : Alire.Requisites.Tree)
                         return Release;

   function New_Working_Release
     (Name         : Crate_Name;
      Origin       : Origins.Origin := Origins.New_Filesystem ("..");

      Dependencies : Conditional.Dependencies :=
        Conditional.For_Dependencies.Empty;

      Properties   : Conditional.Properties   :=
        Conditional.For_Properties.Empty
     )

      return         Release;
   --  For working releases that may have incomplete information

   function Extending
     (Base         : Release;
      Dependencies : Conditional.Dependencies := Conditional.No_Dependencies;
      Properties   : Conditional.Properties   := Conditional.No_Properties;
      Available    : Alire.Requisites.Tree    := Requisites.No_Requisites)
      return Release;
   --  Takes a release and merges given fields

   function Renaming (Base     : Release;
                      Provides : Crate_Name) return Release;

   function Renaming (Base     : Release;
                      Provides : Crates.Named'Class) return Release;
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

   function Name (R : Release) return Crate_Name;

   function Name_Str (R : Release) return String is (+R.Name);

   function Description (R : Release) return Description_String;
   --  Returns the description for the crate, which is also stored as a
   --  property of the release.

   function Long_Description (R : Release) return String;
   --  Returns the long description for the crate, which is also stored as a
   --  property of the release.

   function Provides (R : Release) return Crate_Name;
   --  The actual name to be used during dependency resolution (but nowhere
   --  else).

   function Forbids (R : Release;
                     P : Alire.Properties.Vector)
     return Conditional.Dependencies;

   function Notes   (R : Release) return Description_String;
   --  Specific to release

   function Version (R : Release) return Semantic_Versioning.Version;

   function Dependencies (R : Release) return Conditional.Dependencies;
   --  Retrieve dependencies as-is from the TOML description

   function Dependencies (R : Release;
                          P : Alire.Properties.Vector)
                          return Conditional.Dependencies;
   --  Retrieve only the dependencies that apply on platform P

   function Properties (R : Release) return Conditional.Properties;

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
   --  Return the relative project paths defined by this release. If this is a
   --  child project, these paths have to be composed with the parent root dir
   --  instead of its own root dir.

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

   function Tag (R : Release) return Alire.Properties.Vector;

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
   --  Properties that R has under platform properties P

   function Outward_Dependencies (R : Release) return Conditional.Dependencies;

   type Release (Prj_Len,
                 Notes_Len : Natural) is
     new Interfaces.Tomifiable
     and Interfaces.Detomifiable
     and Interfaces.Yamlable
   with record
      Name         : Crate_Name (1 .. Prj_Len);
      Alias        : UString; -- I finally gave up on constraints
      Version      : Semantic_Versioning.Version;
      Origin       : Origins.Origin;
      Notes        : Description_String (1 .. Notes_Len);
      Dependencies : Conditional.Dependencies;
      --  Note on child releases: these are the explicit dependencies only. The
      --  implicit dependency on the parent crate is injected only to the outer
      --  world. Use Outwards_Dependencies if for some reason you need the full
      --  list of dependencies in this body.
      Forbidden    : Conditional.Dependencies;
      Properties   : Conditional.Properties;
      Available    : Requisites.Tree;
   end record;

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

   function Provides (R : Release) return Crate_Name
   is (if UStrings.Length (R.Alias) = 0
       then R.Name
       else +(+R.Alias));

   function Notes (R : Release) return Description_String
   is (R.Notes);

   function Dependencies (R : Release) return Conditional.Dependencies
   is (R.Outward_Dependencies);

   function Dependencies (R : Release;
                          P : Alire.Properties.Vector)
                          return Conditional.Dependencies
   is (R.Outward_Dependencies.Evaluate (P));

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

   function Description (R : Release) return Description_String
   --  Image returns "Description: Blah" so we have to cut.
   is (Utils.Tail
       (Conditional.Enumerate (R.Properties).Filter
        (Alire.TOML_Keys.Description).First_Element.Image, ' '));

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

   use all type Origins.Kinds;
   function Unique_Folder (R : Release) return Folder_String
   is ((+R.Name) & "_" &
         Utils.Head (Utils.Head (Image (R.Version), '-'), '+') & "_" &
         --  Remove patch/build strings that may violate folder valid chars
       (case R.Origin.Kind is
           when Child          => "child",
           when External       => "external",
           when Filesystem     => "filesystem",
           when System         => "system",
           when Source_Archive => R.Origin.Short_Unique_Id,
           when Git | Hg       => R.Origin.Short_Unique_Id,
           when SVN            => R.Origin.Commit));

   function On_Platform_Actions (R : Release;
                                 P : Alire.Properties.Vector)
                                 return Alire.Properties.Vector
   is (R.On_Platform_Properties (P, Actions.Action'Tag));

   function Satisfies (R   : Release;
                       Dep : Alire.Dependencies.Dependency)
                       return Boolean
   is (R.Name = Dep.Crate and then Dep.Versions.Contains (R.Version));

   function Version_Image (R : Release) return String
   is (Semantic_Versioning.Image (R.Version));

end Alire.Releases;
