with Alire.Interfaces;
with Alire.Milestones;
with Alire.Utils;

with Semantic_Versioning.Basic;
with Semantic_Versioning.Extended;

with TOML; use all type TOML.Any_Value_Kind;

private with Alire.Utils.TTY;

package Alire.Dependencies with Preelaborate is

   --  A single dependency is a crate name plus a version set

   type Dependency (<>) is
     new Interfaces.Classifiable -- since the crate name is the key
     and Interfaces.Colorable
     and Interfaces.Tomifiable
     and Interfaces.Yamlable
   with private;

   function New_Dependency
     (Crate    : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return Dependency;

   function New_Dependency
     (Crate   : Crate_Name;
      Version : Semantic_Versioning.Version)
      return Dependency;

   function New_Dependency (Milestone : Milestones.Milestone;
                            Updatable : Boolean := False)
                            return Dependency;
   --  Return either an exact crate=version or a safely upgradable crate^1.x
   --  dependency for the given milestone.

   function From_String (Spec : String) return Dependency;
   --  Intended to parse command-line dependencies given as crate[subset]:
   --  alr^1.0, alr=1.0, alr~0.7, etc. If no subset is specified, Any version
   --  is returned. May raise Checked_Error if parsing cannot succeed.

   function Crate (Dep : Dependency) return Crate_Name;

   function Versions (Dep : Dependency)
                      return Semantic_Versioning.Extended.Version_Set;

   function Image (Dep : Dependency) return String;
   --  Standard-style version image, e.g. "make^3.1"

   function Manifest_Image (Dep : Dependency) return String;
   --  Returns a line describing the dependency as it would appear in the
   --  manifest, e.g.: my_crate = "^3.2.1"

   overriding
   function TTY_Image (Dep : Dependency) return String;

   overriding
   function Key (Dep : Dependency) return String;

   function From_TOML (Key    : String;
                       Value  : TOML.TOML_Value) return Dependency with
     Pre =>
       (Key'Length >= Min_Name_Length or else
          raise Checked_Error with "dependency name too short") and then
       (Value.Kind = TOML.TOML_String or else
          raise Checked_Error with "dependency version must be a string");
   --  May raise Checked_Error with stored Alire.Errors.

   overriding
   function To_TOML (Dep : Dependency) return TOML.TOML_Value;
   --  Creates the RHS of the "crate = 'version'"

   overriding
   function To_YAML (Dep : Dependency) return String;

   function Lexicographical_Sort (L, R : Dependency) return Boolean;
   --  By name and then version set image

private

   type Dependency (Name_Len : Natural) is
     new Interfaces.Classifiable
     and Interfaces.Colorable
     and Interfaces.Tomifiable
     and Interfaces.Yamlable
   with record
      Crate      : Crate_Name (Name_Len);
      Versions   : Semantic_Versioning.Extended.Version_Set;
   end record;

   function New_Dependency
     (Crate    : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return Dependency
   is (Crate.Name'Length, Crate, Versions);

   function New_Dependency
     (Crate   : Crate_Name;
      Version : Semantic_Versioning.Version)
      return Dependency
   is (New_Dependency
       (Crate,
        Semantic_Versioning.Extended.To_Extended
            (Semantic_Versioning.Basic.Exactly (Version))));

   function New_Dependency (Milestone : Milestones.Milestone;
                            Updatable : Boolean := False)
                            return Dependency
   is (if Updatable
       then
          New_Dependency (Milestone.Crate,
                          Semantic_Versioning.Updatable (Milestone.Version))
       else
          New_Dependency (Milestone.Crate, Milestone.Version));

   function Crate (Dep : Dependency) return Crate_Name is (Dep.Crate);

   function Versions (Dep : Dependency)
                      return Semantic_Versioning.Extended.Version_Set
   is (Dep.Versions);

   function Image (Dep : Dependency) return String is
     ((+Dep.Crate) & Dep.Versions.Image);

   function Manifest_Image (Dep : Dependency) return String is
     ((+Dep.Crate) & " = " & '"' & Dep.Versions.Image & '"');

   overriding
   function TTY_Image (Dep : Dependency) return String is
     (Utils.TTY.Name (+Dep.Crate) & TTY.Version (Dep.Versions.Image));

   overriding
   function To_YAML (Dep : Dependency) return String is
     ("{crate: """ & AAA.Strings.To_Lower_Case (+Dep.Crate) &
        """, version: """ & Dep.Versions.Image &
        """}");

   overriding function Key (Dep : Dependency) return String is (+Dep.Crate);

   function Lexicographical_Sort (L, R : Dependency) return Boolean
   is (L.Crate < R.Crate or else
       (L.Crate = R.Crate and then L.Versions.Image < R.Versions.Image));

end Alire.Dependencies;
