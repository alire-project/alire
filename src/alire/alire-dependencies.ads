with Alire.Interfaces;
with Alire.Milestones;
with Alire.TOML_Adapters;
with Alire.Utils;

with Semantic_Versioning.Basic;
with Semantic_Versioning.Extended;

with TOML; use all type TOML.Any_Value_Kind;

private with Alire.Utils.TTY;

package Alire.Dependencies with Preelaborate is

   --  A single dependency is a crate name plus a version set

   type Dependency (<>) is
     new Interfaces.Classificable -- since the crate name is the key
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

   function Crate (Dep : Dependency) return Crate_Name;

   function Versions (Dep : Dependency)
                      return Semantic_Versioning.Extended.Version_Set;

   function Image (Dep : Dependency) return String;
   --  Standard-style version image, e.g. "make^3.1"

   overriding
   function TTY_Image (Dep : Dependency) return String;

   overriding
   function Key (Dep : Dependency) return String;

   function From_Milestones (Allowed : Milestones.Allowed_Milestones)
                             return Dependency;

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

private

   package TTY renames Utils.TTY;

   type Dependency (Name_Len : Natural) is
     new Interfaces.Classificable
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

   function Crate (Dep : Dependency) return Crate_Name is (Dep.Crate);

   function Versions (Dep : Dependency)
                      return Semantic_Versioning.Extended.Version_Set
   is (Dep.Versions);

   function From_Milestones (Allowed : Milestones.Allowed_Milestones)
                             return Dependency is
     (New_Dependency (Allowed.Crate, Allowed.Versions));

   function Image (Dep : Dependency) return String is
     ((+Dep.Crate) & Dep.Versions.Image);

   overriding
   function TTY_Image (Dep : Dependency) return String is
     (TTY.Name (+Dep.Crate) & TTY.Version (Dep.Versions.Image));

   overriding
   function To_YAML (Dep : Dependency) return String is
     ("{crate: """ & Utils.To_Lower_Case (+Dep.Crate) &
        """, version: """ & Dep.Versions.Image &
        """}");

   overriding function Key (Dep : Dependency) return String is (+Dep.Crate);

end Alire.Dependencies;
