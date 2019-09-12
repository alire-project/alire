with Alire.Interfaces;
with Alire.TOML_Adapters;
with Alire.Utils;

with Semantic_Versioning;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.Dependencies with Preelaborate is

   subtype Names is Alire.Project;

   --  A single dependency is a project name plus a version set

   type Dependency (<>) is
     new Interfaces.Classificable -- since the project name is the key
     and Interfaces.Tomifiable
     and Interfaces.Yamlable
   with private;

   function New_Dependency (Project  : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set)
                            return Dependency;

   function Project (Dep : Dependency) return Names;

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set;

   function Image (Dep : Dependency) return String;
   --  String representation of the dependency, e.g. "^1.0.0"

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

   function From_TOML (From : TOML_Adapters.Key_Queue) return Dependency;
   --  Wrapper for case loaders; may fail as the previous function.

   overriding
   function To_TOML (Dep : Dependency) return TOML.TOML_Value;

   overriding
   function To_YAML (Dep : Dependency) return String;

   function Unavailable return Dependency;
   --  Special never available dependency to beautify a bit textual outputs

private

   type Dependency (Name_Len : Natural) is
     new Interfaces.Classificable
     and Interfaces.Tomifiable
     and Interfaces.Yamlable
   with record
      Project    : Alire.Project (1 .. Name_Len);
      Versions   : Semantic_Versioning.Version_Set;
   end record;

   function New_Dependency (Project  : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set)
                            return Dependency
   is (Project'Length, Project, Versions);

   function Project (Dep : Dependency) return Names is (Dep.Project);

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set
   is (Dep.Versions);

   function Image (Dep : Dependency) return String is
     (if Dep = Unavailable
      then "Unavailable"
      else
        (Utils.To_Lower_Case (+Dep.Project) & " is " &
           Semantic_Versioning.Image_Ada (Dep.Versions)));

   overriding
   function To_YAML (Dep : Dependency) return String is
     (if Dep = Unavailable
      then "{}"
      else
        ("{crate: """ & Utils.To_Lower_Case (+Dep.Project) &
           """, version: """ & Semantic_Versioning.Image_Ada (Dep.Versions) &
           """}"));

   overriding function Key (Dep : Dependency) return String is (+Dep.Project);

   function Unavailable return Dependency
   is (New_Dependency ("alire",
                       Semantic_Versioning.Exactly
                         (Semantic_Versioning.V ("0"))));

end Alire.Dependencies;
