with Alire.Interfaces;
with Alire.Utils;

with Semantic_Versioning;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.Dependencies with Preelaborate is

   subtype Names is Alire.Project;

   --  A single dependency is a project name plus a version set

   type Dependency (<>) is new
     Interfaces.Classificable and -- since the project name is the key
     Interfaces.Tomifiable with private;

   function New_Dependency (Project  : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set) return Dependency;

   function Project (Dep : Dependency) return Names;

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set;

   function Image (Dep : Dependency) return String;

   overriding function Key (Dep : Dependency) return String;

   overriding function To_TOML (Dep : Dependency) return TOML.TOML_Value;
   --  String representation of the dependency, e.g. "^1.0.0"

   function Unavailable return Dependency;
   --  Special never available dependency to beautify a bit textual outputs

private

   type Dependency (Name_Len : Natural) is New
     Interfaces.Classificable and
     Interfaces.Tomifiable with
   record
      Project    : Alire.Project (1 .. Name_Len);
      Versions   : Semantic_Versioning.Version_Set;
   end record;

   function New_Dependency (Project  : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set) return Dependency
   is (Project'Length, Project, Versions);

   function Project (Dep : Dependency) return Names is (Dep.Project);

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set is
     (Dep.Versions);

   function Image (Dep : Dependency) return String is -- Exceptional case: alire=0.0.0 means Unavailable
     (if Dep = Unavailable
      then "Unavailable"
      else
        (Utils.To_Lower_Case (+Dep.Project) & " is " &
           Semantic_Versioning.Image (Dep.Versions)));

   overriding function Key (Dep : Dependency) return String is (+Dep.Project);

   function Unavailable return Dependency is
     (New_Dependency ("alire", Semantic_Versioning.Exactly (Semantic_Versioning.V ("0"))));

end Alire.Dependencies;
