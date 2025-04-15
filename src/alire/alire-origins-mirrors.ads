with Alire.Properties;
with Alire.TOML_Adapters;

package Alire.Origins.Mirrors is

   subtype Mirrored_Kinds is Origins.Kinds with
     Predicate =>
       Mirrored_Kinds in VCS_Kinds | Source_Archive | Binary_Archive;

   type Mirror is tagged private;

   function Kind (This : Mirror) return Kinds;

   function URL (This : Mirror) return Alire.URL;

   function Whenever (This : Mirror; Env : Properties.Vector)
   return Mirror;

   function Is_Available (This : Mirror; Env : Properties.Vector)
            return Boolean;

   function From_TOML (From : Alire.TOML_Adapters.Key_Queue) return Mirror;

   function To_TOML (This : Mirror) return TOML.TOML_Value;

private

   subtype Mirror_Name is UString with
     Predicate => Is_Valid_Name (+Mirror_Name);

   package Conditional_Archives is new Alire.Conditional_Trees
     (Values => Archive_Data,
      Image  => Alire.Origins.Binary_Image);

   type Mirror_Data (Kind : Mirrored_Kinds := Git) is record
      Name : Mirror_Name;
      case Kind is
         when Alire.Origins.Binary_Archive =>
            Binary_Mirror : Conditional_Archive;

         when VCS_Kinds =>
            Repo_URL : Unbounded_String;

         when Alire.Origins.Source_Archive =>
            Src_URL : Unbounded_String;

         when others =>
            null;
      end case;
   end record;

   type Mirror is tagged record
      Data : Mirror_Data;
   end record;

end Alire.Origins.Mirrors;
