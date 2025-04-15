with Alire.Properties;
with Alire.TOML_Adapters;

package Alire.Origins.Mirrors is

   subtype Mirror_Kinds is Origins.Kinds with
     Predicate =>
       Mirror_Kinds in VCS_Kinds | Source_Archive | Binary_Archive;

   type Mirror is tagged private;

   function Kind (This : Mirror) return Mirror_Kinds;

   function URL (This : Mirror) return Alire.URL;

   function Whenever (This : Mirror; Env : Properties.Vector)
                      return Mirror;

   function Is_Available (This : Mirror; Env : Properties.Vector)
                          return Boolean;
   --  Availability in the sense of existence, as conditional expressions can
   --  resolve to nothing depending on Env. The "available" field is part of a
   --  release and not of an origin/mirror.

   function From_TOML (From : Alire.TOML_Adapters.Key_Queue) return Mirror;

   function To_TOML (This : Mirror) return TOML.TOML_Value;

private

   subtype Mirror_Name is UString with
     Predicate => Is_Valid_Name (+Mirror_Name);

   package Conditional_Archives is new Alire.Conditional_Trees
     (Values => Archive_Data,
      Image  => Alire.Origins.Binary_Image);

   type Mirror_Data (Kind : Mirror_Kinds := Git) is record
      Name : Mirror_Name;
      case Kind is
         when Binary_Archive =>
            Binary_Mirror : Conditional_Archive;
            --  We reuse conditional binary origins, but not all fields are
            --  informed in the case of mirrors: we do not use hashes

         when VCS_Kinds =>
            Repo_URL : Unbounded_String;

         when Source_Archive =>
            Src_URL         : Unbounded_String;
            Src_File_Name   : Unbounded_String;
            Src_File_Format : Known_Source_Archive_Format;
      end case;
   end record;

   type Mirror is tagged record
      Data : Mirror_Data;
      --  Unserialized information

      Raw  : TOML.TOML_Value;
      --  The original TOML object, for To_TOML
   end record;

end Alire.Origins.Mirrors;
