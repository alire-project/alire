with Alire.Interfaces;

package Alire.Utils.YAML with Preelaborate is

   --  Needs to be split from Utils to avoid a dependency circularity.

   generic
      type T (<>) is new Alire.Interfaces.Yamlable with private;
      with package Vectors is new Ada.Containers.Indefinite_Vectors
        (Index_Type => <>, Element_Type => T);
      type Vector is new Vectors.Vector with private;
   function To_YAML (V : Vector) return String;
   --  Turn a vector of Yamlable into a YAML array

   function YAML_Stringify (Input : String) return String;
   --  Turn String data into YAML string, including enclosing double-quotes and
   --  escape characters.

end Alire.Utils.YAML;
