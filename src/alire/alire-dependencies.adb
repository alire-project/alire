with Alire.TOML_Adapters;

package body Alire.Dependencies is

   overriding function To_TOML (Dep : Dependency) return TOML.TOML_Value is
      use Semantic_Versioning;
      use TOML_Adapters;
   begin
      if Dep.Versions = Any then
         return +"any";
      elsif Length (Dep.Versions) > 1 then
         raise Unimplemented; -- TODO (but not yet in index format)
      else
         return +Image_Abbreviated (Dep.Versions,
                                    Unicode        => True,
                                    Implicit_Equal => True);
      end if;
   end To_TOML;

end Alire.Dependencies;
