package body Alire.Dependencies is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (Key    : String;
                       Value  : TOML.TOML_Value) return Dependency
   is
      package SV renames Semantic_Versioning;
      Version_Str : constant String := Value.As_String;
   begin
      return New_Dependency (+Utils.To_Lower_Case (Key),
                             SV.To_Set (Version_Str));
      --  TODO: if no operator appears the version, this results in strict
      --  match. Rust, for example, assumes caret (^) in this case. Do we want
      --  to do the same?
   exception
      when SV.Malformed_Input =>
         raise Checked_Error with "version set invalid: " & Version_Str;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

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
                                    Unicode        => False,
                                    Implicit_Equal => True);
      end if;
   end To_TOML;

end Alire.Dependencies;
