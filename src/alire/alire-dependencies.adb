package body Alire.Dependencies is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (Key    : String;
                       Value  : TOML.TOML_Value) return Dependency
   is
      package SV renames Semantic_Versioning;
      Version_Str : constant String := Value.As_String;
      EVS         : constant SV.Extended.Version_Set :=
                      SV.Extended.Value (Version_Str);
   begin
      return New_Dependency (+Utils.To_Lower_Case (Key), EVS);
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
      use TOML_Adapters;
   begin
      return +Dep.Versions.Image;
   end To_TOML;

end Alire.Dependencies;
