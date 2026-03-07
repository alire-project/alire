with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Alire.TOML_Adapters;

with Semantic_Versioning;

package body Alire.Dependencies is

   -----------------
   -- From_String --
   -----------------

   function From_String (Spec : String) return Dependency
   is
      --  Locate and identify the version operator
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      Op_Pos  : constant Natural := Index (Spec, To_Set ("*=^~<>/("), Inside);
      Name    : constant String  := (if Op_Pos > Spec'First
                                     then Spec (Spec'First .. Op_Pos - 1)
                                     else Spec);
      Result  : constant Semver.Extended.Result :=
                  (if Op_Pos > Spec'First
                   then Semver.Extended.Parse (Spec (Op_Pos .. Spec'Last))
                   else Semver.Extended.Parse ("*"));
   begin
      if Result.Valid then
         return New_Dependency (+Name, Result.Set);
      else
         Raise_Checked_Error ("Invalid version set expression: "
                              & Spec (Op_Pos .. Spec'Last));
      end if;
   exception
      when Alire.Checked_Error =>
         raise;
      when E : others =>
         Log_Exception (E);
         Raise_Checked_Error ("A crate/version string was invalid");
   end From_String;

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
      return New_Dependency (+AAA.Strings.To_Lower_Case (Key), EVS);
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
