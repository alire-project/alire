with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

with Alire.TOML_Adapters;
with Alire.TOML_Keys;

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

      Version_Str : Ada.Strings.Unbounded.Unbounded_String;
      Optional    : Boolean := False;
      Features    : AAA.Strings.Set;
      Defaults    : Boolean := True;
   begin
      if Value.Kind = TOML.TOML_String then
         Version_Str := Ada.Strings.Unbounded.To_Unbounded_String
           (Value.As_String);
      else
         declare
            From : constant TOML_Adapters.Key_Queue :=
              TOML_Adapters.From (Value, "dependency " & Key & ":");
         begin
            Version_Str := Ada.Strings.Unbounded.To_Unbounded_String
              (From.Checked_Pop (Alire.TOML_Keys.Version,
                                 TOML.TOML_String).As_String);

            if From.Contains ("optional") then
               Optional := From.Checked_Pop
                 ("optional", TOML.TOML_Boolean).As_Boolean;
            end if;

            if From.Contains (Alire.TOML_Keys.Default_Features) then
               Defaults := From.Checked_Pop
                 (Alire.TOML_Keys.Default_Features,
                  TOML.TOML_Boolean).As_Boolean;
            end if;

            if From.Contains (Alire.TOML_Keys.Features) then
               for Item of TOML_Adapters.To_Vector
                 (From.Checked_Pop (Alire.TOML_Keys.Features,
                                    TOML.TOML_Array))
               loop
                  if Item = "" then
                     From.Checked_Error ("feature name cannot be empty");
                  end if;
                  Features.Include (AAA.Strings.To_Lower_Case (Item));
               end loop;
            end if;

            From.Report_Extra_Keys;
         end;
      end if;

      return New_Dependency
        (+AAA.Strings.To_Lower_Case (Key),
         SV.Extended.Value (Ada.Strings.Unbounded.To_String (Version_Str)),
         Optional => Optional,
         Features => Features,
         Default_Features => Defaults,
         Feature_Syntax => Value.Kind = TOML.TOML_Table);
      --  TODO: if no operator appears the version, this results in strict
      --  match. Rust, for example, assumes caret (^) in this case. Do we want
      --  to do the same?
   exception
      when SV.Malformed_Input =>
         raise Checked_Error with
           "version set invalid: "
           & Ada.Strings.Unbounded.To_String (Version_Str);
   end From_TOML;

   --------------------
   -- Manifest_Image --
   --------------------

   function Manifest_Image (Dep : Dependency) return String is
      use AAA.Strings;
   begin
      if not Dep.Feature_Syntax
      then
         return (+Dep.Crate) & " = " & '"' & Dep.Versions.Image & '"';
      else
         return (+Dep.Crate) & " = { version = """ & Dep.Versions.Image
           & """"
           & (if Dep.Optional then ", optional = true" else "")
           & (if Dep.Default_Features
              then ""
              else ", default-features = false")
           & (if Dep.Features.Is_Empty
              then ""
              else ", features = ["""
                & Dep.Features.To_Vector.Flatten (""", """) & """]")
           & " }";
      end if;
   end Manifest_Image;

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (Dep : Dependency) return TOML.TOML_Value is
      use TOML_Adapters;
   begin
      if not Dep.Feature_Syntax
      then
         return +Dep.Versions.Image;
      else
         return Table : constant TOML.TOML_Value := TOML.Create_Table do
            Table.Set (Alire.TOML_Keys.Version, +Dep.Versions.Image);
            if Dep.Optional then
               Table.Set ("optional", TOML.Create_Boolean (True));
            end if;
            if not Dep.Default_Features then
               Table.Set (Alire.TOML_Keys.Default_Features,
                          TOML.Create_Boolean (False));
            end if;
            if not Dep.Features.Is_Empty then
               Table.Set (Alire.TOML_Keys.Features,
                          +Dep.Features.To_Vector);
            end if;
         end return;
      end if;
   end To_TOML;

end Alire.Dependencies;
