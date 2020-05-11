with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Alire.Milestones is

   package Semver renames Semantic_Versioning;

   --------------------
   -- Crate_Versions --
   --------------------

   function Crate_Versions (Spec : String) return Allowed_Milestones
   is
      --  Locate and identify the version operator
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      Op_Pos  : constant Natural := Index (Spec, To_Set ("=^~<>/("), Inside);
      Name    : constant String  := (if Op_Pos > Spec'First
                                     then Spec (Spec'First .. Op_Pos - 1)
                                     else Spec);
      Result  : constant Semver.Extended.Result :=
                  (if Op_Pos > Spec'First
                   then Semver.Extended.Parse (Spec (Op_Pos .. Spec'Last))
                   else Semver.Extended.Parse ("*"));
   begin
      if Result.Valid then
         return M : Allowed_Milestones (Name'Length) do
            M.Crate  := +Name;
            M.Versions := Result.Set;
         end return;
      else
         Trace.Error ("Invalid version set expression: "
                      & Spec (Op_Pos .. Spec'Last));
         Trace.Error (Result.Error);
         raise Checked_Error with "Invalid version set expression";
      end if;
   exception
      when Alire.Checked_Error =>
         raise;
      when others =>
         Trace.Error ("A crate/version string was invalid");
         raise Checked_Error;
   end Crate_Versions;

end Alire.Milestones;
