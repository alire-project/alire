with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Alr.Parsers is

   package Semver renames Semantic_Versioning;

   ----------------------
   -- Project_Versions --
   ----------------------

   function Project_Versions (Spec : String) return Allowed_Milestones
   is
   --  Locate and identify the version operator
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      Op_Pos  : constant Natural := Index (Spec, To_Set ("=^~"), Inside);

      --  Ready to separate name from version, and operator if existing
      Name    : constant String := (if Op_Pos > Spec'First
                                    then Spec (Spec'First .. Op_Pos - 1)
                                    else Spec);

      Op      : constant Character := (if Op_Pos > Spec'First
                                       then Spec (Op_Pos)
                                       else ASCII.NUL);

      V       : constant Semver.Version := (if Op_Pos > Spec'First
                                            then Semver.Relaxed (Spec (Op_Pos + 1 .. Spec'Last))
                                            else Semver.V ("0.0.0"));

      Versions : constant Semver.Version_Set := (case Op is
                                                    when ASCII.NUL => Semver.Any,
                                                    when '='       => Semver.Exactly (V),
                                                    when '^'       => Semver.Within_Major (V),
                                                    when '~'       => Semver.Within_Minor (V),
                                                    when others    => raise Constraint_Error with "Unrecognized version operator: " & Op);
   begin
      --  Previous return with copy caused double free on finalize of Versions???
      return M : Allowed_Milestones (Name'Length) do
         M.Project  := +Name;
         M.Versions := Versions;
      end return;
   exception
      when others =>
         Trace.Error ("A project/version string was invalid");
         raise Command_Failed;
   end Project_Versions;

end Alr.Parsers;
