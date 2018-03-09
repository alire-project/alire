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
      Name    : constant Alire.Name_String := (if Op_Pos > Spec'First
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
      return (Name'Length, Name, Versions);
   end Project_Versions;

end Alr.Parsers;
