pragma Alire_Test;
with AAA.Strings;

with Alire.VFS;

procedure Alr_Tests.Contained_Paths is

   --  Paths relative to a crate at Base within its origin may go up from
   --  Base, but not above the origin root.

   procedure Check (Path : String; Base : String; Reason : String := "") is
      Error : constant String :=
                VFS.Error_In_Contained_Path (Path, Within => Base);
   begin
      if Reason = "" then
         pragma Assert (Error = "",
                        "Should be valid: " & Path & " from " & Base & ": "
                        & Error);
      else
         pragma Assert (Error /= "",
                        "Should be invalid: " & Path & " from " & Base);
         pragma Assert (AAA.Strings.Contains (Error, Reason),
                        "Unexpected reason for " & Path & ": " & Error);
      end if;
   end Check;

   Escapes : constant String := "above the root";

begin
   --  Crate at the origin root

   Check ("",          "");
   Check ("x.gpr",     "");
   Check ("./a/b/",    "");
   Check ("a/../b",    "");
   Check ("..",        "", Escapes);
   Check ("../x.gpr",  "", Escapes);
   Check ("a/../../x", "", Escapes);

   --  Crate in a subdir, written in any of its accepted forms

   for Base of AAA.Strings.To_Vector ("a/b")
                 .Append ("./a/b/")
                 .Append ("a//b")
                 .Append ("./a/./b")
   loop
      Check ("../x.gpr",          Base);
      Check ("../../x.gpr",       Base);
      Check ("../../a/b/../x",    Base);
      Check ("./.././../x.gpr",   Base);
      Check ("../../../x.gpr",    Base, Escapes);
      Check ("c/../../../../x",   Base, Escapes);
      Check ("../../..",          Base, Escapes);
   end loop;

   --  Going above the root is rejected even if the path comes back down

   Check ("../a/x.gpr",       "a");
   Check ("../../r/a/x.gpr",  "a", Escapes);

   --  Paths must also be portable

   Check ("/x.gpr",   "a", "relative path");
   Check ("C:x.gpr",  "a", "':'");
   Check ("..\x.gpr", "a", "forward slashes");
end Alr_Tests.Contained_Paths;
