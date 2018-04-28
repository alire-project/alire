with Ada.Strings.Unbounded;

with McKae.XML.EZ_Out.Text_File;

package body AJUnitGen is

   ---------------
   -- Add_Suite --
   ---------------

   procedure Add_Suite (Col : in out Collection; Suite : Test_Suite'Class) is
   begin
      Col.Append (Test_Suite (Suite));
   end Add_Suite;

   --------------
   -- Add_Case --
   --------------

   procedure Add_Case (Suite : in out Test_Suite; Test : Test_Case) is
   begin
      Suite.Tests.Append (Test);
      Suite.Size := Suite.Size + 1;
      Suite.Counters (Test.Outcome) := Suite.Counters (Test.Outcome) + 1;
   end Add_Case;

   ---------------
   -- New_Suite --
   ---------------

   function New_Suite (Name : String) return Test_Suite is
   begin
      return (Name'Length, Name, others => <>);
   end New_Suite;

   --------------
   -- New_Case --
   --------------

   function New_Case (Name      : String;
                      Outcome   : Outcomes := Pass;
                      Classname : String   := "";
                      Message   : String   := "";
                      Output    : String   := "") return Test_Case is
   begin
      return (Name'Length,
              Classname'Length,
              Message'Length,
              Output'Length,
              Name,
              Outcome,
              Classname,
              Message,
              Output);
   end New_Case;

   -------------------
   -- To_Collection --
   -------------------

   function To_Collection (Suite : Test_Suite) return Collection'Class is
      Result : Collection;
   begin
      Result.Append (Suite);
      return Result;
   end To_Collection;

   -----------
   -- Write --
   -----------

   procedure Write (Col : Collection; File : Ada.Text_IO.File_Type) is
      use McKae.XML.EZ_Out.Text_File;
      Id : Natural := 0;
   begin
      Output_XML_Header (File);

      Start_Element (File, "testsuites");

      for S of Col loop
         Start_Element (File, "testsuite",
                        ("name" = S.Name,
                         "id" = Id,
                         "tests" = S.Size,
                         "errors" = S.Counters (Error),
                         "failures" = S.Counters (Fail),
                         "skipped" = S.Counters (Skip)));

         for T of S.Tests loop
            --  Element start
            case T.Outcome is
               when Pass =>
                  Output_Element (File, "testcase",
                                  Content => "",
                                  Attrs   =>
                                    ("name" = T.Name,
                                     "classname" = T.Classname,
                                     "status" = T.Outcome'Img));
               when others =>
                  Start_Element (File, "testcase",
                                 Attrs   =>
                                   ("name" = T.Name,
                                    "classname" = T.Classname,
                                    "status" = T.Outcome'Img));
            end case;

            --  Element content
            case T.Outcome is
               when Pass =>
                  null;
               when Error =>
                  Output_Element (File, "error", T.Output, "message" = T.Message);
               when Fail =>
                  Output_Element (File, "failure", T.Output, "message" = T.Message);
               when Skip =>
                  Output_Element (File, "skipped", T.Output, "message" = T.Message);
            end case;

            --  Element end
            case T.Outcome is
               when Pass =>
                  null;
               when others =>
                  End_Element (File, "testcase");
            end case;
         end loop;

         End_Element (File, "testsuite");
         Id := Id + 1;
      end loop;

      End_Element (File, "testsuites");
   end Write;

   Unsafe_Chars : constant array (Character) of Boolean :=
                    ('&' | ''' | '"' | '<' | '>' => True,
                     others                      => False);

   Replacings : constant array (Character) of access String :=
                  ('&'    => new String'("&amp;"),
                   '''    => new String'("&apos;"),
                   '"'    => new String'("&quot;"),
                   '<'    => new String'("&lt;"),
                   '>'    => new String'("&gt;"),
                   others => new String'(""));

   ------------
   -- Escape --
   ------------

   function Escape (S : in String) return String is
      use Ada.Strings.Unbounded;
      package ASU renames Ada.Strings.Unbounded;


      Aux : Unbounded_String := To_Unbounded_String (S);
      Pos : Positive := 1;
      Cur : Character;
   begin
      loop
         exit when Pos > ASU.Length (Aux);

         Cur := ASU.Element (Aux, Pos);
         if Unsafe_Chars (Cur) then
            ASU.Replace_Slice (Aux, Pos, Pos, Replacings (Cur).all);
         end if;

         Pos := Pos + 1;
      end loop;

      return To_String (Aux);
   end Escape;

end AJUnitGen;
