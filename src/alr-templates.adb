with Ada.Text_IO; use Ada.Text_IO;

with Alr.OS_Lib;

with Semantic_Versioning;

package body Alr.Templates is

   package Semver renames Semantic_Versioning;

   procedure Generate_Project_Alire (Instance : Alire.Index.Instance;
                                     Root     : Alire.Releases.Release)
   is
      File : File_Type;

      Tab_1 : constant String (1 .. 3) := (others => ' ');
      Tab_2 : constant String := Tab_1 & Tab_1;
      Tab_3 : constant String := Tab_2 & Tab_1;

      Q    : constant String (1 .. 1) := (1 => '"');
   begin
      if Instance.Contains (Root.Project) then
         declare
            Pruned_Instance : Alire.Index.Instance := Instance;
         begin
            Pruned_Instance.Delete (Root.Project);
            Generate_Project_Alire (Pruned_Instance, Root);
            return;
         end;
      end if;

      Create (File, Out_File, OS_Lib.Alire_File (Root.Project));

      Put_Line (File, "package Alire.Index." & Root.Project & " is");
      New_Line (File);
      Put_Line (File, Tab_1 & "working_Release : constant Release := Register_Local (");
      Put_Line (File, Tab_1 & Tab_1 & Q & Root.Project & Q & ",");
      Put_Line (File, Tab_1 & Tab_1 & "V (" & Q & Semver.Image (Root.Version) & Q & "),");
      Put      (File, Tab_1 & Tab_1 & "License => Unknown");

      if Instance.Is_Empty then
         Put_Line (File, ");");
      else
         Put_Line (File, ",");

         Put (File, Tab_2 & "Depends_On =>");

         declare
            First : Boolean := True;
         begin
            for Rel of Instance loop
               if not First then
                  Put_line (File, " and");
               else
                  New_Line (File);
               end if;
               Put (File, Tab_3 &
                      "Exactly (" & Q & Rel.Project & Q &
                      ", V (" & Q & Semver.Image (Rel.Version) & Q & "))");
               First := False;
            end loop;
         end;

         Put_Line (File, ");");
      end if;

      New_Line (File);
      Put_Line (File, "end Alire.Index." & Root.Project & ";");

      Close (File);
   end Generate_Project_Alire;

end Alr.Templates;
