with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Alr.OS;
with Alr.OS_Lib;
with Alr.Utils;

with Semantic_Versioning;

package body Alr.Templates is

   package Semver renames Semantic_Versioning;

   Tab_1 : constant String (1 .. 3) := (others => ' ');
   Tab_2 : constant String := Tab_1 & Tab_1;
   Tab_3 : constant String := Tab_2 & Tab_1;

   function Q (S : String) return String renames Utils.Quote;

   -------------
   -- Project --
   -------------

   function Project (Filename : String) return String is
   begin
      for I in reverse Filename'Range loop
         if Filename (I) = '-' then
            return Filename (I + 1 .. Filename'Last - 4);
         end if;
      end loop;

      raise Program_Error with "Malformed index filename: " & Filename;
   end Project;

   --------------------
   -- Manual_Warning --
   --------------------

   procedure Manual_Warning (File : File_Type) is
   begin
      Put_Line (File, Tab_1 & "--  This is an automatically generated file. DO NOT EDIT MANUALLY!");
      New_Line (File);
   end Manual_Warning;

   --------------------
   -- Generate_Index --
   --------------------

   procedure Generate_Index (Session_Path, Index_Folder : String) is
      File     : File_Type;
      Filename : constant String := "alr-index.ads";

      use Ada.Directories;
      use Alr.OS_Lib;

      ---------------
      -- Add_Entry --
      ---------------

      procedure Add_Entry (Found : Directory_Entry_Type) is
      begin
         Put_Line (File, "with Alire.Index." &
                     Utils.To_Mixed_Case (Project (Simple_Name (Found))) & ";");
      end Add_Entry;

   begin
      Create (File, Out_File, Session_Path / Filename);

      Manual_Warning (File);

      Put_Line (File, "pragma Warnings (Off);");

      Search (Index_Folder,
              "alire-index-*.ads",
              (Ordinary_File => True, others => False),
              Add_Entry'Access);
      New_Line (File);

      Put_Line (File, "package Alr.Index is");
      Put_Line (File, "end Alr.Index;");

      Close (File);
   end Generate_Index;

   ------------------
   -- Generate_Gpr --
   ------------------

   procedure Generate_Gpr (Instance : Alire.Index.Instance;
                           Root     : Alire.Releases.Release)
   is
      File     : File_Type;
      Filename : constant String := OS_Lib.Build_File (Root.Project);
      Prjname  : constant String := Utils.To_Mixed_Case (Filename (Filename'First .. Filename'Last - 4));

      First    : Boolean := True;

      use Alr.OS_Lib;

   begin
      Log ("Generating GPR for " & Root.Milestone_Image & " with" & Instance.Length'Img & " dependencies", Verbose);

      Create (File, Out_File, Filename);

      Put_Line (File, "aggregate project " & Prjname & " is");
      New_Line (File);

      Manual_Warning (File);

      Put_Line (File, Tab_1 & "for Project_Files use (" & Q (Root.Project & ".gpr") & ");");
      New_Line (File);

      Put (File, Tab_1 & "for Project_Path use (");
      for Rel of Instance loop
         if First then
            New_Line (File);
            First := False;
         else
            Put_Line (File, ",");
         end if;
         if Rel.Project = Root.Project then
            Put (File, Tab_2 & """.""");
         else
            Put (File, Tab_2 & """" & OS.Projects_Folder / Rel.Unique_Folder & """");
         end if;
      end loop;
      Put_Line (File, ");");
      New_Line (File);

      Put_Line (File, Tab_1 & "for external (""ALIRE"") use ""True"";");
      New_Line (File);

      Put_Line (File, "end " & Prjname & ";");

      Close (File);
   end Generate_Gpr;

   ----------------------------
   -- Generate_Project_Alire --
   ----------------------------

   procedure Generate_Project_Alire (Instance : Alire.Index.Instance;
                                     Root     : Alire.Releases.Release;
                                     Exact    : Boolean := True;
                                     Filename : String := "")
   is
      File : File_Type;
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

      Create (File, Out_File, (if Filename /= "" then Filename
                                                 else OS_Lib.Alire_File (Root.Project)));

      Put_Line (File, "with Alr.Project; use Alr.Project;");
      New_Line (File);

      Put_Line (File, "package " & Utils.To_Mixed_Case (Root.Project) & "_Alr is");
      New_Line (File);
      Put_Line (File, Tab_1 & "Working_Release : constant Release := Set_Root_Project (");
      Put_Line (File, Tab_1 & Tab_1 & Q (Root.Project) & ",");
      Put_Line (File, Tab_1 & Tab_1 & "V (" & Q (Semver.Image (Root.Version)) & "),");
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
                    (if Exact then "Exactly ("
                              else "At_Least_Within_Major (") &
                      Q (Rel.Project) &
                      ", V (" & Q (Semver.Image (Rel.Version)) & "))");
               First := False;
            end loop;
         end;

         Put_Line (File, ");");
      end if;
      New_Line (File);

      Put_Line (File, "   --  The explicit dependency on alr is only needed if you want to compile this file.");
      Put_Line (File, "   --  Once you are satisfied with your own dependencies it can be safely removed.");
      New_Line (File);

      Put_Line (File, "end " & Utils.To_Mixed_Case (Root.Project) & "_Alr;");

      Close (File);
   end Generate_Project_Alire;

   ----------------------
   -- Generate_Session --
   ----------------------

   procedure Generate_Session (Session_Path, Alire_File : String) is
      use Ada.Directories;
      use Alr.OS_Lib;

      File : File_Type;
      Hash : constant String := Utils.Hash_File (Alire_File);
   begin
      Create (File, Out_File, Session_Path / "alr-session.ads");

      Put_Line (File, "pragma Warnings (Off);");

      --  Depend on the project alr file that does the root registration
      Put_Line (File, "with " & Utils.To_Mixed_Case (Base_Name (Alire_File)) & ";");
      New_Line (File);

      Put_Line (File, "package Alr.Session is");
      New_Line (File);

      Put_Line (File, Tab_1 & "--  This is a generated file. DO NOT EDIT MANUALLY!");
      New_Line (File);

      Put_Line (File, Tab_1 & "Hash : constant String := """ & Hash & """;");
      New_Line (File);

      Put_Line (File, "end Alr.Session;");

      Close (File);
   end Generate_Session;

end Alr.Templates;
