with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Alr.Files;
with Alr.Hardcoded;
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

   procedure Generate_Full_Index (Session_Path, Index_Folder : String) is
      File     : File_Type;
      Filename : constant String := "alr-index.ads";

      use Ada.Directories;
      use Alr.OS_Lib;

      ---------------
      -- Add_Entry --
      ---------------

      procedure Add_Entry (Found : Directory_Entry_Type) is
         Name : constant String := Utils.To_Lower_Case (Simple_Name (Found));
      begin
         if Kind (Found) = Ordinary_File then
            if Name'Length >= String'("alire-index-X.ads")'Length and then
              Name (Name'Last - 3 .. Name'Last) = ".ads" and then
              Name (Name'First .. Name'First + String'("alire-index-")'Length - 1) = "alire-index-"
            then
               Log ("Indexing " & Full_Name (Found), Debug);
               Put_Line (File, "with Alire.Index." &
                           Utils.To_Mixed_Case (Project (Simple_Name (Found))) & ";");
            else
               Log ("Unexpected file in index folder: " & Full_Name (Found));
            end if;
         end if;
      end Add_Entry;

   begin
      Create (File, Out_File, Session_Path / Filename);

      Manual_Warning (File);

      Put_Line (File, "pragma Warnings (Off);");

      OS_Lib.Traverse_Folder (Index_Folder, Add_Entry'Access, Recurse => True);

--        Search (Index_Folder,
--                "alire-index-*.ads",
--                (Ordinary_File => True, Directory => True, others => False),
--                Add_Entry'Access);

      New_Line (File);

      Put_Line (File, "package Alr.Index is");
      Put_Line (File, "end Alr.Index;");

      Close (File);
   end Generate_Full_Index;

   ------------------
   -- Generate_Gpr --
   ------------------

   procedure Generate_Agg_Gpr (Instance : Query.Instance;
                               Root     : Alire.Releases.Release)
   is
      use all type Utils.String_Vectors.Cursor;

      File     : File_Type;
      Filename : constant String := Hardcoded.Build_File (Root.Project);
      Prjname  : constant String := Utils.To_Mixed_Case (Filename (Filename'First .. Filename'Last - 4));

      First    : Boolean := True;

      use Alr.OS_Lib;

      GPR_Files : constant Utils.String_Vector := Root.GPR_Files;

   begin
      Log ("Generating GPR for " & Root.Milestone.Image & " with" & Instance.Length'Img & " dependencies", Detail);

      Files.Backup_If_Existing (Filename);

      Create (File, Out_File, Filename);

      Put_Line (File, "aggregate project " & Prjname & " is");
      New_Line (File);

      Manual_Warning (File);

      Put_Line (File, Tab_1 & "for Project_Files use (");
      for I in GPR_Files.Iterate loop
         Put (File, Tab_2 & Q (GPR_Files (I)));
         if I /= GPR_Files.Last then
            Put_line (File, ", ");
         end if;
      end loop;
      Put_Line (File, Tab_2 & ");");
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
            Put (File, Tab_2 & """" & Hardcoded.Projects_Folder / Rel.Unique_Folder & """");
         end if;
      end loop;
      Put_Line (File, ");");
      New_Line (File);

      Put_Line (File, Tab_1 & "for external (""ALIRE"") use ""True"";");
      New_Line (File);

      Put_Line (File, "end " & Prjname & ";");

      Close (File);
   end Generate_Agg_Gpr;

   ----------------------------
   -- Generate_Project_Alire --
   ----------------------------

   procedure Generate_Prj_Alr (Instance : Query.Instance;
                               Root     : Alire.Releases.Release;
                               Exact    : Boolean := True;
                               Filename : String := "")
   is
      File : File_Type;
      Name : constant String := (if Filename /= ""
                                 then Filename
                                 else Hardcoded.Alire_File (Root.Project));
   begin
      if Instance.Contains (Root.Project) then
         declare
            Pruned_Instance : Query.Instance := Instance;
         begin
            Pruned_Instance.Delete (Root.Project);
            Generate_Prj_Alr (Pruned_Instance, Root);
            return;
         end;
      end if;

      Files.Backup_If_Existing (Name);

      Create (File, Out_File, Name);

      Put_Line (File, "with Alire.Project; use Alire.Project;");
      New_Line (File);

      Put_Line (File, "package " & Utils.To_Mixed_Case (Root.Project) & "_Alr is");
      New_Line (File);
      Put_Line (File, Tab_1 & "Working_Release : constant Release := Set_Root_Project (");
      Put_Line (File, Tab_1 & Tab_1 & Q (Root.Project) & ",");
      Put_Line (File, Tab_1 & Tab_1 & "V (" & Q (Semver.Image (Root.Version)) & "),");

      if Instance.Is_Empty then
         Put_Line (File, Tab_2 & "Depends_On => No_Dependencies);");
      else
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
                              else "Within_Major (") &
                      Q (Rel.Project) &
                      ", V (" & Q (Semver.Image (Rel.Version)) & "))");
               First := False;
            end loop;
         end;

         Put_Line (File, ");");
      end if;
      New_Line (File);

      Put_Line (File, "   --  An explicit dependency on alr is only needed if you want to compile this file.");
      Put_Line (File, "   --  To do so, include the ""alr.gpr"" project in your own project file.");
      Put_Line (File, "   --  Once you are satisfied with your own dependencies it can be safely removed.");
      New_Line (File);

      Put_Line (File, "end " & Utils.To_Mixed_Case (Root.Project) & "_Alr;");

      Close (File);
   end Generate_Prj_Alr;

   ----------------------
   -- Generate_Session --
   ----------------------

   procedure Generate_Session (Session_Path : String;
                               Alire_File   : String := "") is
      use Ada.Directories;
      use Alr.OS_Lib;

      File : File_Type;
      Hash : constant String := (if Alire_File /= "" then Utils.Hash_File (Alire_File) else "no-alr-file");
   begin
      Create (File, Out_File, Session_Path / "alr-session.ads");

      Put_Line (File, "pragma Warnings (Off);");

      --  Depend on the project alr file that does the root registration
      if Alire_File /= "" then
         Put_Line (File, "with " & Utils.To_Mixed_Case (Base_Name (Alire_File)) & ";");
         New_Line (File);
      end if;

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
