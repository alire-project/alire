with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Alire.Containers;
with Alire.GPR;
with Alire.Milestones;
with Alire.Properties.Labeled; use all type Alire.Properties.Labeled.Labels;
with Alire.Properties.Scenarios;
with Alire.Releases.TOML_IO;
with Alire.Utils;

with Alr.Commands;
with Alr.Files;
with Alr.Paths;
with Alr.OS_Lib;
with Alr.Platform;
with Alr.Root;
with Alr.Utils;

package body Alr.Templates is

   Tab_1 : constant String (1 .. 3) := (others => ' ');
   Tab_2 : constant String := Tab_1 & Tab_1;
--     Tab_3 : constant String := Tab_2 & Tab_1;

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

   procedure Generate_Full_Index (File : in out Ada.Text_IO.File_Type; Index_Folder : String) is
      use Ada.Directories;

      ---------------
      -- Add_Entry --
      ---------------

      procedure Add_Entry (Found : Directory_Entry_Type; Stop : in out Boolean) is
         pragma Unreferenced (Stop);
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
            elsif Name /= "alire-projects.ads" then
               Log ("Unexpected file in index folder: " & Full_Name (Found));
            end if;
         end if;
      end Add_Entry;

   begin
      OS_Lib.Traverse_Folder (Index_Folder, Add_Entry'Access, Recurse => True);
      New_Line (File);
   end Generate_Full_Index;

   ----------------------
   -- Generate_Agg_Gpr --
   ----------------------

   procedure Generate_Agg_Gpr (Root : Alire.Roots.Root) is
      Needed  : constant Query.Solution :=
                  Query.Resolve
                    (Root.Release.Dependencies.Evaluate (Platform.Properties),
                     Commands.Query_Policy);
   begin
      if Needed.Valid then
         Generate_Agg_Gpr (Needed.Releases, Root);
      else
         raise Command_Failed;
      end if;
   end Generate_Agg_Gpr;

   ----------------------
   -- Generate_Agg_Gpr --
   ----------------------

   procedure Generate_Agg_Gpr (Instance : Query.Instance;
                               Root     : Alire.Roots.Root)
   is
      use all type Utils.String_Vectors.Cursor;

      File     : File_Type;
      Filename : constant String := Root.Build_File;
      Prjname  : constant String := Utils.To_Mixed_Case (Ada.Directories.Base_Name (Filename));

      First    : Boolean := True;

      use Alr.OS_Lib;

      GPR_Files : Utils.String_Vector;
      All_Paths : Utils.String_Vector;
      Sorted_Paths : Alire.Utils.String_Set;

      Full_Instance : constant Query.Instance := (Instance.Including (Root.Release));
   begin
      if Query.Exists (Root.Release.Project, Root.Release.Version) then
         Log ("Generating GPR for release " & Root.Release.Milestone.Image &
                " with" & Instance.Length'Img & " dependencies", Detail);
      else
         Log ("Generating GPR for unreleased project " & Root.Release.Milestone.Image & " with" &
                Instance.Length'Img & " dependencies", Detail);
      end if;

      GPR_Files := Root.Release.Project_Files (Platform.Properties, With_Path => True);

      Files.Backup_If_Existing (Filename);

      Create (File, Out_File, Filename);

      Put_Line (File, "aggregate project " & Prjname & " is");
      New_Line (File);

      Manual_Warning (File);

      --  Absolute paths in the following could be made relative.
      --  That would allow moving working copies.
      --  Not worth the hassle yet.

      Put_Line (File, Tab_1 & "for Project_Files use (");
      for I in GPR_Files.Iterate loop
         Put (File, Tab_2 & Q (".." / GPR_Files (I)));
         if I /= GPR_Files.Last then
            Put_line (File, ", ");
         end if;
      end loop;
      Put_Line (File, ");");
      New_Line (File);

      --  First obtain all paths and then output them, if any needed
      for Rel of Instance.Including (Root.Release) loop
         if Rel.Project = Root.Release.Project then
            --  All_Paths.Append (".");
            null; -- That's the first path in aggregate projects anyway
         else
            All_Paths.Append (Paths.Alr_Working_Deps_Path / Rel.Unique_Folder);
         end if;

         --  Add non-root extra project paths, always
         for Path of Rel.Project_Paths (Platform.Properties) loop
            All_Paths.Append ((if Rel.Project = Root.Release.Project
                               then ".."
                               else Paths.Alr_Working_Deps_Path / Rel.Unique_Folder) / Path);
         end loop;
      end loop;

      --  Sort and remove duplicates in paths (may come from extension projects)
      for Path of All_Paths loop
         Sorted_Paths.Include (Path);
      end loop;

      if not Sorted_Paths.Is_Empty then
         Put (File, Tab_1 & "for Project_Path use (");

         for Path of Sorted_Paths loop
            if First then
               New_Line (File);
               First := False;
            else
               Put_Line (File, ",");
            end if;

            Put (File, Tab_2 & Q (Path));
         end loop;

         Put_Line (File, ");");
         New_Line (File);
      end if;

      --  Externals
      --  FIXME: what to do with duplicates? at a minimum research what gprbuild does (err, ignore...)
      for Release of Full_Instance loop
--           Put_Line ("REL: " & Release.Project_Str);
         for Prop of Release.On_Platform_Properties (Platform.Properties) loop
--              Put_Line ("PROP: " & Prop.Image);
            if Prop in Alire.Properties.Scenarios.Property'Class then
--                 Put_Line ("PROP is scenario");
               declare
                  use all type Alire.GPR.Variable_Kinds;
                  Variable : constant Alire.GPR.Variable :=
                               Alire.Properties.Scenarios.Property (Prop).Value;
               begin
--                    Put_Line ("KIND: " & Variable.Kind'Img);
                  if Variable.Kind = External then
                     Put_Line (File,
                               Tab_1 & "for External (" &
                                 Q (Variable.Name) & ") use " & Q (Variable.External_Value) & ";");
                  end if;
               end;
            end if;
         end loop;
      end loop;
      New_Line (File);

      Put_Line (File, Tab_1 & "for external (""ALIRE"") use ""True"";");
      New_Line (File);

      Put_Line (File, "end " & Prjname & ";");

      Close (File);
   end Generate_Agg_Gpr;

   ----------------------------
   -- Generate_Project_Alire --
   ----------------------------

   procedure Generate_Prj_Alr (Release  : Types.Release;
                               Filename : String)
   is
   begin
      Trace.Detail ("Generating " & Release.Project_Str & ".toml file for " &
                    Release.Milestone.Image &
                      " with" & Release.Dependencies.Leaf_Count'Img & " dependencies");

      --  Ensure working folder exists (might not upon first get)
      if not Paths.Is_Simple_Name (Filename) then
         OS_Lib.Create_Folder (Paths.Parent (Filename));
      end if;

      Files.Backup_If_Existing (Filename);

      Alire.Releases.TOML_IO.To_File (Release, Filename => Release.Project_Str & ".toml");
   end Generate_Prj_Alr;

   ----------------------
   -- Generate_Prj_Alr --
   ----------------------

   procedure Generate_Prj_Alr (Release : Types.Release) is
      Guard : OS_Lib.Folder_Guard (Commands.Enter_Project_Folder) with Unreferenced;
   begin
      Generate_Prj_Alr (Release, Root.Current.Crate_File);
   end Generate_Prj_Alr;

end Alr.Templates;
