with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Alire.GPR;
with Alire.Index;
with Alire.Milestones;
with Alire.Properties.Labeled; use all type Alire.Properties.Labeled.Labels;
with Alire.Properties.Scenarios;
with Alire.Utils;

with Alr.Code;
with Alr.Commands;
with Alr.Commands.Withing;
with Alr.Files;
with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Platform;
with Alr.Utils;

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

   procedure Generate_Full_Index (File : in out Ada.Text_IO.File_Type; Index_Folder : String) is
      use Ada.Directories;
      use Alr.OS_Lib;

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
      Success : Boolean;
      Needed  : constant Query.Instance :=
                  Query.Resolve (Root.Dependencies.Evaluate (Platform.Properties),
                                 Success,
                                 Commands.Query_Policy);
   begin
      if Success then
         Generate_Agg_Gpr (Needed, Root);
      else
         raise Command_Failed;
      end if;
   end Generate_Agg_Gpr;

   ------------------
   -- Generate_Gpr --
   ------------------

   procedure Generate_Agg_Gpr (Instance : Query.Instance;
                               Root     : Alire.Roots.Root)
   is
      use all type Utils.String_Vectors.Cursor;

      File     : File_Type;
      Filename : constant String := Hardcoded.Working_Build_File;
      Prjname  : constant String := Utils.To_Mixed_Case (Ada.Directories.Base_Name (Filename));

      First    : Boolean := True;

      use Alr.OS_Lib;

      GPR_Files : Utils.String_Vector;
      All_Paths : Utils.String_Vector;
      Sorted_Paths : Alire.Utils.String_Set;

      Full_Instance : constant Query.Instance := (if Root.Is_Released
                                                  then Instance.Including (Root.Release)
                                                  else Instance);
   begin
      if Root.Is_Released then
         GPR_Files := Root.Release.Project_Files (Platform.Properties, With_Path => True);
         Log ("Generating GPR for release " & Root.Release.Milestone.Image &
                " with" & Instance.Length'Img & " dependencies", Detail);
      else
         GPR_Files.Append ((+Root.Project) & ".gpr");
         Log ("Generating GPR for unreleased project " & (+Root.Project) & " with" &
                Instance.Length'Img & " dependencies", Detail);
      end if;

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
         Put (File, Tab_2 & Q (Ada.Directories.Full_Name (GPR_Files (I))));
         if I /= GPR_Files.Last then
            Put_line (File, ", ");
         end if;
      end loop;
      Put_Line (File, ");");
      New_Line (File);

      --  First obtain all paths and then output them, if any needed
      for Rel of Instance loop
         if Rel.Project = Root.Project then
            --  All_Paths.Append (".");
            null; -- That's the first path in aggregate projects anyway
         else
            All_Paths.Append (Hardcoded.Projects_Folder / Rel.Unique_Folder);
         end if;

         --  Add non-root extra project paths, always
         for Path of Rel.Project_Paths (Platform.Properties) loop
            All_Paths.Append ((if Rel.Project = Root.Project
                               then "."
                               else Hardcoded.Projects_Folder / Rel.Unique_Folder / Path));
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

            Put (File, Tab_2 & Q (Ada.Directories.Full_Name (Path)));
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

   procedure Generate_Prj_Alr (Scenario : Generation_Scenarios;
                               Project  : Alire.Project;
                               Version  : Semantic_Versioning.Version :=
                                 Semantic_Versioning.V ("0");
                               Deps     : Alire.Dependencies.Vectors.Vector :=
                                 Alire.Dependencies.Vectors.No_Dependencies)
   is
      package Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

      Includes : Sets.Set; -- To sort them and remove duplicates

      File : File_Type;
      Name : constant String := Hardcoded.Working_Deps_File;

      Simple_Name : constant String := Ada.Directories.Simple_Name (Name);

      Pkg_Name : constant String := Simple_Name (Simple_Name'First .. Simple_Name'Last - 4);
   begin
      Trace.Detail ("Generating alr_deps.ads file for " &
                    (if Scenario = Released
                       then Alire.Milestones.New_Milestone (Project, Version).Image
                       else "unreleased project " & (+Project)) &
                      " with" & Deps.Length'Img & " dependencies");

      --  Ensure working folder exists (might not upon first get)
      OS_Lib.Create_Folder (Hardcoded.Alr_Working_Folder);
      Files.Backup_If_Existing (Name);
      Create (File, Out_File, Name);

      Put_Line (File, "--  Visibility of operators:");
      Put_Line (File, "with Alire.Index;    use Alire.Index;");
      Put_Line (File, "with Alire.Versions; use Alire.Versions.Expressions;");

      --  With generation

      if Scenario = Released then
         Includes.Include (Commands.Withing.With_Line (Project));
      else
         for Dep of Deps loop
            Includes.Include (Commands.Withing.With_Line (Dep.Project));
         end loop;
      end if;

      if not Includes.Is_Empty then
         New_Line (File);
         Put_Line (File, "--  Dependencies:");
         for Inc of Includes loop
            Put_Line (File, Inc);
         end loop;
      end if;

      New_Line (File);

      --  Spec generation

      Put_Line (File, "package " & Utils.To_Mixed_Case (Pkg_Name) & " is");
      New_Line (File);
      Put_Line (File, Tab_1 & "Current_Root : constant Root := Set_Root (");

      if Scenario = Released then
         --  Typed name plus version
         Put_Line (File, Tab_2 & Alire.Index.Get (Project).Ada_Identifier & ",");
         Put_Line (File, Tab_2 & "V (" & Q (Semver.Image (Version)) & "));");
      else
         --  Untyped name plus dependencies
         Put_Line (File, Tab_2 & Q (+Project) & ",");

         if Deps.Is_Empty then
            Put_Line (File, Tab_2 & "Dependencies => No_Dependencies);");
         else
            Put_Line (File, Tab_2 & "Dependencies =>");
            for Line of Code.Generate (Deps) loop
               Put_Line (File, Tab_3 & Line);
            end loop;

            Put_Line (File, Tab_1 & ");");
         end if;
      end if;
      New_Line (File);

      Put_Line (File, "   --  An explicit dependency on alire is only needed if you want to compile this file.");
      Put_Line (File, "   --  To do so, include the ""alire.gpr"" project in your own project file.");
      Put_Line (File, "   --  Once you are satisfied with your own dependencies it can be safely removed.");
      New_Line (File);

      Put_Line (File, "end " & Utils.To_Mixed_Case (Pkg_Name) & ";");

      Close (File);
   end Generate_Prj_Alr;

   ----------------------
   -- Generate_Session --
   ----------------------

   procedure Generate_Session (Session_Path : String;
                               Full_Index   : Boolean;
                               Alire_File   : String := "") is
      use Ada.Directories;
      use Alr.OS_Lib;

      File : File_Type;
      Hash : constant String := (if Alire_File /= "" then Utils.Hash_File (Alire_File) else "no-alr-file");
   begin
      Create (File, Out_File, Session_Path / "alr-session.ads");

      Put_Line (File, "pragma Warnings (Off);");
      if Full_Index then
         Generate_Full_Index (File, Hardcoded.Alr_Index_Folder);
         New_Line (File);
      end if;

      --  Depend on the project alr file that does the root registration
      if Alire_File /= "" then
         Put_Line (File, "with " & Utils.To_Mixed_Case (Base_Name (Alire_File)) & ";");
         New_Line (File);
      end if;

      Put_Line (File, "package Alr.Session is");
      New_Line (File);

      Put_Line (File, Tab_1 & "--  This is a generated file. DO NOT EDIT MANUALLY!");
      New_Line (File);

      Put_Line (File, Tab_1 & "Alr_Src_Folder : constant String := """ & Hardcoded.Alr_Src_Folder & """;");
      New_Line (File);

      Put_Line (File, Tab_1 & "Hash : constant String := """ & Hash & """;");
      New_Line (File);

      Put_Line (File, Tab_1 & "Full_Index : constant Boolean := " & Full_Index'Img & ";");
      New_Line (File);

      Put_Line (File, Tab_1 & "Session_Build : constant Boolean := " & Boolean'(Alire_File /= "")'Img & ";");
      New_Line (File);

      Put_Line (File, "end Alr.Session;");

      Close (File);
   end Generate_Session;

end Alr.Templates;
