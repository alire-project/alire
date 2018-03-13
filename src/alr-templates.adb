with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Alire.GPR;
with Alire.Index;
with Alire.Projects;           use all type Alire.Projects.Names;
with Alire.Properties.Labeled; use all type Alire.Properties.Labeled.Labels;
with Alire.Properties.Scenarios;

with Alr.Commands;
with Alr.Commands.Withing;
with Alr.Files;
with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Platform;
with Alr.Utils;

with GNAT.OS_Lib;

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
                  Query.Resolve (Root.Dependencies.Evaluate (Query.Platform_Properties),
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
      Filename : constant String := Hardcoded.Build_File (Root.Name);
      Prjname  : constant String := Utils.To_Mixed_Case (Filename (Filename'First .. Filename'Last - 4));

      First    : Boolean := True;

      use Alr.OS_Lib;

      GPR_Files : Utils.String_Vector;
      All_Paths : Utils.String_Vector;
   begin
      if Root.Is_Released then
         GPR_Files := Root.Release.GPR_Files (Query.Platform_Properties);
         Log ("Generating GPR for release " & Root.Release.Milestone.Image &
                " with" & Instance.Length'Img & " dependencies", Detail);
      else
         GPR_Files.Append (Root.Name & ".gpr");
         Log ("Generating GPR for unreleased project " & Root.Name & " with" &
                Instance.Length'Img & " dependencies", Detail);
      end if;

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
      Put_Line (File, ");");
      New_Line (File);

      --  First obtain all paths and then output them, if any needed
      for Rel of Instance loop
         if Rel.Project = Root.Name then
            --  All_Paths.Append (".");
            null; -- That's the first path in aggregate projects anyway
         else
            All_Paths.Append (Hardcoded.Projects_Folder / Rel.Unique_Folder);
         end if;

         --  Add non-root extra project paths, always
         for Path of Rel.Labeled_Properties (Platform.Properties, GPR_Path) loop
            if GNAT.OS_Lib.Is_Absolute_Path (Path) then
               All_Paths.Append (Path);
            else
               All_Paths.Append ((if Rel.Project = Root.Name
                                 then "."
                                 else Hardcoded.Projects_Folder / Rel.Unique_Folder) &
                                   GNAT.OS_Lib.Directory_Separator & Path);
               --  Path won't be a simple name and / (compose) would complain
            end if;
         end loop;
      end loop;

      if not All_Paths.Is_Empty then
         Put (File, Tab_1 & "for Project_Path use (");

         for Path of All_Paths loop
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
      for Release of Instance loop
         for Prop of Release.On_Platform_Properties (Platform.Properties) loop
            if Prop in Alire.Properties.Scenarios.Property'Class then
               declare
                  use all type Alire.GPR.Variable_Kinds;
                  Variable : constant Alire.GPR.Variable :=
                               Alire.Properties.Scenarios.Property (Prop).Value;
               begin
                  if Variable.Kind = External then
                     Put_Line (File,
                               Tab_1 & "for External (" &
                                 Q (Variable.Name) & ") use " & Q (Variable.External_Value) & ";");
                  end if;
               end;
            end if;
         end loop;
      end loop;
      New_Line;

      Put_Line (File, Tab_1 & "for external (""ALIRE"") use ""True"";");
      New_Line (File);

      Put_Line (File, "end " & Prjname & ";");

      Close (File);
   end Generate_Agg_Gpr;

   ----------------------------
   -- Generate_Project_Alire --
   ----------------------------

   procedure Generate_Prj_Alr (Instance : Query.Instance;
                               Root     : Alire.Roots.Root;
                               Scenario : Generation_Scenarios;
                               Filename : String := "")
   is
      package Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

      Includes : Sets.Set; -- To sort them and remove duplicates

      File : File_Type;
      Name : constant String := (if Filename /= ""
                                 then Filename
                                 else Hardcoded.Alire_File (Root.Name));
   begin
      if Root.Is_Released and then Instance.Contains (Root.Release.Name) then
         declare
            Pruned_Instance : Query.Instance := Instance;
         begin
            Pruned_Instance.Delete (Root.Release.Name);
            Generate_Prj_Alr (Pruned_Instance, Root, Scenario, Filename);
            return;
         end;
      end if;

      Files.Backup_If_Existing (Name);

      Create (File, Out_File, Name);

      Put_Line (File, "with Alire.Index; use Alire.Index;");

      if Root.Is_Released and then Scenario /= Pinning then
         Put_Line (File, Commands.Withing.With_Line (Root.Release.Name));
         --  Root dependency that will pull everything else in
      elsif Scenario = Initial then
         Put_Line (File, "with Alire.Index.Alire;");
      else
         -- Err on the safe side and pull in all dependencies
         for R of Instance loop
            Includes.Include (Commands.Withing.With_Line (R.Name));
         end loop;

         for Inc of Includes loop
            Put_Line (File, Inc);
         end loop;
      end if;
      New_Line (File);

      Put_Line (File, "package " & Utils.To_Mixed_Case (Root.Name) & "_Alr is");
      New_Line (File);
      Put_Line (File, Tab_1 & "Current_Root : constant Root := Set_Root (");

      if Root.Is_Released and then Scenario /= Pinning then
         --  Typed name plus version
         Put_Line (File, Tab_2 & Alire.Index.Get (Root.Release.Name).Callable_String & ",");
         Put_Line (File, Tab_2 & "V (" & Q (Semver.Image (Root.Release.Version)) & "));");
      else
         --  Untyped name plus dependencies
         Put_Line (File, Tab_2 & Q (Root.Name) & ",");

         if Scenario = Initial then
            Put_Line (File, Tab_2 & "Dependencies => Alire.Index.Alire.Project.Current);");
         else
            if Instance.Is_Empty then
               Put_Line (File, Tab_2 & "Dependencies => No_Dependencies);");
            else
               Put (File, Tab_2 & "Dependencies =>");

               declare
                  First : Boolean := True;
               begin
                  for Rel of Instance loop
                     if not First then
                        Put_Line (File, " and");
                     else
                        New_Line (File);
                     end if;
                     Put (File, Tab_3 &
                            Alire.Index.Get (Rel.Name).Callable_String &
                            (if Scenario = Pinning
                             then ".At_Version ("
                             else ".Within_Major (") &
                             Q (Semver.Image (Rel.Version)) & ")");
                     First := False;
                  end loop;
               end;

               Put_Line (File, ");");
            end if;
         end if;
      end if;
      New_Line (File);

      Put_Line (File, "   --  An explicit dependency on alire is only needed if you want to compile this file.");
      Put_Line (File, "   --  To do so, include the ""alire.gpr"" project in your own project file.");
      Put_Line (File, "   --  Once you are satisfied with your own dependencies it can be safely removed.");
      New_Line (File);

      Put_Line (File, "end " & Utils.To_Mixed_Case (Root.Name) & "_Alr;");

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
         Generate_Full_Index (File, Hardcoded.Alr_Index_Folder_Absolute);
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

      Put_Line (File, Tab_1 & "Alr_Src_Folder : aliased String := """ & Hardcoded.Alr_Src_Folder & """ with Volatile;");
      New_Line (File);

      Put_Line (File, Tab_1 & "Hash : aliased String := """ & Hash & """ with Volatile;");
      New_Line (File);

      Put_Line (File, Tab_1 & "Full_Index : aliased Boolean := " & Full_Index'Img & " with Volatile;");
      New_Line (File);

      Put_Line (File, Tab_1 & "Session_Build : aliased Boolean := " & Boolean'(Alire_File /= "")'Img & " with Volatile;");
      New_Line (File);

      Put_Line (File, "end Alr.Session;");

--        Alr_Src_Folder : constant String  := "";
--        --  For alr instances that are session specific, we need a way to locate the src folder
--        --    (just for the case where it is not the canonical one, that is: while developing)
--
--        Hash           : constant String := "bootstrap";
--        --  In the curren per-session setup, this should always match unless the dependencies files has been
--        --    tampered with in such a way that its timestamp has not been updated
--
--        Full_Index     : constant Boolean := False;
--        --  Some commands require a full index and some others not.
--        --  We use this to separate bootstrap from index status
--
--        Session_Build  : constant Boolean := False;

      Close (File);
   end Generate_Session;

end Alr.Templates;
