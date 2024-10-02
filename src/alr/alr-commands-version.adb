with Alire.Builds;
with Alire.Cache;
with Alire.Settings.Edit;
with Alire.Directories;
with Alire.Index;
with Alire.Index_On_Disk.Loading;
with Alire.Milestones;
with Alire.Origins.Deployers.System;
with Alire.Paths.Vault;
with Alire.Platforms.Folders;
with Alire.Properties;
with Alire.Roots.Optional;
with Alire.Toolchains;
with Alire.Utils.Tables;
with Alire.Version;

with Alr.Bootstrap;

with GNAT.Compiler_Version;
with GNAT.Source_Info;

with CLIC.User_Input;

package body Alr.Commands.Version is

   use Alire.Directories.Operators; -- "/"

   package GNAT_Version is new GNAT.Compiler_Version;

   package Paths renames Alire.Paths;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use Alire;
      use Alire.Utils;
      use all type Alire.Roots.Optional.States;
      Table : Tables.Table;
      Index_Outcome : Alire.Outcome;
      Indexes : constant Alire.Index_On_Disk.Loading.Set :=
                  Alire.Index_On_Disk.Loading.Find_All
                    (Alire.Settings.Edit.Indexes_Directory, Index_Outcome);
      Root : Alire.Roots.Optional.Root :=
               Alire.Roots.Optional.Search_Root (Alire.Directories.Current);

      Build_Path : constant String :=
                     (if Builds.Sandboxed_Dependencies
                      then
                        (if Root.Is_Valid
                         then Root.Value.Dependencies_Dir
                         else "<workspace>"
                         / Paths.Build_Folder_Inside_Working_Folder
                         / Paths.Cache_Folder_Inside_Working_Folder
                         / Paths.Deps_Folder_Inside_Cache_Folder)
                      else Builds.Path);

      ---------
      -- Add --
      ---------

      procedure Add (Key : String; Val : String := "") is
         use AAA.Strings;
      begin
         if (Key = "" or else Val = "") and then Tables.Structured_Output then
            return; -- Skip cosmetic rows in structured output
         end if;

         if Tables.Structured_Output and then Contains (Key, ":") then
            Add (Replace (Key, ":", ""), Val);
            return;
         end if;

         Table.Append (Key).Append (Val).New_Row;
      end Add;

   begin
      if Args.Count /= 0 then
         Reportaise_Wrong_Arguments (Cmd.Name & " doesn't take arguments");
      end if;

      --  Enrich output when using a structured format only
      if Alire.Utils.Tables.Structured_Output then
         Table.Header ("key").Header ("Value").New_Row;
      end if;

      Add ("APPLICATION", "");
      Add ("alr version:",      Alire.Version.Current.Image);
      Add ("libalire version:", Alire.Version.Current.Image);
      Add ("compilation date:",
           GNAT.Source_Info.Compilation_ISO_Date & " "
           & GNAT.Source_Info.Compilation_Time);
      Add ("compiled with version:", GNAT_Version.Version);

      Add ("");
      Add ("CONFIGURATION");
      Add ("home folder:",     Alire.Platforms.Folders.Home);
      Add ("settings folder:", Alire.Settings.Edit.Path);
      Add ("cache folder:",    Alire.Cache.Path);
      Add ("vault folder:",    Paths.Vault.Path);
      Add ("build folder:",    Build_Path);
      Add ("temp folder:",     Alire.Platforms.Folders.Temp);
      Add ("force flag:",      Alire.Force'Image);
      Add ("non-interactive flag:",
           CLIC.User_Input.Not_Interactive'Image);
      Add ("community index branch:", Alire.Index.Community_Branch);
      Add ("compatible index versions:",
           Alire.Index.Valid_Versions.Image);
      Add ("indexes folder:",
           Alire.Settings.Edit.Indexes_Directory);
      Add ("indexes metadata:",
           (if Index_Outcome.Success
            then "OK"
            else "ERROR: " & Index_Outcome.Message));
      for Index of Indexes loop
         Add ("index #"
              & AAA.Strings.Trim (Index.Priority'Image) & ":",
              "(" & Index.Name & ") " & Index.Origin);
      end loop;
      Add ("toolchain folder:", Alire.Toolchains.Path);
      Add ("toolchain assistant:",
           (if Alire.Toolchains.Assistant_Enabled
            then "enabled"
            else "disabled"));
      declare
         I : Positive := 1;
      begin
         for Tool of Alire.Toolchains.Tools loop
            Add ("tool #" & AAA.Strings.Trim (I'Image)
                 & " " & Tool.As_String & ":",
                 (if Alire.Toolchains.Tool_Is_Configured (Tool)
                  then Alire.Toolchains.Tool_Milestone (Tool).Image
                  else "not configured"));
            I := I + 1;
         end loop;
      end;

      declare
         System_Manager : constant String :=
                            Origins.Deployers.System.Executable_Path;
      begin
         Add ("system package manager:",
              (if System_Manager /= ""
               then System_Manager
               else "not found: "
               & (if Origins.Deployers.System.Executable_Name /= ""
                  then "`" & Origins.Deployers.System.Executable_Name & "`"
                  else "unknown package manager")));
         Add ("distro detection disabled:",
              Platforms.Current.Disable_Distribution_Detection'Image);
      end;

      Add ("");
      Add ("WORKSPACE");
      Add ("root status:", Root.Status'Image);
      Add ("root release:",
           (case Root.Status is
               when Valid  => Root.Value.Release.Milestone.Image,
               when others => "N/A"));
      Add ("root load error:",
           (case Root.Status is
               when Broken  => Cmd.Optional_Root.Message,
               when Valid   => "none",
               when Outside => "N/A"));
      Add ("root folder:",
           (case Root.Status is
               when Outside => "N/A",
               when Broken  => "N/A",
               when Valid   => Root.Value.Path));
      Add ("current folder:", Alire.Directories.Current);

      Add ("");
      Add ("SYSTEM");
      for Prop of Platform.Properties loop
         Add (Prop.Key & ":", Prop.Image);
      end loop;

      Table.Print (Level => Always);
   exception
      when E : others =>
         Alire.Log_Exception (E);
         Trace.Error ("Unexpected error during information gathering");
         Trace.Error ("Gathered information up to the error is:");
         Table.Print (Level => Always);
         raise;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
      .Append ("Shows assorted metadata about the alr executable,"
               & " and about the crate or sandbox found in the current"
               & " directory, if any."));

   -------------------
   -- Print_Version --
   -------------------

   procedure Print_Version is
   begin
      Trace.Always ("alr " & Alire.Version.Current.Image);
   end Print_Version;

end Alr.Commands.Version;
