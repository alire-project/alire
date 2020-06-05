with Ada.Directories;
with Ada.Text_IO;

with Alire.Lockfiles;
with Alire.Milestones;
with Alire.Origins;
with Alire.Releases;
with Alire.Roots;
with Alire.Solutions;
with Alire.Workspace;

with Alr.Root;
with Alr.Utils;

with GNATCOLL.VFS;
with Alr.Bootstrap;

package body Alr.Commands.Init is

   use all type Bootstrap.Session_States;

   Sed_Pattern : constant String := "PROJECT_SKEL";

   --------------
   -- Generate --
   --------------

   procedure Generate (Cmd : Command) is
      use GNATCOLL.VFS;

      package TIO renames Ada.Text_IO;

      For_Library : constant Boolean := not Cmd.Bin;
      Name        : constant String := Argument (1);
      Lower_Name  : constant String := Utils.To_Lower_Case (Name);
      Mixed_Name  : constant String := Utils.To_Mixed_Case (Name);

      Directory     : constant Virtual_File :=
        (if Cmd.In_Place
         then Get_Current_Dir
         else Create (+Name, Normalize => True));
      Src_Directory : constant Virtual_File := Directory / "src";

      File : TIO.File_Type;

      procedure Put_New_Line;
      procedure Put_Line (S : String);
      --  Shortcuts to write to File

      procedure Generate_Project_File;
      --  Generate a project file for this crate

      procedure Generate_Root_Package;
      --  Generate the specification for the root package of this crate

      procedure Generate_Program_Main;
      --  Generate the procedure body for the program main of this crate

      ---------------------------
      -- Generate_Project_File --
      ---------------------------

      procedure Generate_Project_File is
         Filename : constant String :=
            +Full_Name (Directory / (+Lower_Name & ".gpr"));
      begin
         TIO.Create (File, TIO.Out_File, Filename);
         Put_Line ("project " & Mixed_Name & " is");
         Put_New_Line;
         if For_Library then
            Put_Line ("   for Library_Name use """ & Mixed_Name & """;");
            Put_Line ("   for Library_Version use ""0.0.0"";");
            Put_New_Line;
         end if;
         Put_Line ("   for Source_Dirs use (""src"");");
         Put_Line ("   for Object_Dir use ""obj"";");
         if For_Library then
            Put_Line ("   for Library_Dir use ""lib"";");
         else
            Put_Line ("   for Exec_Dir use ""bin"";");
            Put_Line ("   for Main use (""" & Lower_Name & ".adb"");");
         end if;
         Put_New_Line;
         Put_Line ("   package Builder is");
         Put_Line ("      for Switches (""ada"") use (""-j0"", ""-g"");");
         Put_Line ("   end Builder;");
         Put_New_Line;
         Put_Line ("   package Compiler is");
         Put_Line ("      for Switches (""ada"") use");
         Put_Line ("        (""-gnatVa"", ""-gnatwa"", ""-g"", ""-O2"",");
         Put_Line ("         ""-gnata"", ""-gnato"", ""-fstack-check"");");
         Put_Line ("   end Compiler;");
         Put_New_Line;
         Put_Line ("   package Binder is");
         Put_Line ("      for Switches (""ada"") use (""-Es"");");
         Put_Line ("   end Binder;");
         Put_New_Line;
         Put_Line ("end " & Mixed_Name & ";");
         TIO.Close (File);
      end Generate_Project_File;

      ---------------------------
      -- Generate_Root_Package --
      ---------------------------

      procedure Generate_Root_Package is
         Filename : constant String :=
            +Full_Name (Src_Directory / (+Lower_Name & ".ads"));
      begin
         TIO.Create (File, TIO.Out_File, Filename);
         Put_Line ("package " & Mixed_Name & " is");
         Put_New_Line;
         Put_Line ("end " & Mixed_Name & ";");
         TIO.Close (File);
      end Generate_Root_Package;

      ---------------------------
      -- Generate_Program_Main --
      ---------------------------

      procedure Generate_Program_Main is
         Filename : constant String :=
            +Full_Name (Src_Directory / (+Lower_Name & ".adb"));
      begin
         TIO.Create (File, TIO.Out_File, Filename);
         Put_Line ("procedure " & Mixed_Name & " is");
         Put_Line ("begin");
         Put_Line ("   null;");
         Put_Line ("end " & Mixed_Name & ";");
         TIO.Close (File);
      end Generate_Program_Main;

      ------------------
      -- Put_New_Line --
      ------------------

      procedure Put_New_Line is
      begin
         TIO.New_Line (File);
      end Put_New_Line;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (S : String) is
      begin
         TIO.Put_Line (File, S);
      end Put_Line;

   begin
      if Cmd.In_Place then
         null; -- do nothing

      elsif Cmd.No_Skel then
         Directory.Make_Dir;

      else
         Directory.Make_Dir;
         Generate_Project_File;
         Src_Directory.Make_Dir;
         if For_Library then
            Generate_Root_Package;
         else
            Generate_Program_Main;
         end if;
      end if;

      declare
         Root : constant Alire.Roots.Root := Alire.Roots.New_Root
           (+Name, Ada.Directories.Full_Name (+Directory.Full_Name));
      begin
         Make_Dir (Create (+Root.Working_Folder));

         Alire.Workspace.Generate_Manifest (Root.Release, Root);

         Alire.Lockfiles.Write
           (Alire.Solutions.Empty_Valid_Solution,
            Root.Lock_File);
      end;
   end Generate;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Num_Arguments /= 1 then
         Trace.Error ("No crate name given");
         raise Wrong_Command_Arguments;
      end if;

      if not (Cmd.Bin or Cmd.Lib) then
         Log ("Please provide either --bin or --lib");
         raise Command_Failed;
      end if;

      if Cmd.In_Place then
         Cmd.No_Skel := True;
      end if;

      --  Validation finished

      declare
         Name  : constant String := Argument (1);
         Check : constant Alire.Milestones.Allowed_Milestones :=
                   Alire.Milestones.Crate_Versions (Name)
                   with Unreferenced;
      begin
         if Utils.To_Lower_Case (Name) = Utils.To_Lower_Case (Sed_Pattern)
         then
            Reportaise_Command_Failed
              ("The crate name is invalid, as it is used internally by"
               & " alr; please choose another name");
         end if;

         if not Cmd.In_Place and then Ada.Directories.Exists (Name) then
            Log ("Folder " & Utils.Quote (Name)
                 & " already exists, not proceeding.");
            raise Command_Failed;
         end if;

         --  Create and enter folder for generation, if it didn't happen
         --  already.
         if Session_State = Release then
            if Name = Root.Current.Release.Name_Str then
               Trace.Info ("Already in working copy, skipping initialization");
            else
               Trace.Error ("Cannot initialize a working release inside"
                            & " another release, stopping.");
               raise Command_Failed;
            end if;
         end if;

         Generate (Cmd);
         Trace.Detail ("Initialization completed");
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Initializes a new crate containing a ready-to-build GNAT"
               & " project. The crate is created as a child of the current"
               & " directory, containing minimal sources for an executable"
               & " or library, as specified.")
      .New_Line
      .Append ("--in-place is intended to be used inside the crate directory"
               & " to regenerate alire metadata files, if for some reason"
               & " they become missing or invalid.")
     );

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Bin'Access,
                     "", "--bin",
                     "New project is an executable");

      Define_Switch (Config,
                     Cmd.Lib'Access,
                     "", "--lib",
                     "New project is a library");

      Define_Switch (Config,
                     Cmd.In_Place'Access,
                     "", "--in-place",
                     "Create alr files in current folder (implies --no-skel)");

      Define_Switch (Config,
                     Cmd.No_Skel'Access,
                     "", "--no-skel",
                     "Do not generate non-alire skeleton files");
   end Setup_Switches;

end Alr.Commands.Init;
