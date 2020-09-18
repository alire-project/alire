with Ada.Directories;
with Ada.Text_IO;

with Alire.Lockfiles;
with Alire.Milestones;
with Alire.Releases;
with Alire.Roots.Optional;
with Alire.Solutions;
with Alire.Workspace;

with Alr.Platform;
with Alr.Utils;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package body Alr.Commands.Init is

   Sed_Pattern : constant String := "PROJECT_SKEL";

   --------------
   -- Generate --
   --------------

   procedure Generate (Cmd : Command) is

      package TIO renames Ada.Text_IO;

      For_Library : constant Boolean := not Cmd.Bin;
      Name        : constant String := Argument (1);
      Lower_Name  : constant String := Utils.To_Lower_Case (Name);
      Upper_Name  : constant String := Utils.To_Upper_Case (Name);
      Mixed_Name  : constant String := Utils.To_Mixed_Case (Name);

      Directory     : constant Virtual_File :=
        (if Cmd.In_Place
         then Get_Current_Dir
         else Create (+Name, Normalize => True));
      Src_Directory : constant Virtual_File := Directory / "src";

      File : TIO.File_Type;

      procedure Create (Filename : String);
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
         --  Use more than 80 colums for more readable strings
         pragma Style_Checks ("M200");
         Create (Filename);
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
            Put_New_Line;
            Put_Line ("   type Library_Type_Type is " &
                        "(""relocatable"", ""static"", ""static-pic"");");
            Put_Line ("   Library_Type : Library_Type_Type :=");
            Put_Line ("     external (""" & Upper_Name & "_LIBRARY_TYPE"", external (""LIBRARY_TYPE"", ""static""));");
            Put_Line ("   for Library_Kind use Library_Type;");
         else
            Put_Line ("   for Exec_Dir use ""bin"";");
            Put_Line ("   for Main use (""" & Lower_Name & ".adb"");");
         end if;
         Put_New_Line;
         Put_Line ("   type Enabled_Kind is (""enabled"", ""disabled"");");
         Put_Line ("   Compile_Checks : Enabled_Kind := External (""" & Upper_Name & "_COMPILE_CHECKS"", ""enabled"");");
         Put_Line ("   Runtime_Checks : Enabled_Kind := External (""" & Upper_Name & "_RUNTIME_CHECKS"", ""enabled"");");
         Put_Line ("   Style_Checks : Enabled_Kind := External (""" & Upper_Name & "_STYLE_CHECKS"", ""enabled"");");
         Put_Line ("   Contracts_Checks : Enabled_Kind := External (""" & Upper_Name & "_CONTRACTS"", ""enabled"");");
         Put_New_Line;
         Put_Line ("   type Build_Kind is (""debug"", ""optimize"");");
         Put_Line ("   Build_Mode : Build_Kind := External (""" & Upper_Name & "_BUILD_MODE"", ""debug"");");
         Put_New_Line;
         Put_Line ("   Compile_Checks_Switches := ();");
         Put_Line ("   case Compile_Checks is");
         Put_Line ("      when ""enabled"" =>");
         Put_Line ("         Compile_Checks_Switches :=");
         Put_Line ("           (""-gnatwa"",  -- All warnings");
         Put_Line ("            ""-gnatVa"",  -- All validity checks");
         Put_Line ("            ""-gnatwe""); -- Warnings as errors");
         Put_Line ("      when others => null;");
         Put_Line ("   end case;");
         Put_New_Line;
         Put_Line ("   Runtime_Checks_Switches := ();");
         Put_Line ("   case Runtime_Checks is");
         Put_Line ("      when ""enabled"" => null;");
         Put_Line ("      when others =>");
         Put_Line ("         Runtime_Checks_Switches :=");
         Put_Line ("           (""-gnatp""); -- Supress checks");
         Put_Line ("   end case;");
         Put_New_Line;
         Put_Line ("   Style_Checks_Switches := ();");
         Put_Line ("   case Style_Checks is");
         Put_Line ("      when ""enabled"" => null;");
         Put_Line ("         Style_Checks_Switches :=");
         Put_Line ("           (""-gnatyg"",   -- GNAT Style checks");
         Put_Line ("            ""-gnaty-d"",  -- Disable no DOS line terminators");
         Put_Line ("            ""-gnatyM80"", -- Maximum line length");
         Put_Line ("            ""-gnatyO"");  -- Overriding subprograms explicitly marked as such");
         Put_Line ("      when others => null;");
         Put_Line ("   end case;");
         Put_New_Line;
         Put_Line ("   Contracts_Switches := ();");
         Put_Line ("   case Contracts_Checks is");
         Put_Line ("      when ""enabled"" => null;");
         Put_Line ("         Contracts_Switches :=");
         Put_Line ("           (""-gnata""); --  Enable assertions and contracts");
         Put_Line ("      when others =>");
         Put_Line ("   end case;");
         Put_New_Line;
         Put_Line ("   Build_Switches := ();");
         Put_Line ("   case Build_Mode is");
         Put_Line ("      when ""optimize"" =>");
         Put_Line ("         Build_Switches := (""-O3"",     -- Optimization");
         Put_Line ("                            ""-gnatn""); -- Enable inlining");
         Put_Line ("      when ""debug"" =>");
         Put_Line ("         Build_Switches := (""-g"",   -- Debug info");
         Put_Line ("                            ""-Og""); -- No optimization");
         Put_Line ("   end case;");
         Put_New_Line;
         Put_Line ("   package Compiler is");
         Put_Line ("      for Default_Switches (""Ada"") use");
         Put_Line ("        Compile_Checks_Switches &");
         Put_Line ("        Build_Switches &");
         Put_Line ("        Runtime_Checks_Switches &");
         Put_Line ("        Style_Checks_Switches &");
         Put_Line ("        Contracts_Switches &");
         Put_Line ("        (""-gnatQ"");  -- Don't quit. Generate ALI and tree files even if illegalities");
         Put_Line ("   end Compiler;");
         Put_New_Line;
         Put_Line ("   package Binder is");
         Put_Line ("      for Switches (""Ada"") use (""-Es""); --  Symbolic traceback");
         Put_Line ("   end Binder;");
         Put_New_Line;
         TIO.Put (File, "end " & Mixed_Name & ";");
         pragma Style_Checks ("M80");

         TIO.Close (File);
      end Generate_Project_File;

      ---------------------------
      -- Generate_Root_Package --
      ---------------------------

      procedure Generate_Root_Package is
         Filename : constant String :=
            +Full_Name (Src_Directory / (+Lower_Name & ".ads"));
      begin
         Create (Filename);
         Put_Line ("package " & Mixed_Name & " is");
         Put_New_Line;
         TIO.Put (File, "end " & Mixed_Name & ";");
         TIO.Close (File);
      end Generate_Root_Package;

      ---------------------------
      -- Generate_Program_Main --
      ---------------------------

      procedure Generate_Program_Main is
         Filename : constant String :=
            +Full_Name (Src_Directory / (+Lower_Name & ".adb"));
      begin
         Create (Filename);
         Put_Line ("procedure " & Mixed_Name & " is");
         Put_Line ("begin");
         Put_Line ("   null;");
         TIO.Put (File, "end " & Mixed_Name & ";");
         TIO.Close (File);
      end Generate_Program_Main;

      ------------
      -- Create --
      ------------

      procedure Create (Filename : String) is
      begin
         if Ada.Directories.Exists (Filename) then
            Reportaise_Command_Failed
              (Filename & " already exists.");
         end if;

         TIO.Create (File, TIO.Out_File, Filename);
      end Create;
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
      if Cmd.No_Skel then
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
           (+Name,
            Ada.Directories.Full_Name (+Directory.Full_Name),
            Platform.Properties);
      begin
         Make_Dir (Create (+Root.Working_Folder));

         Alire.Workspace.Generate_Manifest (Root.Release, Root);

         Alire.Lockfiles.Write
           ((Solution => Alire.Solutions.Empty_Valid_Solution),
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
      .Append ("--in-place is intended to be used inside the crate directory.")
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
                     "Create alr files in current folder");

      Define_Switch (Config,
                     Cmd.No_Skel'Access,
                     "", "--no-skel",
                     "Do not generate non-alire skeleton files");
   end Setup_Switches;

end Alr.Commands.Init;
