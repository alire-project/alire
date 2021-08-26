with AAA.Text_IO;

with Ada.Directories;
with Ada.Text_IO;

with Alire.Config;
with Alire.Lockfiles;
with Alire.Paths;
with Alire.Solutions;
with Alire.Utils.TTY;
with Alire.Utils.User_Input.Query_Config;

with Alr.Utils;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with TOML;

package body Alr.Commands.Init is

   package TTY renames Alire.Utils.TTY;
   package UI renames Alire.Utils.User_Input;

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
      Config_Directory : constant Virtual_File := Directory / "config";

      File : TIO.File_Type;

      function Create (Filename : String) return Boolean;
      --  Return False if the file already exists

      procedure Put_New_Line;
      procedure Put_Line (S : String);
      --  Shortcuts to write to File

      function Escape (S : String) return String
      --  We trick the TOML exporter to get a valid escaped string
      is
         use Alire.Utils;
         use TOML;
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("key", TOML.Create_String (S));

         --  Remove excess whitespace and quotation
         return
           Trim
             (Trim
                (Trim (Tail (TOML.Dump_As_String (Table), '=')),
                 ASCII.LF),
              '"');
      end Escape;

      function Q (S : String) return String is ("""" & S & """");
      --  Quote string

      function Arr (S : String) return String is ("[" & S & "]");
      --  Wrap string into TOML array

      procedure Generate_Project_File;
      --  Generate a project file for this crate

      procedure Generate_Root_Package;
      --  Generate the specification for the root package of this crate

      procedure Generate_Program_Main;
      --  Generate the procedure body for the program main of this crate

      procedure Generate_Gitignore;
      --  Generate or append .gitignore

      procedure Generate_Manifest;
      --  Generates the initial manifest by hand. This is more legible than
      --  exporting using To_TOML functions. We still use TOML encoding for
      --  the generated strings to be on the safe side.

      ---------------------------
      -- Generate_Project_File --
      ---------------------------

      procedure Generate_Project_File is
         Filename : constant String :=
            +Full_Name (Directory / (+Lower_Name & ".gpr"));

         Config_Filename : constant String :=
            +Full_Name (Config_Directory / (+Lower_Name & "_config.gpr"));
      begin
         --  Use more than 80 colums for more readable strings
         pragma Style_Checks ("M200");

         --  Config project file
         if not Create (Config_Filename) then
            Trace.Warning ("Cannot create '" & Config_Filename & "'");
            return;
         end if;
         Put_Line ("abstract project " & Mixed_Name & "_Config is");
         Put_Line ("   Crate_Version := ""0.0.0"";");
         Put_Line ("end " & Mixed_Name & "_Config;");
         TIO.Close (File);

         --  Main project file
         if not Create (Filename) then
            Trace.Warning ("Cannot create '" & Filename & "'");
            return;
         end if;
         Put_Line ("with ""config/" & Lower_Name & "_config.gpr"";");
         Put_Line ("project " & Mixed_Name & " is");
         Put_New_Line;
         if For_Library then
            Put_Line ("   for Library_Name use """ & Mixed_Name & """;");
            Put_Line ("   for Library_Version use Project'Library_Name & "".so."" & " & Mixed_Name & "_Config.Crate_Version;");
            Put_New_Line;
         end if;
         Put_Line ("   for Source_Dirs use (""src"");");
         Put_Line ("   for Object_Dir use ""obj"";");
         Put_Line ("   for Create_Missing_Dirs use ""True"";");
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
         Put_Line ("   Compile_Checks : Enabled_Kind := External (""" & Upper_Name & "_COMPILE_CHECKS"", ""disabled"");");
         Put_Line ("   Runtime_Checks : Enabled_Kind := External (""" & Upper_Name & "_RUNTIME_CHECKS"", ""disabled"");");
         Put_Line ("   Style_Checks : Enabled_Kind := External (""" & Upper_Name & "_STYLE_CHECKS"", ""disabled"");");
         Put_Line ("   Contracts_Checks : Enabled_Kind := External (""" & Upper_Name & "_CONTRACTS"", ""disabled"");");
         Put_New_Line;
         Put_Line ("   type Build_Kind is (""debug"", ""optimize"");");
         Put_Line ("   Build_Mode : Build_Kind := External (""" & Upper_Name & "_BUILD_MODE"", ""optimize"");");
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
         Put_Line ("      when ""enabled"" =>");
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
         Put_Line ("      when ""enabled"" =>");
         Put_Line ("         Contracts_Switches :=");
         Put_Line ("           (""-gnata""); --  Enable assertions and contracts");
         Put_Line ("      when others => null;");
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
         Put_Line ("        (""-gnatw.X"", -- Disable warnings for No_Exception_Propagation");
         Put_Line ("         ""-gnatQ"");  -- Don't quit. Generate ALI and tree files even if illegalities");
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
         if not Create (Filename) then
            return;
         end if;
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
         if not Create (Filename) then
            return;
         end if;
         Put_Line ("procedure " & Mixed_Name & " is");
         Put_Line ("begin");
         Put_Line ("   null;");
         TIO.Put (File, "end " & Mixed_Name & ";");
         TIO.Close (File);
      end Generate_Program_Main;

      ------------------------
      -- Generate_Gitignore --
      ------------------------

      procedure Generate_Gitignore is
         Filename : constant String :=
            +Full_Name (Directory / ".gitignore");
      begin
         if Ada.Directories.Exists (Filename) then
            TIO.Open (File, TIO.Append_File, Filename);
         else
            TIO.Create (File, TIO.Out_File, Filename);
         end if;

         Put_Line ("obj/");
         Put_Line ("lib/");
         Put_Line ("alire/");
         Put_Line ("config/");
         TIO.Close (File);
      end Generate_Gitignore;

      -----------------------
      -- Generate_Manifest --
      -----------------------

      procedure Generate_Manifest is
         use Alire.Config;
      begin
         if not Defined (Keys.User_Email) or else
           not Defined (Keys.User_Name) or else
           not Defined (Keys.User_Github_Login)
         then
            AAA.Text_IO.Put_Paragraph
              ("Alire needs some user information to initialize the crate"
               & " author and maintainer, for eventual submission to"
               & " the Alire community index. This information will be"
               & " interactively requested now.");
            TIO.New_Line;
            TIO.Put_Line
              ("You can edit this information at any time with 'alr config'");
            TIO.New_Line;
         end if;

         declare
            --  Retrieve initial values from config or user. Only the name may
            --  require encoding, as emails and logins cannot contain strange
            --  characters.
            Login    : constant String := UI.Query_Config.User_GitHub_Login;
            Username : constant String := Escape (UI.Query_Config.User_Name);
            Email    : constant String := UI.Query_Config.User_Email;
            Filename : constant String :=
              +Full_Name (Directory / (+Alire.Roots.Crate_File_Name));
         begin
            if not Create (Filename) then
               Reportaise_Command_Failed ("Cannot create '" & Filename & "'");
            end if;
            Put_Line ("name = " & Q (Lower_Name));
            Put_Line ("description = " & Q ("Shiny new project"));
            Put_Line ("version = " & Q ("0.0.0"));
            Put_New_Line;
            Put_Line ("authors = " & Arr (Q (Username)));
            Put_Line ("maintainers = "
                      & Arr (Q (Username & " <" & Email & ">")));
            Put_Line ("maintainers-logins = " & Arr (Q (Login)));
         end;

         if Cmd.Bin then
            Put_New_Line;
            Put_Line ("executables = " & Arr (Q (Lower_Name)));
         end if;

         TIO.Close (File);
      end Generate_Manifest;

      ------------
      -- Create --
      ------------

      function Create (Filename : String) return Boolean is
      begin
         if Ada.Directories.Exists (Filename) then
            Trace.Warning (Filename & " already exists.");
            return False;
         end if;

         TIO.Create (File, TIO.Out_File, Filename);

         return True;
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
      --  Crate dir
      Directory.Make_Dir;

      --  Empty alire dir
      Virtual_File'(Directory / (+Alire.Paths.Working_Folder_Inside_Root))
        .Make_Dir;

      if not Cmd.No_Skel then
         Config_Directory.Make_Dir;
         Generate_Project_File;
         Src_Directory.Make_Dir;
         if For_Library then
            Generate_Root_Package;
         else
            Generate_Program_Main;
         end if;
         Generate_Gitignore;
      end if;

      Generate_Manifest;

      Alire.Lockfiles.Write
        ((Solution => Alire.Solutions.Empty_Valid_Solution),
         Alire.Lockfiles.File_Name
           (String (Filesystem_String'(Directory.Full_Name))));

      Alire.Put_Success (TTY.Emph (Lower_Name) & " initialized successfully.");
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
         Check : constant Alire.Crate_Name := +Name with Unreferenced;
      begin
         if Utils.To_Lower_Case (Name) = Utils.To_Lower_Case (Sed_Pattern)
         then
            Reportaise_Command_Failed
              ("The crate name is invalid, as it is used internally by"
               & " alr; please choose another name");
         end if;

         Generate (Cmd);
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
