with AAA.Text_IO;

with Ada.Directories;
with Ada.Text_IO;

with Alire.Config;
with Alire.Utils.User_Input.Query_Config;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with TOML;

package body Alr.Commands.Init is

   package UI renames Alire.Utils.User_Input;

   Sed_Pattern : constant String := "PROJECT_SKEL";

   --------------
   -- Generate --
   --------------

   procedure Generate (Cmd  : Command;
                       Args : AAA.Strings.Vector) is

      package TIO renames Ada.Text_IO;
      use AAA.Strings;

      For_Library : constant Boolean := not Cmd.Bin;
      Name        : constant String := Args (1);
      Lower_Name  : constant String := AAA.Strings.To_Lower_Case (Name);
      Upper_Name  : constant String := AAA.Strings.To_Upper_Case (Name);
      Mixed_Name  : constant String := AAA.Strings.To_Mixed_Case (Name);

      Directory     : constant Virtual_File :=
        (if Cmd.In_Place
         then Get_Current_Dir
         else Create (+Name, Normalize => True));
      Src_Directory : constant Virtual_File := Directory / "src";

      File : TIO.File_Type;

      function Create (Filename : String) return Boolean;
      --  Return False if the file already exists

      procedure Put_New_Line;
      procedure Put_Line (S : String);
      --  Shortcuts to write to File

      function Escape (S : String) return String
      --  We trick the TOML exporter to get a valid escaped string
      is
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
      begin
         --  Use more than 80 colums for more readable strings
         pragma Style_Checks ("M200");

         --  --  Config project file
         --  if not Create (Config_Filename) then
         --     Trace.Warning ("Cannot create '" & Config_Filename & "'");
         --     return;
         --  end if;
         --  Put_Line ("abstract project " & Mixed_Name & "_Config is");
         --  Put_Line ("   Crate_Version := ""0.0.0"";");
         --  Put_Line ("   Ada_Compiler_Switches := " &
         --              "External_As_List (""ADAFLAGS"", "" "");");
         --
         --  TIO.Put (File, "end " & Mixed_Name & "_Config;");
         --  TIO.Close (File);

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
         Put_Line ("   package Compiler is");
         Put_Line ("      for Default_Switches (""Ada"") use " & Mixed_Name & "_Config.Ada_Compiler_Switches;");
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

         Put_Line ("/obj/");
         if For_Library then
            Put_Line ("/lib/");
         else
            Put_Line ("/bin/");
         end if;
         Put_Line ("/alire/");
         Put_Line ("/config/");
         TIO.Close (File);
      end Generate_Gitignore;

      -----------------------
      -- Generate_Manifest --
      -----------------------

      procedure Generate_Manifest is
         use Alire.Config;
      begin
         if not DB.Defined (Keys.User_Email) or else
           not DB.Defined (Keys.User_Name) or else
           not DB.Defined (Keys.User_Github_Login)
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

      if not Cmd.No_Skel then
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

      Alire.Put_Success (TTY.Emph (Lower_Name) & " initialized successfully.");
   end Generate;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use AAA.Strings;
   begin
      if Args.Count /= 1 then
         Reportaise_Wrong_Arguments ("No crate name given");
      end if;

      if not (Cmd.Bin or Cmd.Lib) then
         Reportaise_Wrong_Arguments ("Please provide either --bin or --lib");
      end if;

      --  Validation finished

      declare
         Name  : constant String := Args (1);
         Check : constant Alire.Crate_Name := +Name with Unreferenced;
      begin
         if To_Lower_Case (Name) = To_Lower_Case (Sed_Pattern)
         then
            Reportaise_Command_Failed
              ("The crate name is invalid, as it is used internally by"
               & " alr; please choose another name");
         end if;

         Generate (Cmd, Args);
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
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

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
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
