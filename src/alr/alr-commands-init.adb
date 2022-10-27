with AAA.Text_IO;

with Ada.Directories;
with Ada.Wide_Wide_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alire.Config;
with Alire.Utils.User_Input.Query_Config;
with CLIC.User_Input;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with TOML;

with SPDX;
with CLIC.TTY; use CLIC.TTY;

package body Alr.Commands.Init is

   package UI renames Alire.Utils.User_Input;

   type Crate_Kind is (Library, Binary);

   type Crate_Init_Info is record
      Name         : Unbounded_String;
      Kind         : Crate_Kind := Library;
      GitHub_Login : Unbounded_String;
      Username     : Unbounded_String;
      Email        : Unbounded_String;
      Licenses     : Unbounded_String;
      Description  : Unbounded_String;
      Website      : Unbounded_String;
      Tags         : AAA.Strings.Vector;
   end record;

   --------------
   -- Generate --
   --------------

   procedure Generate (Cmd  : Command;
                       Info : Crate_Init_Info)
   is

      package TIO renames Ada.Wide_Wide_Text_IO;
      use AAA.Strings;

      For_Library : constant Boolean := Info.Kind = Library;
      Name        : constant String := To_String (Info.Name);
      Lower_Name  : constant String := AAA.Strings.To_Lower_Case (Name);
      Upper_Name  : constant String := AAA.Strings.To_Upper_Case (Name);
      Mixed_Name  : constant String := AAA.Strings.To_Mixed_Case (Name);

      Directory     : constant Virtual_File :=
        (if Cmd.In_Place
         then Get_Current_Dir
         else Create (+Name, Normalize => True));
      Src_Directory : constant Virtual_File := Directory / "src";
      Share_Directory : constant Virtual_File :=
         Directory / "share" / Filesystem_String (Lower_Name);

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

      function Q (S : Unbounded_String) return String
      is (Q (To_String (S)));
      --  Quote string

      function Arr (S : String) return String is ("[" & S & "]");
      --  Wrap string into TOML array

      function Q_Arr (Arr : AAA.Strings.Vector) return String
      is (if Arr.Is_Empty
          then "[]"
          else "[""" & Arr.Flatten (""", """) & """]");
      --  String vector to TOML array of strings

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
         --  Use more than 80 columns for more readable strings
         pragma Style_Checks ("M200");

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
         Put_Line ("   for Source_Dirs use (""src/"", ""config/"");");
         Put_Line ("   for Object_Dir use ""obj/"" & " & Mixed_Name & "_Config.Build_Profile;");
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
         Put_Line ("   package Install is");
         Put_Line ("      for Artifacts (""."") use (""share"");");
         Put_Line ("   end Install;");
         Put_New_Line;
         TIO.Put (File, WW ("end " & Mixed_Name & ";"));
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
         TIO.Put (File, WW ("end " & Mixed_Name & ";"));
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
         TIO.Put (File, WW ("end " & Mixed_Name & ";"));
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
            Login    : constant String := To_String (Info.GitHub_Login);
            Username : constant String := Escape (To_String (Info.Username));
            Email    : constant String := To_String (Info.Email);
            Filename : constant String :=
              +Full_Name (Directory / (+Alire.Roots.Crate_File_Name));
         begin
            if not Create (Filename) then
               Reportaise_Command_Failed ("Cannot create '" & Filename & "'");
            end if;
            Put_Line ("name = " & Q (Lower_Name));
            Put_Line ("description = " & Q (Info.Description));
            Put_Line ("version = " & Q ("0.1.0-dev"));
            Put_New_Line;
            Put_Line ("authors = " & Arr (Q (Username)));
            Put_Line ("maintainers = "
                      & Arr (Q (Username & " <" & Email & ">")));
            Put_Line ("maintainers-logins = " & Arr (Q (Login)));
            Put_Line ("licenses = " & Q (Info.Licenses));
            Put_Line ("website = " & Q (Info.Website));
            Put_Line ("tags = " & Q_Arr (Info.Tags));
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
         TIO.Put_Line (File, WW (S));
      end Put_Line;

   begin
      --  Crate dir
      Directory.Make_Dir;

      if not Cmd.No_Skel then
         Generate_Project_File;
         Src_Directory.Make_Dir;
         Share_Directory.Make_Dir;
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

   ----------------------
   -- Query_Crate_Name --
   ----------------------

   procedure Query_Crate_Name (Args :        AAA.Strings.Vector;
                               Info : in out Crate_Init_Info)
   is
   begin
      case Args.Length is
         when 0 => -- Query crate name
            loop
               declare
                  Tentative_Name : constant String :=
                    CLIC.User_Input.Query_String
                      (Question   => "Crate name?",
                       Default    => "",
                       Validation => null);
               begin
                  if Alire.Is_Valid_Name (Tentative_Name) then
                     Info.Name := To_Unbounded_String (Tentative_Name);
                     exit;
                  else
                     Ada.Text_IO.Put_Line
                       ("Invalid crate name '"
                        & Tentative_Name & "': "
                        & Alire.Error_In_Name (Tentative_Name));
                  end if;
               end;
            end loop;

         when 1 => -- Use crate name from argument
            declare
               Arg_Name : constant String := Args.First_Element;
            begin
               if not Alire.Is_Valid_Name (Arg_Name) then
                  Reportaise_Wrong_Arguments
                    ("Invalid crate name '"
                     & Arg_Name & "': "
                     & Alire.Error_In_Name (Arg_Name));
               else
                  Info.Name := To_Unbounded_String (Args.First_Element);
               end if;
            end;

         when others =>
            Reportaise_Wrong_Arguments ("'init' takes at most one argument");
      end case;
   end Query_Crate_Name;

   ------------------------
   -- License_Validation --
   ------------------------

   function License_Validation (Str : String) return Boolean is
      SP : constant SPDX.Expression := SPDX.Parse (Str,
                                                   Allow_Custom => True);
   begin
      if SPDX.Valid (SP) then
         return True;
      else
         Ada.Text_IO.Put_Line
           ("Invalid SPDX license expression '" & Str
            & "': " & SPDX.Error (SP));
         Ada.Text_IO.Put_Line
           ("SPDX expression expected (https://spdx.org/licenses/).");
         Ada.Text_IO.Put_Line ("(Use 'custom-' prefix for custom"
                               & " license identifier)");

         return False;
      end if;
   end License_Validation;

   -------------------
   -- Query_License --
   -------------------

   procedure Query_License (Info : in out Crate_Init_Info) is
      License_Other : constant String := "Other...";

      License_Vect : constant AAA.Strings.Vector :=
        AAA.Strings.Empty_Vector
        .Append ("MIT OR Apache-2.0")
        .Append ("MIT")
        .Append ("Apache-2.0")
        .Append ("BSD-3-Clause")
        .Append ("LGPL-3.0-or-later")
        .Append ("GPL-3.0-or-later WITH GPL-3.0-with-GCC-exception")
        .Append ("GPL-3.0-or-later")
        .Append (License_Other);

      Answer : Natural;
   begin
      Answer := CLIC.User_Input.Query_Multi
        (Question  => "Select a software " & Emph ("license") &
           " for the crate?",
         Choices   => License_Vect);

      if Answer not in License_Vect.First_Index .. License_Vect.Last_Index
        or else
          License_Vect (Answer) = License_Other
      then
         Info.Licenses :=
           To_Unbounded_String
             (CLIC.User_Input.Query_String
                (Question   => "Enter SPDX license expression" &
                     " (https://spdx.org/licenses/):",
                 Default    => "",
                 Validation => License_Validation'Access));
      else
         Info.Licenses := To_Unbounded_String (License_Vect (Answer));
      end if;
   end Query_License;

   ----------------------
   -- Query_Crate_Kind --
   ----------------------

   procedure Query_Crate_Kind (Info : in out Crate_Init_Info) is
      Kinds : AAA.Strings.Vector;
      Answer : Natural;
   begin
      for Elt in Crate_Kind loop
         Kinds.Append (Elt'Img);
      end loop;

      Answer := CLIC.User_Input.Query_Multi
        (Question  => "Select the " & Emph ("kind of crate") &
           " you want to create:",
         Choices   => Kinds);

      Info.Kind := Crate_Kind'Value (Kinds (Answer));
   end Query_Crate_Kind;

   ----------------------------
   -- Description_Validation --
   ----------------------------

   function Description_Validation (Str : String) return Boolean is
   begin
      if Str'Length > Alire.Max_Description_Length then
         Ada.Text_IO.Put_Line ("Description too long:"
                               & Str'Length'Img & " (max"
                               & Alire.Max_Description_Length'Img & ")");
         return False;
      else
         return True;
      end if;
   end Description_Validation;

   -----------------------
   -- Query_Description --
   -----------------------

   procedure Query_Description (Info : in out Crate_Init_Info) is
   begin
      Info.Description :=
        To_Unbounded_String
          (CLIC.User_Input.Query_String
             (Question   => "Enter a " & Emph ("short description") &
                " of the crate:",
              Default    => "",
              Validation => Description_Validation'Access));
   end Query_Description;

   -------------------------
   -- Tag_List_Validation --
   -------------------------

   function Tag_List_Validation (Str : String) return Boolean is
      Vect : constant AAA.Strings.Vector :=
        AAA.Strings.Split (Str, ',', Trim => True);

      Tags_Ok : Boolean := True;
   begin
      for Elt of Vect loop
         if Elt /= "" and then not Alire.Utils.Is_Valid_Tag (Elt) then
            Ada.Text_IO.Put_Line ("Invalid tag: '" & Elt & "'");
            Tags_Ok := False;
         end  if;
      end loop;

      return Tags_Ok;
   end Tag_List_Validation;

   ----------------
   -- Query_Tags --
   ----------------

   procedure Query_Tags (Info : in out Crate_Init_Info) is
      Answer : constant String :=
        CLIC.User_Input.Query_String
          (Question => "Enter a comma (',') separated list of " &
             Emph ("tags") & " to help people find your crate:",
           Default => "",
           Validation => Tag_List_Validation'Access);

      Vect : constant AAA.Strings.Vector :=
        AAA.Strings.Split (Answer, ',', Trim => True);
   begin
      for Elt of Vect loop
         if Elt /= "" then
            Info.Tags.Append (AAA.Strings.Trim (Elt));
         end if;
      end loop;
   end Query_Tags;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      Info : Crate_Init_Info;
   begin

      if Cmd.Bin and then Cmd.Lib then
         Reportaise_Wrong_Arguments ("Please provide either --bin or --lib");
      end if;

      Query_Crate_Name (Args, Info);

      if Cmd.Bin then
         Info.Kind := Binary;
      elsif Cmd.Lib then
         Info.Kind := Library;
      else
         Query_Crate_Kind (Info);
      end if;

      Query_Description (Info);

      --  Query User info
      Info.Username := To_Unbounded_String (UI.Query_Config.User_Name);
      Info.GitHub_Login := To_Unbounded_String
        (UI.Query_Config.User_GitHub_Login);
      Info.Email := To_Unbounded_String (UI.Query_Config.User_Email);

      Query_License (Info);

      Query_Tags (Info);

      Info.Website := To_Unbounded_String
        (CLIC.User_Input.Query_String
           (Question   => "Enter a opional " & Emph ("Website URL") &
              " for the crate:",
            Default    => "",
            Validation => null));
      Generate (Cmd, Info);
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
