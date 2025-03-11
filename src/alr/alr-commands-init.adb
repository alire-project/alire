with AAA.Text_IO;

with Ada.Wide_Wide_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alire.Directories;
with Alire.Paths;
with Alire.Settings.Builtins;
with Alire.Roots.Optional;
with Alire.Templates.Builtins;
with Alire.Utils.User_Input.Query_Config;

with CLIC.User_Input;

with SPDX;
with CLIC.TTY; use CLIC.TTY;

package body Alr.Commands.Init is

   package Dirs renames Alire.Directories;
   package Templates renames Alire.Templates;
   package TIO renames Ada.Wide_Wide_Text_IO;
   package UI renames Alire.Utils.User_Input;

   subtype Crate_Init_Info is Templates.Builtins.Crate_Init_Info;

   type Crate_Kind is (Library, Binary);

   --------------
   -- Generate --
   --------------

   procedure Generate (Cmd  : Command;
                       Info : Alire.Templates.Builtins.Crate_Init_Info)
   is
      use Dirs.Operators;

      For_Library : constant Boolean := Info.Is_Library;
      Name        : constant Alire.Crate_Name := +To_String (Info.Name);

      Directory   : constant Alire.Absolute_Path :=
        (if Cmd.In_Place
         then Dirs.Current
         else Dirs.Full_Name (Name.As_String));
      Test_Directory : constant Alire.Absolute_Path :=
         Directory / Alire.Paths.Default_Tests_Folder;

      ---------------------
      -- Generate_Config --
      ---------------------

      procedure Generate_Config is
         Root : Alire.Roots.Optional.Root :=
                  Alire.Roots.Optional.Detect_Root (Directory);
      begin
         Root.Value.Build_Prepare (Saved_Profiles => False,
                                   Force_Regen    => False);
      end Generate_Config;

      use Alire;

      -------------------------
      -- Generate_Main_Crate --
      -------------------------

      procedure Generate_Main_Crate is
      begin
         Templates.Translate_Tree
           (Directory,
            (if For_Library
             then Templates.Builtins.Crate_Lib
             else Templates.Builtins.Crate_Bin),
            Templates.Builtins.Init_Crate_Translation (Info));
      end Generate_Main_Crate;

      -----------------------
      -- Generate_Manifest --
      -----------------------

      procedure Generate_Manifest is
      begin
         Templates.Translate_Tree
           (Directory,
            (if For_Library
             then Templates.Builtins.Crate_Lib_No_Skel
             else Templates.Builtins.Crate_Bin_No_Skel),
            Templates.Builtins.Init_Crate_Translation (Info));
      end Generate_Manifest;

      -------------------------
      -- Generate_Test_Crate --
      -------------------------

      procedure Generate_Test_Crate is
      begin
         if Info.With_Tests then
            Templates.Translate_Tree
              (Test_Directory,
               Templates.Builtins.Crate_Test,
               Templates.Builtins.Init_Crate_Translation (Info));
         end if;
      end Generate_Test_Crate;

   begin

      if Cmd.No_Skel then
         Generate_Manifest;
      else
         Generate_Main_Crate;
      end if;

      if not Cmd.No_Skel then
         Generate_Config;

         if not Cmd.No_Test then
            Generate_Test_Crate;
         end if;
      end if;

      Alire.Put_Success (TTY.Emph (Name.As_String)
                         & " initialized successfully.");
   end Generate;

   ----------------------
   -- Query_Crate_Name --
   ----------------------

   procedure Query_Crate_Name
     (Args :        AAA.Strings.Vector;
      Info : in out Alire.Templates.Builtins.Crate_Init_Info)
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
                     Put_Line
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
         Put_Line
           ("Invalid SPDX license expression '" & Str
            & "': " & SPDX.Error (SP));
         Put_Line
           ("SPDX expression expected (https://spdx.org/licenses/).");
         Put_Line ("(Use 'custom-' prefix for custom"
                   & " license identifier)");

         return False;
      end if;
   end License_Validation;

   -------------------
   -- Query_License --
   -------------------

   procedure Query_License (Info : in out Templates.Builtins.Crate_Init_Info)
   is
      License_Other : constant String := "Other...";

      License_Vect : constant AAA.Strings.Vector :=
        AAA.Strings.Empty_Vector
        .Append ("MIT OR Apache-2.0 WITH LLVM-exception")
        .Append ("MIT")
        .Append ("Apache-2.0 WITH LLVM-exception")
        .Append ("Apache-2.0")
        .Append ("BSD-3-Clause")
        .Append ("LGPL-3.0-or-later")
        .Append ("GPL-3.0-or-later WITH GCC-exception-3.1")
        .Append ("GPL-3.0-or-later")
        .Append (License_Other);

      Answer : Natural := 0;
      function Chosen return String is (License_Vect (Answer));
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
         if not License_Validation (Chosen) then
            raise Program_Error with
              "Invalid license among choices: " & Chosen;
         end if;
         Info.Licenses := To_Unbounded_String (Chosen);
      end if;
   end Query_License;

   ------------------------
   -- Query_GitHub_Login --
   ------------------------

   procedure Query_GitHub_Login (Info : in out Crate_Init_Info) is
   begin
      if Alire.Settings.Builtins.User_Github_Login.Is_Empty then
         AAA.Text_IO.Put_Paragraph
           ("If you intend to publish this crate to the community index, you "
            & "will need a GitHub account with which to submit a pull "
            & "request, which can optionally be configured now (leave blank "
            & "to skip).");
      end if;
      Info.GitHub_Login := To_Unbounded_String
         (UI.Query_Config.User_GitHub_Login);
   end Query_GitHub_Login;

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

      Info.Is_Library := Library = Crate_Kind'Value (Kinds (Answer));
   end Query_Crate_Kind;

   ----------------------------
   -- Description_Validation --
   ----------------------------

   function Description_Validation (Str : String) return Boolean is
   begin
      if Str'Length > Alire.Max_Description_Length then
         Put_Line ("Description too long:"
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
         if Elt /= "" then
            declare
               Tag_Error : constant String := Alire.Utils.Error_In_Tag (Elt);
            begin
               if Tag_Error /= "" then
                  Put_Line (Tag_Error);
                  Tags_Ok := False;
               end if;
            end;
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
      use Alire.Settings;
      Info : Alire.Templates.Builtins.Crate_Init_Info;
      User_Not_Already_Configured : constant Boolean :=
         Builtins.User_Email.Is_Empty
         or else Builtins.User_Name.Is_Empty
         or else Builtins.User_Github_Login.Is_Empty;
   begin
      Cmd.Forbids_Structured_Output;

      if Cmd.Bin and then Cmd.Lib then
         Reportaise_Wrong_Arguments ("Please provide either --bin or --lib");
      end if;

      Info.With_Tests := not (Cmd.No_Test or else Cmd.No_Skel);

      Query_Crate_Name (Args, Info);

      if Cmd.Bin then
         Info.Is_Library := False;
      elsif Cmd.Lib then
         Info.Is_Library := True;
      else
         Query_Crate_Kind (Info);
      end if;

      Query_Description (Info);

      --  Query User info
      if User_Not_Already_Configured then
         TIO.New_Line;
         AAA.Text_IO.Put_Paragraph
           ("Alire needs some user information to prepare the crate for "
            & "eventual submission to an index, which will be interactively "
            & "requested now.");
         TIO.New_Line;
         TIO.Put_Line
           ("You can edit this information at any time with 'alr settings'");
         TIO.New_Line;
      end if;
      Info.Username := To_Unbounded_String (UI.Query_Config.User_Name);
      Query_GitHub_Login (Info);
      Info.Email := To_Unbounded_String (UI.Query_Config.User_Email);

      --  Make it clear that the remainder can't be changed with `alr settings`
      TIO.New_Line;
      if User_Not_Already_Configured then
         AAA.Text_IO.Put_Paragraph
            ("Alire needs some further crate-specific information to help "
             & "other people who want to use your crate.");
      end if;
      TIO.New_Line;

      Query_License (Info);

      Query_Tags (Info);

      Info.Website := To_Unbounded_String
        (CLIC.User_Input.Query_String
           (Question   => "Enter an optional " & Emph ("Website URL") &
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

      Define_Switch (Config,
                     Cmd.No_Test'Access,
                     "", "--no-test",
                     "Do not generate a minimal test crate skeleton"
                     & " (implied by --no-skel)");
   end Setup_Switches;

end Alr.Commands.Init;
