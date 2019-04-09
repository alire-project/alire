with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Alire.Conditional;
with Alire.GPR;
with Alire.Index;
with Alire.Origins;
with Alire.Requisites;
with Alire.Requisites.Booleans;

with TOML;
use type TOML.Any_Value_Kind, TOML.TOML_Value;

with Alire.Utils;

package body Alire.TOML_Index is

   package Dirs renames Ada.Directories;
   package Exc renames Ada.Exceptions;
   package TIO renames Ada.Text_IO;

   procedure Set_Error
     (Result            : out Load_Result;
      Filename, Message : String;
      Context           : String := "")
      with Post => not Result.Success;
   --  Set Result to not successful and assign an error message to it

   function Load_TOML_From_File
     (Filename : String; Result : out Load_Result) return TOML.TOML_Value;
   --  Load a TOML document from the content of the given Filename and return
   --  it. In case of error, the result is not significant and Result.Success
   --  is set to False. Otherwise, it is set to True.

   procedure Check_Index (Catalog_Dir : String; Result : out Load_Result)
      with Pre => Result.Success;
   --  Check that Catalog_Dir contains a file called "index.toml" and that it
   --  describes a supported catalog.

   procedure Load_Package_Directory
     (Catalog_Dir, Package_Dir : String;
      Environment              : Environment_Maps.Map;
      Result                   : out Load_Result)
      with Pre => Result.Success;
   --  Load packages from all *.toml files in Catalog_Dir/Package_Dir

   procedure Load_From_Catalog_Internal
     (Catalog_Dir, Package_Name : String;
      Environment               : Environment_Maps.Map;
      Result                    : out Load_Result);
   --  Like Load_From_Catalog, but do not check the index

   function Package_Directory (Package_Name : String) return String is
     (Package_Name (Package_Name'First .. Package_Name'First + 1));
   --  Return the name of the directory that must contain the description of
   --  the given package.

   Expected_Index : constant TOML.TOML_Value := TOML.Create_Table;
   --  TOML value for the expected content of "index.toml"

   Package_File_Suffix : constant String := ".toml";
   --  Suffix for the name of package description files

   subtype Package_Name_Character is Project_Character
      with Static_Predicate => Package_Name_Character /= Extension_Separator;

   --  Declare all our string literals once to avoid undetected typos later
   --  on.

   Actions_Str           : constant String := "actions";
   Archive_Name_Str      : constant String := "archive-name";
   Authors_Str           : constant String := "authors";
   Available_Str         : constant String := "available";
   Command_Str           : constant String := "command";
   Depends_On_Str        : constant String := "depends-on";
   Description_Str       : constant String := "description";
   Executables_Str       : constant String := "executables";
   GPR_Externals_Str     : constant String := "gpr-externals";
   GPR_Set_Externals_Str : constant String := "gpr-set-externals";
   General_Str           : constant String := "general";
   Licenses_Str          : constant String := "licenses";
   Maintainers_Str       : constant String := "maintainers";
   Notes_Str             : constant String := "notes";
   Origin_Str            : constant String := "origin";
   Post_Compile_Str      : constant String := "post-compile";
   Post_Fetch_Str        : constant String := "post-fetch";
   Project_Files_Str     : constant String := "project-files";
   Type_Str              : constant String := "type";
   Website_Str           : constant String := "website";

   Custom_License_Prefix : constant String := "custom:";

   Native_Prefix : constant String := "native:";

   function To_Version_Set
     (Set   : out Semantic_Versioning.Version_Set;
      Value : TOML.TOML_Value) return Boolean;
   --  Decode Value as a version set. Return whether successful.
   --
   --  TODO: improve error reporting

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (Result            : out Load_Result;
      Filename, Message : String;
      Context           : String := "")
   is
      Full_Context : constant String :=
         (if Context = ""
          then Filename
          else Filename & "(" & Context & ")");
   begin
      Result := (Success => False,
                 Message => Ada.Strings.Unbounded.To_Unbounded_String
                   (Full_Context & ": " & Message));
   end Set_Error;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (Result : Load_Result) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Result.Message);
   end Error_Message;

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment
     (Env     : in out Environment_Maps.Map;
      Distrib : Platforms.Distributions;
      OS       : Platforms.Operating_Systems;
      Compiler : Platforms.Compilers)
   is
      Distrib_Str : constant String :=
        (case Distrib is
         when Platforms.Debian         => "debian",
         when Platforms.Ubuntu         => "ubuntu",
         when Platforms.Distro_Unknown => "none");
      OS_Str : constant String :=
        (case OS is
         when Platforms.GNU_Linux  => "linux",
         when Platforms.OSX        => "macos",
         when Platforms.Windows    => "windows",
         when Platforms.OS_Unknown => raise Program_Error);
      Compiler_Str : constant String :=
        (case Compiler is
         when Platforms.GNAT_Unknown          => "gnat-unknown",
         when Platforms.GNAT_FSF_Old          => "gnat-fsf-old",
         when Platforms.GNAT_FSF_7_2          => "gnat-fsf-7.2",
         when Platforms.GNAT_FSF_7_3_Or_Newer => "gnat-fsf-7.3",
         when Platforms.GNAT_GPL_Old          => "gnat-gpl-old",
         when Platforms.GNAT_GPL_2017         => "gnat-gpl-2017",
         when Platforms.GNAT_Community_2018   => "gnat-community-2018");
   begin
      Env.Insert (+"distrib", +Distrib_Str);
      Env.Insert (+"os", +OS_Str);
      Env.Insert (+"compiler", +Compiler_Str);
   end Set_Environment;

   ------------------------
   -- Valid_Package_Name --
   ------------------------

   function Valid_Package_Name (Name : String) return Boolean is
   begin
      if Name'Length = 0 then
         return False;
      end if;

      for I in Name'Range loop
         if I in Name'First | Name'Last and then Name (I) = '_' then

            --  Reject leading and trailing underscores

            return False;

         elsif I = Name'First and then Name (I) in '0' .. '9' then

            --  Reject leading digits

            return False;

         elsif Name (I) = '_' and then Name (I - 1) = '_' then

            --  Reject consecutive underscores

            return False;
         end if;
      end loop;

      return True;
   end Valid_Package_Name;

   -------------------------
   -- Load_TOML_From_File --
   -------------------------

   function Load_TOML_From_File
     (Filename : String; Result : out Load_Result) return TOML.TOML_Value
   is
      TOML_Result : constant TOML.Read_Result := TOML.Load_File (Filename);
   begin
      if TOML_Result.Success then
         Result := (Success => True);
         return TOML_Result.Value;
      else
         Set_Error (Result, Filename, TOML.Format_Error (TOML_Result));
         return TOML.No_TOML_Value;
      end if;
   end Load_TOML_From_File;

   -----------------
   -- Check_Index --
   -----------------

   procedure Check_Index (Catalog_Dir : String; Result : out Load_Result) is
      Filename : constant String := Dirs.Compose (Catalog_Dir, "index.toml");
      Value    : TOML.TOML_Value;
   begin
      --  Read "index.toml"

      Value := Load_TOML_From_File (Filename, Result);
      if not Result.Success then
         return;
      end if;

      --  Check that its content is what we expect: {"version": "1.0"}. TODO:
      --  provide more information when the check fails.

      if not TOML.Equals (Value, Expected_Index) then
         Set_Error (Result, Filename, "unexpected index information");
         return;
      end if;
   end Check_Index;

   ------------------
   -- Load_Catalog --
   ------------------

   procedure Load_Catalog
     (Catalog_Dir : String;
      Environment : Environment_Maps.Map;
      Result      : out Load_Result)
   is
      Search : Dirs.Search_Type;
      --  Look for all directories in Catalog_Dir. We will process only the
      --  ones whose names contain exactly two characters in 'a' .. 'z' | '_'.

      Dir_Entry : Dirs.Directory_Entry_Type;
   begin
      Trace.Detail ("Loading full catalog from " & Catalog_Dir);

      Check_Index (Catalog_Dir, Result);

      --  Go through all directories allowed to contain packages

      begin
         Dirs.Start_Search
           (Search    => Search,
            Directory => Catalog_Dir,
            Pattern   => "",
            Filter    => (Dirs.Directory => True, others => False));
      exception
         when E : TIO.Use_Error | TIO.Name_Error =>
            Set_Error (Result, Catalog_Dir, Exc.Exception_Name (E),
                       "looking for packages");
      end;

      while Result.Success and then Dirs.More_Entries (Search) loop
         Dirs.Get_Next_Entry (Search, Dir_Entry);
         declare
            Simple_Name : constant String := Dirs.Simple_Name (Dir_Entry);
            First, Last : Character;
         begin
            if Simple_Name'Length = 2 then
               First := Simple_Name (Simple_Name'First);
               Last := Simple_Name (Simple_Name'Last);
               if First in Package_Name_Character
                  and then First not in '_'
                  and then Last in Package_Name_Character
               then
                  Load_Package_Directory
                    (Catalog_Dir, Simple_Name, Environment, Result);
               end if;
            end if;
         end;
      end loop;

      Dirs.End_Search (Search);
   end Load_Catalog;

   ----------------------------
   -- Load_Package_Directory --
   ----------------------------

   procedure Load_Package_Directory
     (Catalog_Dir, Package_Dir : String;
      Environment              : Environment_Maps.Map;
      Result                   : out Load_Result)
   is
      Package_Dir_Full : constant String :=
         Dirs.Compose (Catalog_Dir, Package_Dir);
      --  Full name for the directory under which we must look for package
      --  descriptions.

      Search : Dirs.Search_Type;
      --  Look for all files in Package_Dir_Full whose name matches "*.toml"
      --  and try to load them as package descriptions.

      Dir_Entry : Dirs.Directory_Entry_Type;
   begin
      begin
         Dirs.Start_Search
           (Search    => Search,
            Directory => Package_Dir_Full,
            Pattern   => "",
            Filter    => (Dirs.Ordinary_File => True, others => False));
      exception
         when E : TIO.Use_Error | TIO.Name_Error =>
            Set_Error (Result, Package_Dir_Full, Exc.Exception_Name (E),
                       "looking for packages");
      end;

      while Result.Success and then Dirs.More_Entries (Search) loop
         Dirs.Get_Next_Entry (Search, Dir_Entry);
         declare
            Simple_Name : constant String := Dirs.Simple_Name (Dir_Entry);
         begin
            if Utils.Ends_With (Simple_Name, Package_File_Suffix) then
               declare
                  Package_Name : String renames Simple_Name
                    (Simple_Name'First
                     .. Simple_Name'Last - Package_File_Suffix'Length);
               begin
                  --  Reject invalid package names

                  if not Valid_Package_Name (Package_Name) then
                     Set_Error (Result, Package_Dir_Full,
                                "invalid package name: " & Simple_Name,
                                "looking for packages");
                     exit;
                  end if;

                  --  Reject files not in the appropriate directory

                  if Package_Directory (Package_Name) /= Package_Dir then
                     Set_Error (Result, Package_Dir_Full,
                                "bad location for " & Simple_Name,
                                "looking for packages");
                     exit;
                  end if;

                  Load_From_Catalog_Internal
                    (Catalog_Dir, Package_Name, Environment, Result);
                  if not Result.Success then
                     exit;
                  end if;
               end;
            end if;
         end;
      end loop;

      Dirs.End_Search (Search);
   end Load_Package_Directory;

   --------------------------------
   -- Load_From_Catalog_Internal --
   --------------------------------

   procedure Load_From_Catalog_Internal
     (Catalog_Dir, Package_Name : String;
      Environment               : Environment_Maps.Map;
      Result                    : out Load_Result)
   is
      Filename : constant String :=
         Dirs.Compose
           (Dirs.Compose (Catalog_Dir, Package_Directory (Package_Name)),
            Package_Name & ".toml");

      Value : TOML.TOML_Value;
      Pkg   : Package_Type;
   begin
      Trace.Detail ("Loading " & Package_Name & " from " & Catalog_Dir);

      --  Load the TOML file

      Value := Load_TOML_From_File (Filename, Result);
      if not Result.Success then
         return;
      end if;

      --  Convert it to our intermediate data structures

      Decode_TOML_Package (Filename, Package_Name, Value, Pkg, Result);
      if not Result.Success then
         return;
      end if;

      --  TODO: check that dependencies are available before doing the import
      --  (and potentially import these dependencies first).

      --  Finally import it to the catalog

      Import_TOML_Package (Pkg, Environment);
   end Load_From_Catalog_Internal;

   -----------------------
   -- Load_From_Catalog --
   -----------------------

   procedure Load_From_Catalog
     (Catalog_Dir, Package_Name : String;
      Environment               : Environment_Maps.Map;
      Result                    : out Load_Result) is
   begin
      Check_Index (Catalog_Dir, Result);
      if Result.Success then
         Load_From_Catalog_Internal
           (Catalog_Dir, Package_Name, Environment, Result);
      end if;
   end Load_From_Catalog;

   --------------------
   -- To_Version_Set --
   --------------------

   function To_Version_Set
     (Set   : out Semantic_Versioning.Version_Set;
      Value : TOML.TOML_Value) return Boolean
   is
      package SV renames Semantic_Versioning;
   begin
      if Value.Kind /= TOML.TOML_String then
         return False;
      end if;

      declare
         type Constraint_Kind is (Exactly, Within_Major, Within_Minor);

         V : SV.Version;

         S          : constant String := Value.As_String;
         S_First    : Positive := S'First + 1;
         Constraint : Constraint_Kind;
      begin
         if S'Length = 0 then
            return False;
         end if;

         case S (S'First) is
            when '^' =>
               Constraint := Within_Major;
            when '~' =>
               Constraint := Within_Minor;
            when others =>
               Constraint := Exactly;
               S_First := S'First;
         end case;

         begin
            V := SV.Parse (S (S_First .. S'Last));
         exception
            when Constraint_Error =>
               return False;
         end;

         Set := (case Constraint is
                 when Exactly => SV.Exactly (V),
                 when Within_Major => SV.Within_Major (V),
                 when Within_Minor => SV.Within_Minor (V));
         return True;
      end;
   end To_Version_Set;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency
     (D : Dependency; Result : in out Dependencies_Result.T)
   is
      Position : Dependency_Maps.Cursor;
      Inserted : Boolean;
   begin
      if not Result.Success then
         return;
      end if;

      Result.Value.Insert (D.Name, D, Position, Inserted);
      if not Inserted then
         Result := (Success => False,
                    Error   => +("duplicate entry: " & (+D.Name)));
      end if;
   end Add_Dependency;

   -------------------
   -- Parse_Literal --
   -------------------

   procedure Parse_Literal
     (Value : TOML.TOML_Value; Result : out Dependencies_Result.T) is
   begin
      if Value.Kind /= TOML.TOML_Table then
         Result := (Success => False, Error => +"object expected");
         return;
      end if;

      Result := (Success => True, Value => <>);
      for E of Value.Iterate_On_Table loop
         declare
            Package_Name : US.Unbounded_String renames E.Key;
            Version      : TOML.TOML_Value renames E.Value;
            V            : Semantic_Versioning.Version_Set;
         begin
            if Version.Kind = TOML.TOML_String
               and then Version.As_String = "any"
            then
               Add_Dependency ((Any => True, Name => Package_Name), Result);

            elsif not To_Version_Set (V, Version) then
               Result := (Success => False,
                          Error   => +("invalid version: "
                                       & Version.Dump_As_String));
               return;

            else
               Add_Dependency
                 ((Any => False, Name => Package_Name, Versions => V), Result);
            end if;
         end;
      end loop;
   end Parse_Literal;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Left, Right : Dependency_Maps.Map;
      Result      : out Dependencies_Result.T) is
   begin
      Result := (Success => True, Value => Left);
      for Position in Right.Iterate loop
         Add_Dependency (Dependency_Maps.Element (Position), Result);
      end loop;
   end Merge;

   -------------------
   -- Parse_Literal --
   -------------------

   procedure Parse_Literal
     (Value : TOML.TOML_Value; Result : out Boolean_Result.T) is
   begin
      if Value.Kind = TOML.TOML_Boolean then
         Result := (Success => True, Value => Value.As_Boolean);
      else
         Result := (Success => False, Error => +"boolean expected");
      end if;
   end Parse_Literal;

   -------------------
   -- Parse_Literal --
   -------------------

   procedure Parse_Literal
     (Value : TOML.TOML_Value; Result : out String_Result.T) is
   begin
      if Value.Kind = TOML.TOML_String then
         Result := (Success => True, Value => Value.As_Unbounded_String);
      else
         Result := (Success => False, Error => +"string expected");
      end if;
   end Parse_Literal;

   -------------------
   -- Parse_Literal --
   -------------------

   procedure Parse_Literal
     (Value : TOML.TOML_Value; Result : out Origin_Result.T)
   is

      function Decode_VCS
        (URL                : String;
         Repo_URL, Revision : out US.Unbounded_String) return Boolean;
      --  Attempt to decode URL as a VCS URL: <vcs>+URL@REVISION. Return
      --  whether successful.

      ----------------
      -- Decode_VCS --
      ----------------

      function Decode_VCS
        (URL                : String;
         Repo_URL, Revision : out US.Unbounded_String) return Boolean
      is
         Plus_Index : constant Positive := Ada.Strings.Fixed.Index (URL, "+");
         At_Index   : constant Natural  := Ada.Strings.Fixed.Index
           (URL, "@", Ada.Strings.Forward);
      begin
         if At_Index = 0 then
            return False;
         end if;

         --  TODO: check that Repo_URL starts with a protocol (NAME://) and
         --  that revision is not empty.

         Repo_URL := +URL (Plus_Index + 1 .. At_Index - 1);
         Revision := +URL (At_Index + 1 .. URL'Last);
         return True;
      end Decode_VCS;

   begin
      if Value.Kind /= TOML.TOML_String then
         Result := (Success => False, Error => +"string expected");
         return;
      end if;

      declare
         use Utils;

         S        : constant String := Value.As_String;
         URL      : US.Unbounded_String;
         Revision : US.Unbounded_String;
      begin
         Result := (Success => True, Value => <>);

         if Starts_With (S, "git+")
            and then Decode_VCS (S, URL, Revision)
         then
            Result.Value := (Git, URL, Revision);

         elsif Starts_With (S, "hg+")
            and then Decode_VCS (S, URL, Revision)
         then
            Result.Value := (Mercurial, URL, Revision);

         elsif Starts_With (S, "svn+")
            and then Decode_VCS (S, URL, Revision)
         then
            Result.Value := (SVN, URL, Revision);

         elsif Starts_With (S, Native_Prefix) then
            Result.Value := (Native_Package,
                             +S (S'First + Native_Prefix'Length .. S'Last));

         else
            Result.Value := (Source_Archive, +S);
         end if;
      end;
   end Parse_Literal;

   -------------------------
   -- Decode_TOML_Package --
   -------------------------

   procedure Decode_TOML_Package
     (Filename, Package_Name : String;
      Value                  : TOML.TOML_Value;
      Pkg                    : out Package_Type;
      Result                 : out Load_Result)
   is

      procedure Set_Error (Message, Context : String);
      --  Shortcut for the global Set_Error procedure

      package Key_Queues is new Ada.Containers.Ordered_Sets
        (Element_Type => US.Unbounded_String,
         "="          => US."=",
         "<"          => US."<");

      type Key_Queue_Type is record
         Object : TOML.TOML_Value;
         Keys   : Key_Queues.Set;
      end record;

      function Key_Queue
        (Value : TOML.TOML_Value; Context : String) return Key_Queue_Type;
      --  Issue an error in Result if Value is not a TOML object. Otherwise,
      --  turn its set of keys into a Key_Queue value.

      function Pop
        (Queue     : in out Key_Queue_Type;
         Key       : String;
         Value     : out TOML.TOML_Value;
         Mandatory : Boolean := False;
         Context   : String := "") return Boolean;
      --  Remove Key from the given set of keys and set Value to the
      --  corresponding value in Queue. Return whether Key was present. If not
      --  and Mandatory was true, issue an error in Result.

      function Pop_Next
        (Queue : in out Key_Queue_Type;
         Key   : out US.Unbounded_String;
         Value : out TOML.TOML_Value) return Boolean;
      --  If the Queue is empty, return False. Otherwise, remove the first Key
      --  from Queue, set it to Key and set Value to the corresponding value
      --  and return True.

      function Report_Extra_Keys
        (Context : String; Queue : Key_Queue_Type) return Boolean;
      --  If Queue still contains pending keys, consider it's an error, set
      --  Result accordingly and return false. Just return true otherwise.

      function To_String
        (Context    : String;
         Out_String : out US.Unbounded_String;
         Value      : TOML.TOML_Value) return Boolean;
      --  If Value is not a string, assign error information to Result and
      --  return false. Forward this string to Out_String otherwise.

      function To_String_Vector
        (Context : String;
         Vector  : out String_Vectors.Vector;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not an array that contains strings, assign error
      --  information to Result and return false. Forward these strings to
      --  Vector otherwise.

      function To_String_Set
        (Context : String;
         Set     : out String_Sets.Set;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not an array that contains unique strings, assign error
      --  information to Result and return false. Forward these strings to Set
      --  otherwise.

      function To_String_Expression
        (Context : String;
         Expr    : out String_Expressions.Expression;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not a valid string expression, assign error information
      --  to Result and return false. Initialize Expr and return true
      --  otherwise.

      function To_Origin_Expression
        (Context : String;
         Expr    : out Origin_Expressions.Expression;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not a valid origin expression, assign error information
      --  to Result and return false. Initialize Expr and return true
      --  otherwise.

      function To_GPR_Externals
        (Context : String;
         Map     : out GPR_Externals_Maps.Map;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not valid gpr-externals entry, assign error information
      --  to Result and return false. Fill out Map accordingly otherwise.

      function To_GPR_Set_Externals
        (Context : String;
         Map     : out GPR_Set_Externals_Maps.Map;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not valid gpr-set-externals entry, assign error
      --  information to Result and return false. Fill out Map accordingly
      --  otherwise.

      function To_Dependencies
        (Context : String;
         Deps    : out Dependencies_Expressions.Expression;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not valid depends-on entry, assign error information to
      --  Result and return false. Fill out Deps accordingly otherwise.

      function To_License_Vector
        (Context : String;
         Vector  : out License_Vectors.Vector;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not an array that contains valid license strings, assign
      --  error information to Result and return false. Forward these licenses
      --  to Vector otherwise.

      function To_Action
        (Context : String;
         Act     : out Action;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not a valid action, assign error information to Result
      --  and return false. Import it into Act otherwise.

      function To_Action_Vector
        (Context : String;
         Vector  : out Action_Vectors.Vector;
         Value   : TOML.TOML_Value) return Boolean;
      --  If Value is not an array that contains valid actions, assign error
      --  information to Result and return false. Import these actions into
      --  Vector otherwise.

      function Process_Common
        (Queue   : in out Key_Queue_Type;
         Common  : in out Common_Data;
         Context : String) return Boolean;
      --  Do common processing between general and release entries

      function Process_General (Value : TOML.TOML_Value) return Boolean;
      --  Decode information from Value into Pkg. This should contain the
      --  object encoding general information about the current package.
      --  Return whether successful. If not, assign an error to Result.

      function Process_Release
        (R : Release; Value : TOML.TOML_Value) return Boolean;
      --  Decode information from Value into R. Return whether successful. If
      --  not, assign an error to Result.

      ---------------
      -- Set_Error --
      ---------------

      procedure Set_Error (Message, Context : String) is
      begin
         Set_Error (Result, Filename, Message, Context);
      end Set_Error;

      ---------------
      -- Key_Queue --
      ---------------

      function Key_Queue
        (Value : TOML.TOML_Value; Context : String) return Key_Queue_Type is
      begin
         return Queue : Key_Queue_Type do
            if Value.Kind = TOML.TOML_Table then
               Queue.Object := Value;
               for Key of Value.Keys loop
                  Queue.Keys.Insert (Key);
               end loop;
            else
               Set_Error ("table expected", Context);
            end if;
         end return;
      end Key_Queue;

      ---------
      -- Pop --
      ---------

      function Pop
        (Queue     : in out Key_Queue_Type;
         Key       : String;
         Value     : out TOML.TOML_Value;
         Mandatory : Boolean := False;
         Context   : String := "") return Boolean
      is
         Cursor : Key_Queues.Cursor := Queue.Keys.Find (+Key);
      begin
         if Key_Queues.Has_Element (Cursor) then
            Queue.Keys.Delete (Cursor);
            Value := Queue.Object.Get (Key);
            return True;

         elsif Mandatory then
            Set_Error ("missing mandatory entry: " & Key, Context);
         end if;

         return False;
      end Pop;

      --------------
      -- Pop_Next --
      --------------

      function Pop_Next
        (Queue : in out Key_Queue_Type;
         Key   : out US.Unbounded_String;
         Value : out TOML.TOML_Value) return Boolean is
      begin
         if Queue.Keys.Is_Empty then
            return False;
         end if;

         Key := Queue.Keys.First_Element;
         Value := Queue.Object.Get (Key);
         Queue.Keys.Delete_First;
         return True;
      end Pop_Next;

      -----------------------
      -- Report_Extra_Keys --
      -----------------------

      function Report_Extra_Keys
        (Context : String; Queue : Key_Queue_Type) return Boolean
      is
         Message  : US.Unbounded_String := +"forbidden extra entries: ";
         Is_First : Boolean := True;
      begin
         if Queue.Keys.Is_Empty then
            return True;
         else
            for Key of Queue.Keys loop
               if Is_First then
                  Is_First := False;
               else
                  US.Append (Message, ", ");
               end if;
               US.Append (Message, Key);
            end loop;
            Set_Error (+Message, Context);
            return False;
         end if;
      end Report_Extra_Keys;

      ---------------
      -- To_String --
      ---------------

      function To_String
        (Context    : String;
         Out_String : out US.Unbounded_String;
         Value      : TOML.TOML_Value) return Boolean
      is
      begin
         if Value.Kind /= TOML.TOML_String then
            Set_Error ("string expected", Context);
            return False;
         else
            Out_String := Value.As_Unbounded_String;
            return True;
         end if;
      end To_String;

      ----------------------
      -- To_String_Vector --
      ----------------------

      function To_String_Vector
        (Context : String;
         Vector  : out String_Vectors.Vector;
         Value   : TOML.TOML_Value) return Boolean
      is
         Item : TOML.TOML_Value;
      begin
         if Value.Kind /= TOML.TOML_Array then
            Set_Error ("array expected", Context);
            return False;
         end if;

         for I in 1 .. Value.Length loop
            Item := Value.Item (I);
            if Item.Kind /= TOML.TOML_String then
               Set_Error ("string expected", Context & "[" & I'Image & "]");
               return False;
            end if;
            Vector.Append (Item.As_Unbounded_String);
         end loop;

         return True;
      end To_String_Vector;

      -------------------
      -- To_String_Set --
      -------------------

      function To_String_Set
        (Context : String;
         Set     : out String_Sets.Set;
         Value   : TOML.TOML_Value) return Boolean
      is
         Vector : String_Vectors.Vector;
      begin
         if not To_String_Vector (Context, Vector, Value) then
            return False;
         end if;

         for S of Vector loop
            declare
               Position : String_Sets.Cursor;
               Inserted : Boolean;
            begin
               Set.Insert (S, Position, Inserted);
               if not Inserted then
                  Set_Error ("double entry: " & (+S), Context);
                  return False;
               end if;
            end;
         end loop;

         return True;
      end To_String_Set;

      --------------------------
      -- To_String_Expression --
      --------------------------

      function To_String_Expression
        (Context : String;
         Expr    : out String_Expressions.Expression;
         Value   : TOML.TOML_Value) return Boolean
      is
         Res : String_Expressions.Parsing_Result :=
            String_Expressions.Parse (Value);
      begin
         if Res.Success then
            String_Expressions.Move (Expr, Res.Value);
         else
            Set_Error (+Res.Error, Context);
         end if;
         return Res.Success;
      end To_String_Expression;

      --------------------------
      -- To_Origin_Expression --
      --------------------------

      function To_Origin_Expression
        (Context : String;
         Expr    : out Origin_Expressions.Expression;
         Value   : TOML.TOML_Value) return Boolean
      is
         Res : Origin_Expressions.Parsing_Result :=
            Origin_Expressions.Parse (Value);
      begin
         if Res.Success then
            Origin_Expressions.Move (Expr, Res.Value);
         else
            Set_Error (+Res.Error, Context);
         end if;
         return Res.Success;
      end To_Origin_Expression;

      ----------------------
      -- To_GPR_Externals --
      ----------------------

      function To_GPR_Externals
        (Context : String;
         Map     : out GPR_Externals_Maps.Map;
         Value   : TOML.TOML_Value) return Boolean is
      begin
         if Value.Kind /= TOML.TOML_Table then
            Set_Error ("object expected", Context);
            return False;
         end if;

         for E of Value.Iterate_On_Table loop
            declare
               Externals_Values : GPR_Externals_Values;
               Dummy            : Boolean;
            begin
               --  Allocate and register the string set right away. Controlled
               --  objects will make sure that it is deallocated in case of
               --  failure. The call to Insert is not supposed to fail since
               --  it's not possible for a single Value object to have several
               --  identical keys.

               Externals_Values := new String_Sets.Set;
               Map.Insert (E.Key, Externals_Values);

               Dummy := To_String_Set
                 (Context & ":" & (+E.Key), Externals_Values.all, E.Value);
               if not Result.Success then
                  return False;
               end if;
            end;
         end loop;
         return True;
      end To_GPR_Externals;

      --------------------------
      -- To_GPR_Set_Externals --
      --------------------------

      function To_GPR_Set_Externals
        (Context : String;
         Map     : out GPR_Set_Externals_Maps.Map;
         Value   : TOML.TOML_Value) return Boolean is
      begin
         if Value.Kind /= TOML.TOML_Table then
            Set_Error ("object expected", Context);
            return False;
         end if;

         for E of Value.Iterate_On_Table loop
            declare
               Set_Externals_Expr : GPR_Set_Externals_Expr;
               Dummy              : Boolean;
            begin
               --  Allocate and register the string expression right away.
               --  Controlled objects will make sure that it is deallocated in
               --  case of failure. The call to Insert is not supposed to fail
               --  since it's not possible for a single TOML table to have
               --  several identical keys.

               Set_Externals_Expr:= new String_Expressions.Expression;
               Map.Insert (E.Key, Set_Externals_Expr);

               Dummy := To_String_Expression
                 (Context & ":" & (+E.Key), Set_Externals_Expr.all, E.Value);
               if not Result.Success then
                  return False;
               end if;
            end;
         end loop;
         return True;
      end To_GPR_Set_Externals;

      ---------------------
      -- To_Dependencies --
      ---------------------

      function To_Dependencies
        (Context : String;
         Deps    : out Dependencies_Expressions.Expression;
         Value   : TOML.TOML_Value) return Boolean
      is
         Parsing_Result : Dependencies_Expressions.Parsing_Result :=
            Dependencies_Expressions.Parse (Value);
      begin
         if Parsing_Result.Success then
            Dependencies_Expressions.Move (Deps, Parsing_Result.Value);
            return True;
         else
            Set_Error (+Parsing_Result.Error, Context);
            return False;
         end if;
      end To_Dependencies;

      -----------------------
      -- To_License_Vector --
      -----------------------

      function To_License_Vector
        (Context : String;
         Vector  : out License_Vectors.Vector;
         Value   : TOML.TOML_Value) return Boolean
      is
         Strings : String_Vectors.Vector;
      begin
         if not To_String_Vector (Context, Strings, Value) then
            return False;
         end if;

         for S of Strings loop
            declare
               use Licensing;

               Text : constant String := +S;
               L    : License_Type;
            begin
               if Utils.Starts_With (Text, Custom_License_Prefix) then
                  L := (Custom => True,
                        Text   => US.Unbounded_Slice
                          (S, Custom_License_Prefix'Length + 1,
                           US.Length (S)));
               else
                  L := (Custom => False, License => <>);
                  L.License := From_String (Text);
                  if L.License = Unknown then
                     Set_Error ("unknown license: " & Text, Context);
                     return False;
                  end if;
               end if;
               Vector.Append (L);
            end;
         end loop;

         return True;
      end To_License_Vector;

      ---------------
      -- To_Action --
      ---------------

      function To_Action
        (Context : String;
         Act     : out Action;
         Value   : TOML.TOML_Value) return Boolean
      is
         Queue : Key_Queue_Type := Key_Queue (Value, General_Str);
         Kind  : US.Unbounded_String;
         Tmp   : TOML.TOML_Value;
      begin
         if not Result.Success then
            return False;
         end if;

         if not Pop (Queue, Type_Str, Tmp, True, Context)
            or else not To_String (Context, Kind, Tmp)

            or else not Pop (Queue, Command_Str, Tmp, True, Context)
            or else not To_String (Context, Act.Command, Tmp)
         then
            return False;
         end if;

         declare
            K : constant String := +Kind;
         begin
            if K = Post_Fetch_Str then
               Act.Kind := Actions.Post_Fetch;
            elsif K = Post_Compile_Str then
               Act.Kind := Actions.Post_Compile;
            else
               Set_Error ("invalid action kind: " & K, Context);
               return False;
            end if;
         end;

         return Report_Extra_Keys (Context, Queue);
      end To_Action;

      ----------------------
      -- To_Action_Vector --
      ----------------------

      function To_Action_Vector
        (Context : String;
         Vector  : out Action_Vectors.Vector;
         Value   : TOML.TOML_Value) return Boolean
      is
         Item : Action;
      begin
         if Value.Kind /= TOML.TOML_Array then
            Set_Error ("array expected", Context);
            return False;
         end if;

         for I in 1 .. Value.Length loop
            if not To_Action
                 (Context & "[" & I'Image & "]", Item, Value.Item (I))
            then
               return False;
            end if;
            Vector.Append (Item);
         end loop;

         return True;
      end To_Action_Vector;

      --------------------
      -- Process_Common --
      --------------------

      function Process_Common
        (Queue   : in out Key_Queue_Type;
         Common  : in out Common_Data;
         Context : String) return Boolean
      is
         Tmp : TOML.TOML_Value;
      begin
         --  Decode the optional notes

         if Pop (Queue, Notes_Str, Tmp)
            and then not To_String
              (Context & ":notes", Common.Notes, Tmp)
         then
            return False;
         end if;

         --  Decode the optional list of executables, project files, GPR
         --  externals and default values for these.

         if Pop (Queue, Executables_Str, Tmp)
            and then not To_String_Vector
              (Context & ":executables", Common.Executables, Tmp)
         then
            return False;
         end if;

         if Pop (Queue, Project_Files_Str, Tmp)
            and then not To_String_Vector
              (Context & ":project-files", Common.Project_Files, Tmp)
         then
            return False;
         end if;

         if Pop (Queue, GPR_Externals_Str, Tmp)
            and then not To_GPR_Externals
              (Context & ":gpr-externals", Common.GPR_Externals, Tmp)
         then
            return False;
         end if;

         if Pop (Queue, GPR_Set_Externals_Str, Tmp)
            and then not To_GPR_Set_Externals
              (Context & ":gpr-set-externals", Common.GPR_Set_Externals, Tmp)
         then
            return False;
         end if;

         --  Decode the optional list of dependencies

         if Pop (Queue, Depends_On_Str, Tmp)
            and then not To_Dependencies
              (Context & ":depends-on", Common.Dependencies, Tmp)
         then
            return False;
         end if;

         --  Decode the optional availability expression

         if Pop (Queue, Available_Str, Tmp) then
            declare
               Res : Boolean_Expressions.Parsing_Result :=
                  Boolean_Expressions.Parse (Tmp);
            begin
               if Res.Success then
                  Boolean_Expressions.Move (Common.Available, Res.Value);
               else
                  Set_Error (+Res.Error, Context & ":available");
                  return False;
               end if;
            end;
         end if;

         --  Decode the optional list of actions

         if Pop (Queue, Actions_Str, Tmp)
            and then not To_Action_Vector
              (Context & ":actions", Common.Actions, Tmp)
         then
            return False;
         end if;

         return True;
      end Process_Common;

      ---------------------
      -- Process_General --
      ---------------------

      function Process_General (Value : TOML.TOML_Value) return Boolean is
         Queue : Key_Queue_Type := Key_Queue (Value, General_Str);
         Tmp  : TOML.TOML_Value;
      begin
         if not Result.Success then
            return False;
         end if;

         --  Decode the package description

         if not Pop (Queue, Description_Str, Tmp, True, General_Str)
            or else not To_String ("general:description", Pkg.Description, Tmp)
         then
            return False;
         end if;

         --  Decode the list of authors

         if not Pop (Queue, Authors_Str, Tmp)
            or else not To_String_Vector ("general:authors", Pkg.Authors, Tmp)
         then
            return False;
         end if;

         --  Decode the list of maintainers

         if not Pop (Queue, Maintainers_Str, Tmp, True, General_Str)
            or else not To_String_Vector ("general:maintainers",
                                          Pkg.Maintainers, Tmp)
         then
            return False;
         end if;

         --  Decode the list of licenses

         if not Pop (Queue, Licenses_Str, Tmp, True, General_Str)
            or else not To_License_Vector ("general:licenses",
                                           Pkg.Licenses, Tmp)
         then
            return False;
         end if;

         --  Decode the optional website

         if Pop (Queue, Website_Str, Tmp)
            and then not To_String ("general:website", Pkg.Website, Tmp)
         then
            return False;
         end if;

         if not Process_Common (Queue, Pkg.Common, "general") then
            return False;
         end if;

         return Report_Extra_Keys ("general", Queue);
      end Process_General;

      ---------------------
      -- Process_Release --
      ---------------------

      function Process_Release
        (R : Release; Value : TOML.TOML_Value) return Boolean
      is
         Context : constant String := Semantic_Versioning.Image (R.Version);
         Queue : Key_Queue_Type := Key_Queue (Value, General_Str);
         Tmp   : TOML.TOML_Value;
      begin
         if not Result.Success then
            return False;
         end if;

         --  Decode the mandatory origin expression

         if not Pop (Queue, Origin_Str, Tmp, True, Context)
            or else not To_Origin_Expression
              (Context & ":origin", R.Origin, Tmp)
         then
            return False;
         end if;

         --  Decode the optional archive name string

         if Pop (Queue, Archive_Name_Str, Tmp)
            and then not To_String (Context & ":archive-name",
                                    R.Archive_Name, Tmp)
         then
            return False;
         end if;

         if not Process_Common (Queue, R.Common, Context) then
            return False;
         end if;

         return Report_Extra_Keys (Context, Queue);
      end Process_Release;

      Queue   : Key_Queue_Type;
      Key     : US.Unbounded_String;
      Tmp     : TOML.TOML_Value;
      Version : Semantic_Versioning.Version;
   begin
      Result := (Success => True);

      Pkg.Name := +Package_Name;

      --  Make sure the Value TOML document is an object that has at least a
      --  "general" entry, and then process it.

      Queue := Key_Queue (Value, "root");
      if not Result.Success then
         return;

      elsif not Pop (Queue, General_Str, Tmp, True, General_Str)
            or else not Process_General (Tmp)
      then
         return;
      end if;

      --  Then parse all releases

      while Pop_Next (Queue, Key, Tmp) loop

         --  First, decode the version number

         declare
            Version_Text : constant String := +Key;
         begin
            Version := Semantic_Versioning.Parse (Version_Text);
         exception
            when Constraint_Error =>
               Set_Error ("invalid version: " & Version_Text, "root");
               return;
         end;

         --  Then register it and decode the release data

         declare
            R : constant Release := new Release_Record'
              (Version => Version,
               others  => <>);
         begin
            Pkg.Releases.Insert (Version, R);
            if not Process_Release (R, Tmp) then
               return;
            end if;
         end;
      end loop;
   end Decode_TOML_Package;

   -------------------------
   -- Import_TOML_Package --
   -------------------------

   procedure Import_TOML_Package
     (Pkg         : Package_Type;
      Environment : Environment_Maps.Map)
   is
      Cat_Ent : constant Index.Catalog_Entry :=
         Index.Manually_Catalogued_Project
           (+Pkg.Name, "Alire.Index", +Pkg.Description);

      General_Dependencies : constant Dependencies_Result.T :=
         Dependencies_Expressions.Evaluate_Or_Default
           (Pkg.Common.Dependencies, Dependency_Maps.Empty_Map, Environment);
      --  Dependencies shared by all releases

      General_Properties : Index.Release_Properties :=
         Index.No_Properties;
      --  Properties shared by all releases

      General_Available : constant Boolean_Result.T :=
         Boolean_Expressions.Evaluate_Or_Default
           (Pkg.Common.Available, True, Environment);
      --  First filter for release availability

      R : Release;
      --  Release currently being imported

      Error_Message    : US.Unbounded_String;
      Evaluation_Error : exception;

      procedure Error (Message : US.Unbounded_String);
      --  Set Error_Message to Message and raise an Evaluation_Error exception

      procedure Add_Property
        (Properties   : in out Index.Release_Properties;
         New_Property : Index.Release_Properties);
      --  Helper to add New_Property to Properties

      function Origin return Origins.Origin;
      --  Return the origin for the current release

      function Dependencies return Index.Release_Dependencies;
      --  Return dependencies for the current release

      function Properties
        (Data : Common_Data) return Index.Release_Properties;
      --  Return properties to describe Data

      -----------
      -- Error --
      -----------

      procedure Error (Message : US.Unbounded_String) is
      begin
         Error_Message := Message;
         raise Evaluation_Error;
      end Error;

      ------------------
      -- Add_Property --
      ------------------

      procedure Add_Property
        (Properties   : in out Index.Release_Properties;
         New_Property : Index.Release_Properties)
      is
      begin
         Properties := Index."and" (Properties, New_Property);
      end Add_Property;

      ------------
      -- Origin --
      ------------

      function Origin return Origins.Origin is
         Label : constant Origin_Result.T :=
            Origin_Expressions.Evaluate (R.Origin, Environment);
         O     : Origin_Type;
      begin
         if not Label.Success then
            Error (Label.Error);
         end if;

         O := Label.Value;
         return
           (case O.Kind is
            when Git => Index.Git (+O.Repo_URL, +O.Revision),
            when Mercurial => Index.Hg (+O.Repo_URL, +O.Revision),
            when SVN => Index.SVN (+O.Repo_URL, +O.Revision),
            when Source_Archive => Index.Source_Archive
                                     (+O.Archive_URL, +R.Archive_Name),
            when Native_Package => Index.Native
              ((others => Index.Packaged_As (+O.Package_Name))));
      end Origin;

      ------------------
      -- Dependencies --
      ------------------

      function Dependencies return Index.Release_Dependencies is
         use Index;

         Release_Deps : constant Dependencies_Result.T :=
            Dependencies_Expressions.Evaluate_Or_Default
              (R.Common.Dependencies, Dependency_Maps.Empty_Map, Environment);
         Merged_Deps  : Dependencies_Result.T;

         Deps : Dependency_Maps.Map;

         Result : Release_Dependencies := No_Dependencies;
      begin
         --  Make sure we manage to evaluate and merge all dependencies

         if not Release_Deps.Success then
            Error (Release_Deps.Error);
         end if;
         Merge (General_Dependencies.Value, Release_Deps.Value,
                Merged_Deps);
         if not Merged_Deps.Success then
            Error (Merged_Deps.Error);
         end if;

         --  Then convert dependencies to Alire's internal format

         Deps.Move (Merged_Deps.Value);
         for Cursor in Deps.Iterate loop
            declare
               D        : constant Dependency :=
                  Dependency_Maps.Element (Cursor);
               Name     : constant Project := Project (+D.Name);
               Versions : constant Semantic_Versioning.Version_Set :=
                 (if D.Any
                  then Semantic_Versioning.Any
                  else D.Versions);
            begin
               Result := Result and Conditional.New_Dependency
                 (Name, Versions);
            end;
         end loop;
         return Result;
      end Dependencies;

      ----------------
      -- Properties --
      ----------------

      function Properties
        (Data : Common_Data) return Index.Release_Properties
      is
         use Index;

         Result : Release_Properties := No_Properties;
      begin
         --  Import notes as comments

         if US.Length (Data.Notes) > 0 then
            Add_Property (Result, Comment (+Data.Notes));
         end if;

         --  Import project files

         for PF of Data.Project_Files loop
            Add_Property (Result, Project_File (+PF));
         end loop;

         --  Import GPR externals

         for Cur in Data.GPR_Externals.Iterate loop
            declare
               Name         : constant String :=
                  +GPR_Externals_Maps.Key (Cur);
               Values       : String_Sets.Set renames
                  GPR_Externals_Maps.Element (Cur).all;
               Value_Vector : GPR.Value_Vector;
            begin
               for V of Values loop
                  Value_Vector := GPR."or" (Value_Vector, +V);
               end loop;
               Add_Property
                 (Result, (if Values.Is_Empty
                           then GPR_Free_Scenario (Name)
                           else GPR_Scenario (Name, Value_Vector)));
            end;
         end loop;

         --  Import default values for GPR externals

         for Cur in Data.GPR_Set_Externals.Iterate loop
            declare
               Name  : constant String := +GPR_Set_Externals_Maps.Key (Cur);
               Value : constant String_Result.T :=
                  String_Expressions.Evaluate
                    (GPR_Set_Externals_Maps.Element (Cur).all, Environment);
            begin
               if not Value.Success then
                  Error (Value.Error);
               end if;

               Add_Property (Result, GPR_External (Name, +Value.Value));
            end;
         end loop;

         --  Import executables

         for E of Data.Executables loop
            Add_Property (Result, Executable (+E));
         end loop;

         --  Import actions
         for A of Data.Actions loop
            Add_Property (Result, Action_Run
              (Moment           => A.Kind,
               Relative_Command => +A.Command));
         end loop;

         return Result;
      end Properties;

      function To_Requisite (B : Boolean) return Requisites.Tree is
        (if B
         then Requisites.Booleans.Always_True
         else Requisites.Booleans.Always_False);

   begin
      --  Make sure the global data is acceptable

      if not General_Dependencies.Success then
         Error (General_Dependencies.Error);
      elsif not General_Available.Success then
         Error (General_Available.Error);
      end if;

      --  Create general properties: website, authors, maintainers and licenses

      declare
         use Index;
      begin
         if US.Length (Pkg.Website) /= 0 then
            Add_Property (General_Properties, Website (+Pkg.Website));
         end if;

         for A of Pkg.Authors loop
            Add_Property (General_Properties, Author (+A));
         end loop;

         for M of Pkg.Maintainers loop
            Add_Property (General_Properties, Maintainer (+M));
         end loop;

         for L of Pkg.Licenses loop

            --  TODO: enhance internal data structures to handle custom
            --  licenses.

            if not L.Custom then
               Add_Property (General_Properties, License (L.License));
            end if;
         end loop;
      end;

      --  Register all releases in Pkg

      for Cursor in Pkg.Releases.Iterate loop
         R := Release_Maps.Element (Cursor);
         declare
            Release_Available : constant Boolean_Result.T :=
               Boolean_Expressions.Evaluate_Or_Default
                 (R.Common.Available, True, Environment);
            --  Second filter for release availability
         begin
            if not Release_Available.Success then
               Error (Release_Available.Error);
            end if;

            declare
               Dummy : constant Index.Release := Cat_Ent.Register
                 (Version        => R.Version,
                  Origin         => Origin,
                  Dependencies   => Dependencies,
                  Properties     => Index."and"
                    (Index."and" (General_Properties, Properties (Pkg.Common)),
                                  Properties (R.Common)),
                  Available_When => To_Requisite
                    (General_Available.Value
                     and then Release_Available.Value));
            begin
               null;
            end;
         end;
      end loop;

   exception
      when Evaluation_Error =>
         Trace.Error (+Error_Message);
         null;
   end Import_TOML_Package;

begin
   Expected_Index.Set ("version", TOML.Create_String ("1.0"));
end Alire.TOML_Index;
