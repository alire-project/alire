with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Alire.Errors;
with Alire.GPR;
with Alire.Projects.With_Releases;
with Alire.Utils;

with TOML;
use type TOML.Any_Value_Kind, TOML.TOML_Value;
with TOML.File_IO;

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
      Result                   : out Load_Result)
      with Pre => Result.Success;
   --  Load packages from all *.toml files in Catalog_Dir/Package_Dir

   procedure Load_From_Catalog_Internal
     (Catalog_Dir, Package_Name : String;
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
      TOML_Result : constant TOML.Read_Result :=
        TOML.File_IO.Load_File (Filename);
   begin
      if TOML_Result.Success then
         Result := Outcome_Success;
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
                    (Catalog_Dir, Simple_Name, Result);
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
                    (Catalog_Dir, Package_Name, Result);
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
      Result                    : out Load_Result)
   is
      Filename : constant String :=
         Dirs.Compose
           (Dirs.Compose (Catalog_Dir, Package_Directory (Package_Name)),
            Package_Name & ".toml");

      Value    : TOML.TOML_Value;
   begin
      Trace.Debug ("Loading " & Package_Name & " from " & Catalog_Dir);

      --  Load the TOML file

      Value := Load_TOML_From_File (Filename, Result);
      if not Result.Success then
         return;
      end if;

      --  Decode as Crate

      declare
         Crate  : Projects.With_Releases.Crate :=
                    Projects.With_Releases.New_Crate (+Package_Name);
      begin
         Result := Crate.From_TOML (TOML_Adapters.From
                                    (Value,
                                      Context => "Loading crate " & Filename));

         if Result.Success then
            --  Index_Crate (Filename, Crate);
            --  Incoming in future commit
            null;
         end if;
      end;
   end Load_From_Catalog_Internal;

   ----------------------------
   -- Load_Release_From_File --
   ----------------------------

   function Load_Release_From_File (Filename : String) return Releases.Release
   is
      Name : constant String :=
               Dirs.Base_Name (Dirs.Simple_Name (Filename));
      --  This file is requested by Alire so we don't need to check that it's a
      --  proper TOML name.

      --  Attempt to load the file
      Result : Load_Result;
      Value  : constant TOML.TOML_Value :=
                 Load_TOML_From_File (Filename, Result);
   begin
      if not Result.Success then
         raise Checked_Error with Errors.Set (Message (Result));
      end if;

      --  Parse the TOML structure
      declare
         Crate  : Projects.With_Releases.Crate :=
                    Projects.With_Releases.New_Crate (+Name);
         Result : constant Load_Result :=
                    Crate.From_TOML
                      (TOML_Adapters.From
                         (Value,
                          Context => "Loading crate " & Filename));
      begin
         if Result.Success then
            if Natural (Crate.Releases.Length) = 1 then
               return Crate.Releases.First_Element;
            else
               raise Checked_Error with Errors.Set
                 ("File " & Filename & " should contain a single release but "
                  & "contains" & Crate.Releases.Length'Img & " release(s)");
            end if;
         else
            raise Checked_Error with Errors.Set (Message (Result));
         end if;
      end;
   end Load_Release_From_File;

begin
   Expected_Index.Set ("version", TOML.Create_String ("1.0"));
end Alire.TOML_Index;
