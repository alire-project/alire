private with Alire.Conditional_Trees.TOML_Load;
with Alire.Errors;
with Alire.Hashes;
with Alire.Interfaces;
with Alire.Properties;
with Alire.TOML_Adapters;
private with Alire.TOML_Keys;
with Alire.Utils.TTY;

private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.Origins is

   package TTY renames Alire.Utils.TTY;

   type Kinds is
     (Binary_Archive, -- A pre-compiled binary (dynamic expr + source archive)
      External,       -- A do-nothing origin, with some custom description
      Filesystem,     -- Not really an origin, but a working copy of a release
      Git,            -- Remote git repo
      Hg,             -- Remote hg repo
      SVN,            -- Remote svn repo
      Source_Archive, -- Remote source archive
      System          -- System package
     );

   type String_Access is access constant String;
   type Prefix_Array is array (Kinds) of String_Access;
   Prefixes : constant Prefix_Array;

   subtype Archive_Kinds is Kinds
     with Static_Predicate => Archive_Kinds in Binary_Archive | Source_Archive;

   subtype External_Kinds is Kinds
     with Static_Predicate => External_Kinds in External | System;

   subtype VCS_Kinds is Kinds range Git .. SVN;

   type Source_Archive_Format is (Unknown, Tarball, Zip_Archive);
   subtype Known_Source_Archive_Format is
     Source_Archive_Format range Tarball .. Source_Archive_Format'Last;

   Unknown_Source_Archive_Format_Error : exception;

   type Origin is new
     Interfaces.Detomifiable and
     Interfaces.Tomifiable with private;

   function Kind (This : Origin) return Kinds;

   function Whenever (This : Origin; Env : Properties.Vector) return Origin;
   --  Resolve expressions in the origin

   -------------------
   --  member data  --
   -------------------

   function Commit (This : Origin) return String
     with Pre => This.Kind in VCS_Kinds;
   function URL (This : Origin) return Alire.URL
     with Pre => This.Kind in VCS_Kinds;
   function URL_With_Commit (This : Origin) return Alire.URL
     with Pre => This.Kind in VCS_Kinds;
   --  Append commit as '#commit'
   function TTY_URL_With_Commit (This : Origin) return String
     with Pre => This.Kind in VCS_Kinds;

   function Path (This : Origin) return String
     with Pre => This.Kind = Filesystem;

   function Archive_URL (This : Origin) return Alire.URL
     with Pre => This.Kind in Archive_Kinds;
   function Archive_Name (This : Origin) return String
     with Pre => This.Kind in Archive_Kinds;
   function Archive_Format (This : Origin) return Known_Source_Archive_Format
     with Pre => This.Kind in Archive_Kinds;
   function Archive_Format (Name : String) return Source_Archive_Format;
   --  Guess the format of a source archive from its file name.

   function Get_URL (This : Origin) return Alire.URL with
     Pre => This.Kind in Filesystem | Source_Archive | VCS_Kinds;

   function Is_System (This : Origin) return Boolean is (This.Kind = System);
   function Package_Name (This : Origin) return String
     with Pre => This.Kind = System;

   function Is_Regular (This : Origin) return Boolean is
     (This.Kind not in External | System);
   --  A regular origin is one that is compiled from sources, instead of coming
   --  from external definitions (detected or not).

   function Short_Unique_Id (This : Origin) return String with
     Pre => This.Kind in Git | Hg | Archive_Kinds;

   --  Helper types

   subtype Git_Commit is String (1 .. 40) with
     Dynamic_Predicate =>
       (for all Char of Git_Commit => Char in '0' .. '9' | 'a' .. 'f');

   subtype Hg_Commit  is String (1 .. 40);

   --  Constructors

   function New_External (Description : String) return Origin;

   function New_Filesystem (Path : String) return Origin;

   function New_Git (URL    : Alire.URL;
                     Commit : Git_Commit)
                     return Origin;

   function New_Hg (URL    : Alire.URL;
                    Commit : Hg_Commit)
                    return Origin;

   function New_SVN (URL : Alire.URL; Commit : String) return Origin;

   function New_VCS (URL : Alire.URL; Commit : String) return Origin;
   --  Attempt to identify an origin kind from the transport (git+https). If no
   --  VCS specified, look for ".git" extension.

   Unknown_Source_Archive_Name_Error : exception;

   function New_Source_Archive
     (URL  : Alire.URL;
      Name : String := "") return Origin;
   --  Create a reference to a source archive to be downloaded and extracted.
   --  URL is the address of the archive to download. Name is the name of the
   --  file to download.
   --
   --  This raises an Unknown_Source_Archive_Format_Error exception when we
   --  either cannot deduce the archive format from its filename or when the
   --  archive format is unknown.
   --
   --  If Name is omitted, it is tentatively inferred from URL. If it cannot be
   --  inferred, this raises a Unknown_Source_Archive_Name_Error exception.

   function New_System (System_Package_Name : String) return Origin;
   --  A system origin points to a single system package known to exist,
   --  already detected by some External.

   function Image (This : Origin) return String;

   procedure Add_Hash (This : in out Origin;
                       Hash :        Hashes.Any_Hash);

   function From_String (Image : String) return Origin with
     Post => From_String'Result.Kind in Filesystem | Source_Archive;
   --  Parse a string and dispatch to the appropriate constructor. This
   --  function can be used to retrieve unhashed origins too (precisely
   --  for hashing).

   overriding
   function From_TOML (This : in out Origin;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome;
   --  Pops "origin" from From.

   overriding function To_TOML (This : Origin) return TOML.TOML_Value with
     Post => To_TOML'Result.Kind = TOML.TOML_Table;

private

   use Ada.Strings.Unbounded;

   use all type Hashes.Any_Hash;

   package Hash_Vectors is new
     Ada.Containers.Indefinite_Vectors (Positive, Hashes.Any_Hash);

   function Get_Hashes (This : Origin) return Hash_Vectors.Vector;
   --  Ugly Get_ but it avoids lots of ambiguities down the line

   function "+" (S : String) return Unbounded_String
   renames To_Unbounded_String;

   function "+" (U : Unbounded_String) return String
   renames To_String;

   type Package_Names is tagged record
      Name : Unbounded_String;
   end record;

   function Image (This : Package_Names) return String is (+This.Name);

   function Unavailable return Package_Names
   is (Name => Null_Unbounded_String);

   function Packaged_As (Name : String) return Package_Names
   is (Name => +Name);

   function S (Str : Unbounded_String) return String is (To_String (Str));

   type Archive_Data is
     new Interfaces.Classificable
     and Interfaces.Tomifiable
     and Interfaces.Yamlable with
   record
      URL    : Unbounded_String;
      Name   : Unbounded_String;
      Format : Known_Source_Archive_Format;
      Hashes : Hash_Vectors.Vector;
      Binary : Boolean;
   end record;

   overriding
   function Key (This : Archive_Data) return String is (TOML_Keys.Origin);

   overriding
   function To_TOML (This : Archive_Data) return TOML.TOML_Value;

   overriding
   function To_YAML (This : Archive_Data) return String
   is (raise Unimplemented with Errors.Set ("Should not be needed"));

   function Image (Archive : Archive_Data;
                   Kind    : Archive_Kinds) return String
   is ((if Kind in Source_Archive
        then "source archive "
        else "binary archive ")
       & (if S (Archive.Name) /= ""
          then S (Archive.Name) & " "
          else "")
       & "at " & S (Archive.URL));

   function Binary_Image (Archive : Archive_Data) return String
   is (Image (Archive, Binary_Archive));

   function Source_Image (Archive : Archive_Data) return String
   is (Image (Archive, Source_Archive));

   package Conditional_Archives is
     new Conditional_Trees (Values => Archive_Data,
                            Image  => Binary_Image);

   type Conditional_Archive is new Conditional_Archives.Tree with null record;
   package Binary_Loader is new Conditional_Archives.TOML_Load;

   function As_Data (This : Conditional_Archive) return Archive_Data'Class;

   type Origin_Data (Kind : Kinds := External) is record
      case Kind is
         when Binary_Archive =>
            Bin_Archive : Conditional_Archive;

         when External =>
            Description : Unbounded_String;

         when Filesystem =>
            Path   : Unbounded_String;
            Hashes : Hash_Vectors.Vector;

         when VCS_Kinds =>
            Repo_URL : Unbounded_String;
            Commit   : Unbounded_String;

         when Source_Archive =>
            Src_Archive : Archive_Data;

         when System =>
            Package_Name : Unbounded_String;
      end case;
   end record;

   type Origin
   is new
     Interfaces.Detomifiable and
     Interfaces.Tomifiable
   with record
      Data : Origin_Data;
   end record;

   function Image_Of_Hashes (This : Origin) return String;

   Prefix_External : aliased constant String := "external:";
   Prefix_Git      : aliased constant String := "git+";
   Prefix_Hg       : aliased constant String := "hg+";
   Prefix_SVN      : aliased constant String := "svn+";
   Prefix_File     : aliased constant String := "file://";
   Prefix_System   : aliased constant String := "system:";

   Prefixes : constant Prefix_Array :=
                (Git            => Prefix_Git'Access,
                 Hg             => Prefix_Hg'Access,
                 SVN            => Prefix_SVN'Access,
                 External       => Prefix_External'Access,
                 Filesystem     => Prefix_File'Access,
                 System         => Prefix_System'Access,
                 Archive_Kinds  => null);

end Alire.Origins;
