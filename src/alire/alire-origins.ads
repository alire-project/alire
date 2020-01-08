with Alire.Hashes;
with Alire.Interfaces;
with Alire.Platforms;
with Alire.TOML_Adapters;

private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.Origins with Preelaborate is

   --------------------------------------------
   --  supporting types for native packages  --
   --------------------------------------------

   --  These are used to represent native packages in a comfortable way in the
   --  index

   type Package_Names is tagged private;

   function Image (This : Package_Names) return String;

   function Unavailable return Package_Names;

   function Packaged_As (Name : String) return Package_Names;

   type Native_Packages is array (Platforms.Distributions) of Package_Names;
   --  The name of a package in every distro for a given version

   type Kinds is
     (External,       -- A do-nothing origin, with some custom description
      Filesystem,     -- Not really an origin, but a working copy of a release
      Git,            -- Remote git repo
      Hg,             -- Remote hg repo
      SVN,            -- Remote svn repo
      Source_Archive, -- Remote source archive
      Native          -- Native platform package
     );

   type String_Access is access constant String;
   type Prefix_Array is array (Kinds) of String_Access;
   Prefixes : constant Prefix_Array;

   subtype VCS_Kinds is Kinds range Git .. SVN;

   type Source_Archive_Format is (Unknown, Tarball, Zip_Archive);
   subtype Known_Source_Archive_Format is
     Source_Archive_Format range Tarball .. Source_Archive_Format'Last;

   Unknown_Source_Archive_Format_Error : exception;

   type Origin is new
     Interfaces.Detomifiable and
     Interfaces.Tomifiable with private;

   function Kind (This : Origin) return Kinds;

   -------------------
   --  member data  --
   -------------------

   function Commit (This : Origin) return String
     with Pre => This.Kind in VCS_Kinds;
   function URL (This : Origin) return Alire.URL
     with Pre => This.Kind in VCS_Kinds;
   function URL_With_Commit (This : Origin) return Alire.URL
     with Pre => This.Kind in VCS_Kinds;
   --  Append commit as '@commit'

   function Path (This : Origin) return String
     with Pre => This.Kind = Filesystem;

   function Archive_URL (This : Origin) return Alire.URL
     with Pre => This.Kind = Source_Archive;
   function Archive_Name (This : Origin) return String
     with Pre => This.Kind = Source_Archive;
   function Archive_Format (This : Origin) return Known_Source_Archive_Format
     with Pre => This.Kind = Source_Archive;
   function Archive_Format (Name : String) return Source_Archive_Format;
   --  Guess the format of a source archive from its file name.

   function Is_Native (This : Origin) return Boolean is (This.Kind = Native);
   function Package_Name (This         : Origin;
                          Distribution : Platforms.Distributions)
                          return String;
   function All_Native_Names (This : Origin) return Native_Packages;

   function Short_Unique_Id (This : Origin) return String with
     Pre => This.Kind in Git | Hg | Source_Archive;

   --  Helper types

   subtype Git_Commit is String (1 .. 40);
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

   function New_Native (Packages : Native_Packages) return Origin;

   function Image (This : Origin) return String;

   procedure Add_Hash (This : in out Origin;
                       Hash :        Hashes.Any_Hash);

   function From_String
     (This   : out Origin;
      From   : String;
      Parent : TOML_Adapters.Key_Queue := TOML_Adapters.Empty_Queue;
      Hashed : Boolean := True)
      return Outcome;
   --  Parse a string and dispatch to the appropiate constructor.
   --  Parent is an optional parent TOML table that may contain extra fields
   --  (e.g., source_archive in case of an https: origin).
   --  Hashed indicates if integrity hashes are expected, so this function can
   --  be used to retrieve unhashed origins too (precisely for hashing).

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

   type Origin_Data (Kind : Kinds := Kinds'First) is record
      Hashes : Hash_Vectors.Vector;

      case Kind is
         when External =>
            Description : Unbounded_String;

         when Filesystem =>
            Path : Unbounded_String;

         when VCS_Kinds =>
            Repo_URL : Unbounded_String;
            Commit   : Unbounded_String;

         when Source_Archive =>
            Archive_URL    : Unbounded_String;
            Archive_Name   : Unbounded_String;
            Archive_Format : Known_Source_Archive_Format;

         when Native =>
            Packages : Native_Packages;
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

   function New_External (Description : String) return Origin is
      (Data => (External, Description => +Description, Hashes => <>));

   function New_Filesystem (Path : String) return Origin is
     (Data => (Filesystem, Path => +Path, Hashes => <>));

   function New_Git (URL    : Alire.URL;
                     Commit : Git_Commit)
                     return Origin is
     (Data => (Git, Repo_URL => +URL, Commit => +Commit, Hashes => <>));

   function New_Hg (URL    : Alire.URL;
                    Commit : Hg_Commit)
                    return Origin is
     (Data => (Hg, Repo_URL => +URL, Commit => +Commit, Hashes => <>));

   function New_SVN (URL : Alire.URL; Commit : String) return Origin is
     (Data => (SVN, Repo_URL => +URL, Commit => +Commit, Hashes => <>));

   function New_Native (Packages : Native_Packages) return Origin is
     (Data => (Native, Packages => Packages, Hashes => <>));

   function Kind (This : Origin) return Kinds is (This.Data.Kind);

   function URL    (This : Origin) return Alire.URL is
     (Alire.URL (+This.Data.Repo_URL));
   function Commit (This : Origin) return String is
     (+This.Data.Commit);
   function URL_With_Commit (This : Origin) return Alire.URL is
     (This.URL & "@" & This.Commit);

   function Path (This : Origin) return String is (+This.Data.Path);

   function Archive_URL (This : Origin) return Alire.URL is
     (+This.Data.Archive_URL);
   function Archive_Name (This : Origin) return String is
     (+This.Data.Archive_Name);
   function Archive_Format (This : Origin) return Known_Source_Archive_Format
   is (This.Data.Archive_Format);

   function Package_Name (This         : Origin;
                          Distribution : Platforms.Distributions)
                          return String is
     (+This.Data.Packages (Distribution).Name);

   function All_Native_Names (This : Origin) return Native_Packages is
     (This.Data.Packages);

   function S (Str : Unbounded_String) return String is (To_String (Str));

   function Image (This : Origin) return String is
     ((case This.Kind is
          when VCS_Kinds      =>
             "commit " & S (This.Data.Commit)
       & " from " & S (This.Data.Repo_URL),
          when Source_Archive =>
             "source archive " & (if S (This.Data.Archive_Name) /= ""
                                  then S (This.Data.Archive_Name) & " "
                                  else "")
       & "at " & S (This.Data.Archive_URL),
          when Native         =>
             "native package from platform software manager",
          when Filesystem     =>
             "path " & S (This.Data.Path),
          when External       =>
             "external " & S (This.Data.Description))
      & (if This.Data.Hashes.Is_Empty
         then ""
         elsif This.Data.Hashes.Last_Index = 1
         then " with hash " & This.Image_Of_Hashes
         else " with hashes " & This.Image_Of_Hashes)
     );

   Prefix_Git    : aliased constant String := "git+";
   Prefix_Hg     : aliased constant String := "hg+";
   Prefix_SVN    : aliased constant String := "svn+";
   Prefix_File   : aliased constant String := "file://";
   Prefix_Native : aliased constant String := "native:";

   Prefixes : constant Prefix_Array :=
                (Git            => Prefix_Git'Access,
                 Hg             => Prefix_Hg'Access,
                 SVN            => Prefix_SVN'Access,
                 External       => null,
                 Filesystem     => Prefix_File'Access,
                 Native         => Prefix_Native'Access,
                 Source_Archive => null);

end Alire.Origins;
