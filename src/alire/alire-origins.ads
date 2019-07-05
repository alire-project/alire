with Alire.Interfaces;
with Alire.Platforms;
with Alire.Utils;

private with Ada.Strings.Unbounded;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.Origins with Preelaborate is

   --  Minimal information about origins of sources. We use the term origins to
   --  avoid mixing 'alire sources' with 'project sources' or other 'sources'.

   --  The actual capabilities for check-outs or fetches are in alr proper

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
     (Filesystem,     -- Not really an origin, but a working copy of a project
      Git,            -- Remote git repo
      Hg,             -- Remote hg repo
      SVN,            -- Remote svn repo
      Source_Archive, -- Remote source archive
      Native          -- Native platform package
     );

   subtype VCS_Kinds is Kinds range Git .. SVN;

   type Source_Archive_Format is (Unknown, Tarball, Zip_Archive);
   subtype Known_Source_Archive_Format is
     Source_Archive_Format range Tarball .. Source_Archive_Format'Last;

   Unknown_Source_Archive_Format_Error : exception;

   type Origin is new
     Interfaces.Codifiable and
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

   function Is_Native (This : Origin) return Boolean is (This.Kind = Native);
   function Package_Name (This         : Origin;
                          Distribution : Platforms.Distributions)
                          return String;
   function All_Native_Names (This : Origin) return Native_Packages;

   --  Helper types

   subtype Git_Commit is String (1 .. 40);
   subtype Hg_Commit  is String (1 .. 40);

   --  Constructors

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
     (URL : Alire.URL; Name : String := "") return Origin;
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

   overriding function To_Code (This : Origin) return Utils.String_Vector;

   overriding function To_TOML (This : Origin) return TOML.TOML_Value with
     Post => To_TOML'Result.Kind = TOML.TOML_Table;

private

   use Ada.Strings.Unbounded;

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
      case Kind is
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
   is new Interfaces.Codifiable and Interfaces.Tomifiable
   with record
      Data : Origin_Data;
   end record;

   function New_Filesystem (Path : String) return Origin is
     (Data => (Filesystem, Path => +Path));

   function New_Git (URL    : Alire.URL;
                     Commit : Git_Commit)
                     return Origin is
     (Data => (Git, +URL, +Commit));

   function New_Hg (URL    : Alire.URL;
                    Commit : Hg_Commit)
                    return Origin is
     (Data => (Hg, +URL, +Commit));

   function New_SVN (URL : Alire.URL; Commit : String) return Origin is
     (Data => (SVN, +URL, +Commit));

   function New_Native (Packages : Native_Packages) return Origin is
     (Data => (Native, Packages));

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
     (case This.Kind is
         when VCS_Kinds =>
            "commit " & S (This.Data.Commit)
            & " from " & S (This.Data.Repo_URL),
         when Source_Archive =>
            "source archive " & S (This.Data.Archive_Name)
            & " at " & S (This.Data.Archive_URL),
         when Native =>
            "native package from platform software manager",
         when Filesystem =>
            "path " & S (This.Data.Path));

   overriding function To_Code (This : Origin) return Utils.String_Vector is
     (if This.Kind = Filesystem
      then Utils.To_Vector (Path (This))
      else raise Program_Error with "Unimplemented");

end Alire.Origins;
