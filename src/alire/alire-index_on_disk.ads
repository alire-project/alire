with Alire.Interfaces;
with Alire.OS_Lib;

with TOML;

package Alire.Index_On_Disk is

   --  Root abstract type that describes the operations available on a folder
   --  that contains an index.
   --  See child packages for actual implementations.
   --  Some common operations are provided here with classwide parameter.

   --  Index metadata are stored in the <config>/indexes/<unique_id>/index.toml
   --  Actual index is stored in <config>/indexes/<name>/repo

   --  URLs given to New_Handler functions must be complete, commit optional:
   --  E.g.: git+https://path/to/server/and/project[#commit]
   --  E.g.: file:///path/to/local/folder

   Checkout_Directory : constant String := "repo";
   Metadata_Filename  : constant String := "index.toml";

   File_Prefix        : constant String := "file://";
   HTTP_Prefix        : constant String := "http";

   subtype Priorities is Integer; -- Lower is loaded before

   Default_Priority   : constant := 1;

   type Index (<>) is abstract new Interfaces.Tomifiable with private;

   pragma Warnings (Off, "postcondition does not mention function result");
   function New_Handler (Origin   :     URL;
                         Name     :     String;
                         Parent   :     Any_Path;
                         Result   : out Outcome;
                         Priority :     Priorities := Default_Priority)
                         return Index'Class
     with Post => Name in Restricted_Name or not Result.Success;
   pragma Warnings (On);
   --  Factory function.
   --  Name is a user given name used to denote the index (like a git origin).
   --  Parent is the folder that contains all indexes.
   --  If not Result.Success, the returned index is invalid and will raise
   --  Program_Error in all attempted operations.

   function New_Handler (From   :     TOML.TOML_Value;
                         Parent :     Any_Path;
                         Result : out Outcome) return Index'Class with
     Pre => From.Kind in TOML.TOML_Table;
   --  Load from an output Index.To_TOML value

   function New_Handler (Origin :     URL;
                         Name   :     Restricted_Name;
                         Parent :     Any_Path)
                         return Index is abstract;
   --  Descendants use this function to initialize a URL-specific handler

   function Add (This : Index) return Outcome is abstract;
   --  Deploy the index on disk for the first time at <Parent>/<Name>/repo.
   --  This version only writes the actual index contents.

   function Add_With_Metadata (This : Index'Class) return Outcome;
   --  Creates destination dir, writes metadata, adds the contents, and
   --  verifies the result (the whole shebang).

   function Delete (This : Index'Class) return Outcome;
   --  Remove index from current configuration and delete its folder

   function Load (This : Index'Class; Strict : Boolean) return Outcome;
   --  Loads the actual index contents into the in-memory index

   procedure Load (This : Index'Class; Crate : Crate_Name; Strict : Boolean);
   --  Load releases for just one crate from the index

   function Update (This : Index) return Outcome is abstract;
   --  If the index allows updating (e.g. git), do it.
   --  Otherwise, silently do nothing and return success, since at this level
   --  there is no way to know if an indexed can be updated, and is always
   --  called.

   function Verify (This : Index'Class) return Outcome;
   --  Ascertain if an index is properly populated (metadata, crates);

   function Write_Metadata (This     : Index'Class;
                            Filename : String) return Outcome;
   --  Write metadata to given file

   -----------------------
   -- Index information --
   -----------------------

   function Index_Directory (This : Index) return String;
   --  Returns the actual path in which the index is to be checked out,
   --  i.e., <config>/indexes/<Parent>/<Name>/repo

   function Metadata_Directory (This : Index'Class) return String;
   --  Returns the parent of the checkout directory

   function Metadata_File (This : Index'Class) return String;
   --  Returns the metadata file in the parent of the checkout directory

   function Name (This : Index'Class) return Restricted_Name;
   --  Returns the user's given Name for the index

   function Origin (This : Index) return URL;
   --  A unique string that describes where to find this index (git, dir...).

   function Priority (This : Index'Class) return Priorities;

   ---------------
   -- Utilities --
   ---------------

   overriding
   function To_TOML (This : Index) return TOML.TOML_Value;
   --  Index metadata in TOML format

   function With_Priority (This     : Index'Class;
                           Priority : Priorities) return Index'Class;

   function "<" (L, R : Index'Class) return Boolean;
   --  Useful later for collections of indexes

private

   type Index (URL_Len  : Natural;
               Name_Len : Natural;
               Dir_Len  : Natural) is abstract new Interfaces.Tomifiable with
      record
         Origin : URL (1 .. URL_Len);
         Name   : Restricted_Name (1 .. Name_Len);
         Parent : String (1 .. Dir_Len);
         Priority : Priorities := Default_Priority; -- Lower is better
      end record;

   use OS_Lib.Operators;

   function Index_Directory (This : Index) return String is
     (This.Parent / This.Name / Checkout_Directory);

   function Metadata_Directory (This : Index'Class) return String is
     (This.Parent / This.Name);

   function Metadata_File (This : Index'Class) return String is
     (This.Metadata_Directory / Metadata_Filename);

   function Origin (This : Index) return URL is (This.Origin);

   function Priority (This : Index'Class) return Priorities is (This.Priority);

   function "<" (L, R : Index'Class) return Boolean is
     (L.Priority < R.Priority or else
      (L.Priority = R.Priority and then L.Origin < R.Origin));

end Alire.Index_On_Disk;
