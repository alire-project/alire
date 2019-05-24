with Alire.Interfaces;
with Alire.OS_Lib;
with Alire.TOML_Index;

with TOML;

package Alire.Index_On_Disk is

   --  Root abstract type that describes the operations available on a folder
   --  that contains an index.
   --  See child packages for actual implementations.
   --  Some common operations are provided here with classwide parameter.

   --  Index metadata are stored in the <config>/indexes/<unique_id>/index.toml
   --  Actual index is stored in <config>/indexes/<name>/repo

   --  URLs given to New_Handler functions must be complete, commit optional:
   --  E.g.: git+https://path/to/server/and/project[@commit]
   --  E.g.: file:///path/to/local/folder

   Checkout_Directory : constant String := "repo";
   Metadata_Filename  : constant String := "index.toml";

   type Index (<>) is abstract new Interfaces.Tomifiable with private;

   pragma Warnings (Off); -- because Post doesn't mention New_Handler'Result
   function New_Handler (Origin :     URL;
                         Name   :     String;
                         Parent :     Platform_Independent_Path;
                         Result : out Outcome) return Index'Class
     with Post => Name in Restricted_Name or not Result.Success;
   pragma Warnings (On);
   --  Factory function.
   --  Name is a user given name used to denote the index (like a git origin).
   --  Parent is the folder that contains all indexes.
   --  If not Result.Success, the returned index is invalid and will raise
   --  Program_Error in all attempted operations.

   function New_Handler (Origin : URL;
                         Name   : Restricted_Name;
                         Parent : Platform_Independent_Path)
                         return Index is abstract;
   --  Descendants use this function to initialize a URL-specific handler

   function Add (This : Index) return Outcome is abstract;
   --  Deploy the index on disk for the first time at <Parent>/<Name>/repo

   function Delete (This : Index'Class) return Outcome;
   --  Remove index from current configuration and delete its folder

   function Index_Directory (This : Index'Class) return String;
   --  Returns the actual path in which the index is to be checked out,
   --  i.e., <config>/indexes/<Parent>/<Name>/repo

   function Metadata_Directory (This : Index'Class) return String;
   --  Returns the parent of the checkout directory

   function Load (This : Index'Class;
                  Env  : Alire.TOML_Index.Environment_Variables)
                  return Outcome;
   --  Loads the actual index contents into the in-memory index

   function Name (This : Index'Class) return Restricted_Name;
   --  Returns the user's given Name for the index

   function Origin (This : Index) return URL;
   --  A unique string that describes where to find this index (git, dir...).

   function Update (This : Index) return Outcome is abstract;
   --  If the index allows updating (e.g. git), do it.
   --  Otherwise, silently do nothing and return success.

   overriding
   function To_TOML (This : Index) return TOML.TOML_Value;
   --  Index metadata in TOML format

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
      end record;

   use OS_Lib.Operators;

   function Index_Directory (This : Index'Class) return String is
     (This.Parent / This.Name / Checkout_Directory);

   function Metadata_Directory (This : Index'Class) return String is
     (This.Parent / This.Name);

   function Origin (This : Index) return URL is (This.Origin);

   function "<" (L, R : Index'Class) return Boolean is (L.Origin < R.Origin);

end Alire.Index_On_Disk;
