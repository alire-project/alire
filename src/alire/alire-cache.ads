with Ada.Containers.Indefinite_Ordered_Multisets;
with Ada.Directories;

package Alire.Cache is

   --  Cache inspection and management. The cache is where we store all data
   --  that, if not found, is re-downloaded or regenerated. This currently
   --  comprises toolchains, pristine releases (the vault), builds, and the
   --  user index fork clone when publishing.

   function Path return Absolute_Path;
   --  The location for data that will be recreated if missing; its value in
   --  precedence order is:
   --  1) Setting builtin 'cache.dir'
   --  2) if Alire.Settings.Path is overridden, Settings.Path/cache
   --  3) Platforms.Folders.Cache

   subtype Sizes is Ada.Directories.File_Size;
   --  A size, in bytes

   --  The following builds a tree of items in the cache, that can be queried
   --  to present information up to a level of detail.

   type Depths is (Location, Release, Build);
   --  Locations are the top-level folders: toolchains, releases, builds.
   --  Releases are a unique release milestone plus short commit.
   --  Builds are synced copies for a release, named as the release + build id.

   type Base_Item is abstract tagged null record;

   function "<" (L, R : Base_Item'Class) return Boolean;

   function Depth (This : Base_Item'Class) return Depths;

   function Name (This : Base_Item'Class) return String;

   function Path (This : Base_Item'Class) return Absolute_Path;

   function Size (This : Base_Item'Class) return Sizes;

   package Item_Sets is
     new Ada.Containers.Indefinite_Ordered_Multisets (Base_Item'Class);

   subtype Usages is Item_Sets.Set;

   function Children (This : Base_Item'Class) return Usages;

   function Usage return Usages;
   --  Compute cache usage. First level is locations, second level is releases,
   --  third level is builds. Within level, childen are sorted by size.

   type Item is new Base_Item with record
      Depth    : Depths;
      Name     : UString;
      Path     : Unbounded_Absolute_Path;
      Size     : Sizes; -- Accumulated size below this item
      Children : Usages;
   end record;

   function Element (This : Base_Item'Class) return Item is (Item (This))
     with Inline;

private

   use type Sizes;

   --------------
   -- Children --
   --------------

   function Children (This : Base_Item'Class) return Usages
   is (This.Element.Children);

   -----------
   -- Depth --
   -----------

   function Depth (This : Base_Item'Class) return Depths
   is (This.Element.Depth);

   ----------
   -- Name --
   ----------

   function Name (This : Base_Item'Class) return String
   is (UStrings.To_String (This.Element.Name));

   ----------
   -- Path --
   ----------

   function Path (This : Base_Item'Class) return Absolute_Path
   is (Absolute_Path (UStrings.To_String (This.Element.Path)));

   ----------
   -- Size --
   ----------

   function Size (This : Base_Item'Class) return Sizes is (This.Element.Size);

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Base_Item'Class) return Boolean
   is (L.Size > R.Size
       or else
         (L.Size = R.Size
          and then L.Name < R.Name));

end Alire.Cache;
