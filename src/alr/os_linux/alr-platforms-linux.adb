with Alire.Origins.Deployers;
with Alire.Platform;

with Alr.OS_Lib;

with Interfaces.C;
with Interfaces.C.Strings;

package body Alr.Platforms.Linux is

   use Alr.OS_Lib.Paths;

   ------------------
   -- Cache_Folder --
   ------------------

   overriding function Cache_Folder (This : Linux_Variant) return String is
     (OS_Lib.Getenv ("XDG_CACHE_HOME",
                     Default => OS_Lib.Getenv ("HOME") / ".cache" / "alire"));

   -------------------
   -- Config_Folder --
   -------------------

   overriding function Config_Folder (This : Linux_Variant) return String is
     (OS_Lib.Getenv ("XDG_CONFIG_HOME",
                     Default => OS_Lib.Getenv ("HOME") / ".config" / "alire"));

   ------------------
   -- Distribution --
   ------------------

   overriding function Distribution (This : Linux_Variant)
                                     return Alire.Platforms.Distributions
   is (Alire.Platform.Distribution);

   --------------------
   -- Own_Executable --
   --------------------

   overriding
   function Own_Executable (This : Linux_Variant) return String is
      pragma Unreferenced (This);
      --   (int buflen, char *buffer, int *len)
      use Interfaces;
      use type C.size_t;

      --------------
      -- Readlink --
      --------------

      function Readlink (Path   : C.Strings.chars_ptr;
                         Buffer : out C.char_array;
                         Buflen :     C.size_t) return C.size_t;
      pragma Import (C, Readlink, "readlink");

      Buffer : aliased C.char_array (1 .. 1024);
      Used   : C.size_t;

      Link : aliased C.char_array := C.To_C (Linux_Self_Exec);
   begin
      Used := Readlink (C.Strings.To_Chars_Ptr (Link'Unchecked_Access),
                        Buffer,
                        Buffer'Length);

      return C.To_Ada (Buffer (Buffer'First .. Buffer'First + Used - 1),
                       Trim_Nul => False);
   end Own_Executable;

   ---------------------
   -- Package_Version --
   ---------------------

   function Package_Version (This   : Linux_Variant;
                             Origin : Alire.Origins.Origin)
                             return String
   is (Alire.Origins.Deployers.New_Deployer (Origin).Native_Version);

end Alr.Platforms.Linux;
