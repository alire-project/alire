with Alr.OS_Lib;
with Alr.Utils;

with Interfaces.C;
with Interfaces.C.Strings;

package body Alr.OS is

   use Alr.OS_Lib.Paths;

   ------------------
   -- Cache_Folder --
   ------------------

   function Cache_Folder return String is
   begin
      return OS_Lib.Getenv ("XDG_CACHE_HOME",
                            Default => OS_Lib.Getenv ("HOME") / ".cache" / "alire");
   end Cache_Folder;

   -------------------
   -- Config_Folder --
   -------------------

   function Config_Folder return String is
   begin
      return OS_Lib.Getenv ("XDG_CONFIG_HOME",
                            Default => OS_Lib.Getenv ("HOME") / ".config" / "alire");
   end Config_Folder;

   --------------------------
   -- Create_Config_Folder --
   --------------------------

   procedure Create_Folder (Path : String) is
   begin
      OS_Lib.Spawn ("mkdir", "-p " & Path, Force_Quiet => True);
      --  FIXME not portable
   end Create_Folder;

   --------------------
   -- Os_Fingerprint --
   --------------------

   function Os_Fingerprint return String is
      Lines : constant Utils.String_Vector :=
                OS_Lib.Spawn_And_Capture ("lsb_release", "-d");
   begin
      return Lines.First_Element;
   end Os_Fingerprint;

   --------------------
   -- Own_Executable --
   --------------------

   function Own_Executable return String is
      -- (int buflen, char *buffer, int *len)
      use Interfaces;
      use type C.Size_T;

      --------------
      -- Readlink --
      --------------

      function Readlink (Path   : C.Strings.chars_ptr;
                         Buffer : out C.Char_Array;
                         Buflen :     C.size_t) return C.size_t;
      pragma Import (C, Readlink, "readlink");

      Buffer : aliased C.Char_Array (1 .. 1024);
      Used   : C.size_t;

      Link : aliased C.Char_Array := C.To_C (Linux_Self_Exec);
   begin
      Used := Readlink (C.Strings.To_Chars_Ptr (Link'Unchecked_Access),
                        Buffer,
                        Buffer'Length);

      return C.To_Ada (Buffer (Buffer'First .. Buffer'First + Used - 1), Trim_Nul => False);
   end Own_Executable;

end Alr.OS;
