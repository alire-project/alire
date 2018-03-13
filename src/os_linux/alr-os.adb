with Alr.OS_Lib;

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

   ------------------
   -- Distribution --
   ------------------

   Cached_Distro : Alire.Platforms.Distributions;
   Distro_Cached : Boolean := False;

   function Distribution return Alire.Platforms.Distributions is
   begin
      if Distro_Cached then
         return Cached_Distro;
      else
         declare
            use all type Alire.Platforms.Distributions;
            Release : constant String_Vector := OS_Lib.Spawn_And_Capture ("lsb_release", "-is");
         begin
            for Known in Alire.Platforms.Distributions'Range loop
               for Line of Release loop
                  if Contains (To_Lower_Case (Known'Img), To_Lower_Case (Line)) then
                     Cached_Distro := Known;
                     Distro_Cached := True;
                     return Known;
                  end if;
               end loop;
            end loop;

            Trace.Debug ("Found unsupported distro: " & Release (1));

            Cached_Distro := Unsupported;
            Distro_Cached := True;
            return Unsupported;
         end;
      end if;
   end Distribution;

   ----------------------
   -- Operating_System --
   ----------------------

   function Operating_System return Alire.Platforms.Operating_Systems is (Alire.Platforms.GNU_Linux);

   -------------
   -- Version --
   -------------

   Cached_Version : Alire.Platforms.Versions;
   Version_Cached : Boolean := False;

   function Version return Alire.Platforms.Versions is
   begin
      if Version_Cached then
         return Cached_Version;
      else
         declare
            use all type Alire.Platforms.Versions;
            Release : constant String_Vector := OS_Lib.Spawn_And_Capture ("lsb_release", "-cs");
         begin
            for Known in Alire.Platforms.Versions'Range loop
               for Line of Release loop
                  if Contains (To_Lower_Case (Known'Img), Line) then
                     Version_Cached := True;
                     Cached_Version := Known;
                     return Known;
                  end if;
               end loop;
            end loop;

            Trace.Debug ("Found unsupported version: " & Release (1));

            Version_Cached := True;
            Cached_Version := Unsupported;
            return Unsupported;
         end;
      end if;
   end Version;

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
