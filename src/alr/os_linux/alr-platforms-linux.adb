with Alr.Origins;
with Alr.OS_Lib;
with Alr.Utils;

with Interfaces.C;
with Interfaces.C.Strings;

package body Alr.Platforms.Linux is

   use Alr.OS_Lib.Paths;
   use Alr.Utils;

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

   Cached_Distro : Alire.Platforms.Distributions;
   Distro_Cached : Boolean := False;

   overriding function Distribution (This : Linux_Variant)
                                     return Alire.Platforms.Distributions
   is
      pragma Unreferenced (This);
   begin
      if Distro_Cached then
         return Cached_Distro;
      else
         declare
            use all type Alire.Platforms.Distributions;
            Release : String_Vector;
         begin
            OS_Lib.Spawn_And_Capture (Release, "lsb_release", "-is");
            for Known in Alire.Platforms.Distributions'Range loop
               for Line of Release loop
                  if Contains (To_Lower_Case (Known'Img), To_Lower_Case (Line))
                  then
                     Cached_Distro := Known;
                     Distro_Cached := True;
                     return Known;
                  end if;
               end loop;
            end loop;

            Trace.Debug ("Found unsupported distro: " & Release (1));

            Cached_Distro := Distro_Unknown;
            Distro_Cached := True;
            return Distro_Unknown;
         end;
      end if;
   end Distribution;

   --------------------
   -- Distro_Version --
   --------------------

   Cached_Version : Alire.Platforms.Versions;
   Version_Cached : Boolean := False;

   overriding function Distro_Version (This : Linux_Variant)
                                       return Alire.Platforms.Versions
   is
      pragma Unreferenced (This);
   begin
      if Version_Cached then
         return Cached_Version;
      else
         declare
            use all type Alire.Platforms.Versions;
            Release : String_Vector;
         begin
            OS_Lib.Spawn_And_Capture (Release, "lsb_release", "-cs");
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
            Cached_Version := Distro_Version_Unknown;
            return Distro_Version_Unknown;
         end;
      end if;
   end Distro_Version;

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
   is (Alr.Origins.New_Origin (Origin).Native_Version);

end Alr.Platforms.Linux;
