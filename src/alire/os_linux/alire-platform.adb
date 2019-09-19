with Alire.OS_Lib.Subprocess;
with Alire.Utils;

package body Alire.Platform is

   --  Linux implementation

   ---------------------------
   -- Default_Config_Folder --
   ---------------------------

   function Default_Config_Folder return String is
      use OS_Lib;
   begin
      return (OS_Lib.Getenv ("XDG_CONFIG_HOME",
              Default => OS_Lib.Getenv ("HOME") / ".config" / "alire"));
   end Default_Config_Folder;

   ------------------
   -- Distribution --
   ------------------

   Cached_Distro : Alire.Platforms.Distributions;
   Distro_Cached : Boolean := False;

   function Distribution return Platforms.Distributions is
      use Alire.OS_Lib;
   begin
      if Distro_Cached then
         return Cached_Distro;
      else
         declare
            use all type Alire.Platforms.Distributions;
            use Utils;
            Release : Utils.String_Vector;
         begin
            if Subprocess.Spawn_And_Capture (Release,
                                             "lsb_release", "-is") /= 0
            then
               Trace.Debug ("Unable to detect distribution");
               return Distro_Unknown;
            end if;

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

   ----------
   -- Name --
   ----------

   function Name return Supported is (Linux);

end Alire.Platform;
