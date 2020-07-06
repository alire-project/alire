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
      use all type Alire.Platforms.Distributions;
   begin
      if Distro_Cached then
         return Cached_Distro;
      else
         declare
            use Utils;
            Release : constant Utils.String_Vector :=
                        Subprocess.Checked_Spawn_And_Capture
                          ("cat", Empty_Vector & "/etc/os-release");
         begin
            for Line of Release loop
               declare
                  Normalized : constant String :=
                                 To_Lower_Case (Replace (Line, " ", ""));
               begin
                  if Starts_With (Normalized, "id=") then
                     Cached_Distro :=
                       Platforms.Distributions'Value (Tail (Normalized, '='));
                     Distro_Cached := True;
                     return Cached_Distro;
                  end if;
               exception
                  when others =>
                     exit; -- Not a known distro.
               end;
            end loop;

            Trace.Debug ("Found unsupported distro: " & Release (1));

            Cached_Distro := Distro_Unknown;
            Distro_Cached := True;
            return Distro_Unknown;
         end;
      end if;
   exception
      when E : Checked_Error =>
         Trace.Debug ("Unable to detect distribution:");
         Log_Exception (E);
         return Distro_Unknown;
   end Distribution;

   -----------------------
   -- Distribution_Root --
   -----------------------

   function Distribution_Root return Absolute_Path
   is ("/");

end Alire.Platform;
