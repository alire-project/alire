with AAA.Strings; use AAA.Strings;

with Alire.Platforms.Common;

with GNAT.Regpat;

package body Alire.Platforms.Current is

   --  Linux implementation

   use OS_Lib.Operators; -- Bring in "/" for paths

   ------------------
   -- Cache_Folder --
   ------------------

   function Cache_Folder return String is (Common.Config_Folder);

   -------------------
   -- Config_Folder --
   -------------------

   function Config_Folder return String is (Common.Config_Folder);

   ---------------------------
   -- Detected_Distribution --
   ---------------------------

   OS_Identity_File : constant String := "/etc/os-release";

   Cached_Distro : Alire.Platforms.Distributions;
   Distro_Cached : Boolean := False;

   function Detected_Distribution return Platforms.Distributions is
      use Alire.OS_Lib;
   begin
      if Distro_Cached then
         return Cached_Distro;
      elsif not GNAT.OS_Lib.Is_Regular_File (OS_Identity_File) then
         Trace.Debug ("Distribution identity file not found: "
                      & OS_Identity_File);
         Distro_Cached := True;
         Cached_Distro := Distro_Unknown;
         return Cached_Distro;
      else
         declare
            Release : constant AAA.Strings.Vector :=
                        Subprocess.Checked_Spawn_And_Capture
                ("cat", Empty_Vector & OS_Identity_File);

            function Get_Os_Release_Value_For_Key (Key : String)
                                      return Alire.Platforms.Distributions is

               use GNAT.Regpat;

               --  Regexp accepting lines not starting with '#' like:
               --  key=value
               --  key='value'
               --  key='value1 value2'
               --  key="value"
               --  key="value1 value2"
               Regexp : constant Pattern_Matcher :=
                 Compile ("^" & Key & "=[""']?([^""']+)[""']?");
               Matches : Match_Array (1 .. 1);

            begin
               for Line of Release loop
                  declare
                     Normalized : constant String :=
                       To_Lower_Case (Line);

                     Values : AAA.Strings.Vector;
                  begin
                     Match (Regexp, Normalized, Matches);
                     if Matches (1) /= No_Match then
                        --  Generate Values from space separated items
                        Values :=
                          Split (
                            Normalized
                            (Matches (1).First .. Matches (1).Last), ' ');

                        for Value of Values loop
                           begin
                              return Platforms.Distributions'Value
                                (Value);
                           exception
                              when others =>
                                 null; -- Not a known distro.
                           end;
                        end loop;
                     end if;
                  exception
                     when others =>
                        null; -- Not a known distro.
                  end;
               end loop;

               return Distro_Unknown;

            end Get_Os_Release_Value_For_Key;

         begin
            --  First try with id key
            Cached_Distro :=
              Get_Os_Release_Value_For_Key (Key => "id");

            --  If no supported distribution found, fallback to id_like key
            if Cached_Distro = Distro_Unknown then
               Trace.Debug
                 ("Unknown distro for key 'id', falling back to 'id_like'");
               Cached_Distro :=
                 Get_Os_Release_Value_For_Key (Key => "id_like");
            end if;

            --  Still an unsupported distribution ?
            if Cached_Distro = Distro_Unknown then
               Trace.Debug ("Found unsupported distro: " & Release (1));
            end if;

            Distro_Cached := True;
            return Cached_Distro;
         end;
      end if;
   exception
      when E : Checked_Error =>
         Trace.Debug ("Unable to detect distribution:");
         Log_Exception (E);
         return Distro_Unknown;
   end Detected_Distribution;

   -----------------------
   -- Distribution_Root --
   -----------------------

   function Distribution_Root return Absolute_Path
   is ("/");

   ----------------------
   -- Load_Environment --
   ----------------------

   procedure Load_Environment (Ctx : in out Alire.Environment.Context)
   is null;

   ----------------------
   -- Operating_System --
   ----------------------

   function Operating_System return Alire.Platforms.Operating_Systems
   is (Alire.Platforms.Linux);

end Alire.Platforms.Current;
