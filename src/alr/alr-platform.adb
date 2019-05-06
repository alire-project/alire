with Alire.Properties.Platform;

with Alr.OS_Lib;
with Alr.Utils;

with Interfaces.C;

with Semantic_Versioning;

package body Alr.Platform is

   --------------
   --  Singletons

   type Supported_Access is access Platforms.Supported'Class;
   Instance : Supported_Access;

   ---------------
   -- Am_I_Root --
   ---------------

   function Am_I_Root return Boolean is
      function GetEUID return Interfaces.C.int
        with Import, Convention => C, External_Name => "alr_geteuid";
   begin
      Trace.Debug ("UID=" & Utils.Trim (GetEUID'Img));
      return Integer (GetEUID) = 0;
   end Am_I_Root;

   ----------------------
   -- Basic_Properties --
   ----------------------

   package Platprop renames Alire.Properties.Platform;
   use all type Alire.Properties.Vector;

   function Properties return Alire.Properties.Vector
   is (Platprop.Compiler_Is (Compiler) and
         Platprop.Distribution_Is (Distribution) and
         Platprop.System_Is (Operating_System) and
         Platprop.Target_Is (Target) and
         Platprop.Version_Is (Distro_Version) and
         Platprop.Word_Size_Is (Word_Size));

   --------------
   -- Compiler --
   --------------

   Compiler_Cached       : Alire.Platforms.Compilers;
   Compiler_Cached_Valid : Boolean := False;

   function Compiler_Uncached return Alire.Platforms.Compilers is
      package Semver renames Semantic_Versioning;
      use all type Semver.Point;
      use Utils;

      Year   : Natural;
      Output : String_Vector;
   begin
      OS_Lib.Spawn_And_Capture (Output, "gnat");

      for Line of Output loop
         if Line'Length > 4
              and then
            Line (Line'First .. Line'First + 3) = "GNAT"
         then
            declare
               Version : constant String := To_Lower_Case (Tail (Line, ' '));
            begin
               if Contains (Version, "gpl") then
                  begin
                     Year := Natural'Value (Head (Tail (Version, ' '), ' '));
                     if Year < 2017 then
                        return GNAT_GPL_Old;
                     else
                        return GNAT_GPL_2017;
                     end if;
                  exception
                     when others =>
                        --  Somehow it doesn't follow the GPL XXXX (X)
                        --  convention.
                        return GNAT_GPL_Old;
                  end;
               elsif Contains (Version, "community") then
                  return GNAT_Community_2018;
               else
                  declare
                     V : Semver.Version;
                  begin
                     V := Semver.Parse (Version, Relaxed => False);

                     if Semver.Major (V) > 7 then
                        return GNAT_FSF_7_3_Or_Newer;
                     elsif Semver.Major (V) = 7 then
                        case Semver.Minor (V) is
                        when 0 | 1  => return GNAT_FSF_Old;
                        when 2      => return GNAT_FSF_7_2;
                        when others => return GNAT_FSF_7_3_Or_Newer;
                        end case;
                     else
                        return GNAT_FSF_Old;
                     end if;
                  exception
                     when others =>
                        --  Not a plain semantic version like FSF uses
                        return GNAT_Unknown;
                  end;
               end if;
            end;
         end if;
      end loop;

      Trace.Debug ("Unexpected output from gnat");
      return GNAT_Unknown;
   end Compiler_Uncached;

   function Compiler return Alire.Platforms.Compilers is
   begin
      if Compiler_Cached_Valid then
         return Compiler_Cached;
      else
         Compiler_Cached := Compiler_Uncached;
         Compiler_Cached_Valid := True;
         return Compiler_Cached;
      end if;
   end Compiler;

   ---------
   -- Get --
   ---------

   function Get return Platforms.Supported'Class is (Instance.all);

   ---------
   -- Set --
   ---------

   procedure Set (P : Platforms.Supported'Class) is
   begin
      Instance := new Platforms.Supported'Class'(P);
   end Set;

end Alr.Platform;
