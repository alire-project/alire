with Alire.Properties.Platform;

with Alr.OS_Lib;
with Alr.Utils;

with Semantic_Versioning;

package body Alr.Platform is

   -------------
   -- Singletons

   Instance : access Platforms.Supported'Class;

   Props    : access Alire.Properties.Vector;

   ----------------------
   -- Basic_Properties --
   ----------------------

   package Platprop renames Alire.Properties.Platform;
   use all type Alire.Properties.Vector;

   function Basic_Properties return Alire.Properties.Vector is
     (Platprop.Compiler_Is (Compiler) and
      Platprop.Distribution_Is (Distribution) and
      Platprop.System_Is (Operating_System) and
      Platprop.Version_Is (Distro_Version) and
      Platprop.Word_Size_Is (Word_Size));

   --------------
   -- Compiler --
   --------------

   function Compiler return Alire.Platforms.Compilers is
      package Semver renames Semantic_Versioning;
      use all type Semver.Point;
      use Utils;

      Year   : Natural;
      Output : String_Vector;
   begin
      OS_Lib.Spawn_And_Capture (Output, "gnat");

      for Line of Output loop
         if Line'Length > 4 and then Line (Line'First .. Line'First + 3) = "GNAT" then
            declare
               Version : constant String := To_Lower_Case (Tail (Line, ' '));
            begin
               if Contains (Version, "gpl") then
                  begin
                     Year := Natural'Value (Head (Tail (Version, ' '), ' '));
                     if Year < 2017 then
                        return GNAT_GPL_Old;
                     else
                        return GNAT_GPL_2017_Or_Newer;
                     end if;
                  exception
                     when others => -- Somehow it doesn't follow the GPL XXXX (X) convention
                        return GNAT_GPL_Old;
                  end;
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
                     when others => -- Not a plain semantic version like FSF uses
                        return GNAT_Unknown;
                  end;
               end if;
            end;
         end if;
      end loop;

      Trace.Debug ("Unexpected output from gnat");
      return GNAT_Unknown;
   end Compiler;

   ---------
   -- Get --
   ---------

   function Get return Platforms.Supported'Class is (Instance.all);

   ----------------
   -- Properties --
   ----------------

   function Properties return Alire.Properties.Vector is (Props.all);

   ---------
   -- Set --
   ---------

   procedure Set (P : Platforms.Supported'Class) is
   begin
      Instance := new Platforms.Supported'Class'(P);
   end Set;

   -------------------
   -- Set_Propertes --
   -------------------

   procedure Set_Propertes (P : Alire.Properties.Vector) is
   begin
      Props := new Alire.Properties.Vector'(P);
   end Set_Propertes;

end Alr.Platform;
