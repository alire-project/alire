with Alire.OS_Lib.Subprocess;
with Alire.Properties.Platform;
with Alire.Utils;

with Alr.Utils;

with Semantic_Versioning;

package body Alr.Platform is

   --------------
   --  Singletons

   type Supported_Access is access Platforms.Supported'Class;
   Instance : Supported_Access;

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
         Platprop.Word_Size_Is (Word_Size));

   --------------
   -- Compiler --
   --------------

   Compiler_Cached       : Alire.Platforms.Compilers;
   Compiler_Cached_Valid : Boolean := False;

   function Compiler_Uncached return Alire.Platforms.Compilers is
      package Semver renames Semantic_Versioning;
      use Utils;

      Year   : Natural;
      Output : String_Vector;
   begin
      Output := Alire.OS_Lib.Subprocess.Checked_Spawn_And_Capture
        ("gnat", Alire.Utils.Empty_Vector, Err_To_Out => True);

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
                  begin
                     Year := Natural'Value (Head (Tail (Version, ' '), ' '));
                     case Year is
                        when 2018   => return GNAT_Community_2018;
                        when 2019   => return GNAT_Community_2019;
                        when others => return GNAT_Unknown;
                     end case;
                  exception
                     when others =>
                        --  Somehow it doesn't follow the Community XXXX (X)
                        --  convention.
                        return GNAT_Unknown;
                  end;
               else
                  declare
                     V    : Semver.Version;
                     Last : Natural := Version'First;
                  begin
                     --  At least on Ubuntu, Version looks like:
                     --     9.2.1 20191008
                     --
                     --  We want semver to parse only the first part. Set Last
                     --  to the index of the last character before the first
                     --  space in Version. If there is no space in Line, set
                     --  it to Version'Last.

                     for I in Version'Range loop
                        exit when Version (I) = ' ';
                        Last := I;
                     end loop;

                     V := Semver.Parse
                       (Version (Version'First .. Last), Relaxed => False);

                     case Semver.Major (V) is
                        when 0 .. 6 =>
                           return GNAT_FSF_Old;
                        when 7 =>
                           case Semver.Minor (V) is
                              when 0 | 1  => return GNAT_FSF_Old;
                              when 2      => return GNAT_FSF_7_2;
                              when 3      => return GNAT_FSF_7_3;
                              when 4      => return GNAT_FSF_7_4;
                              when 5      => return GNAT_FSF_7_5;
                              when others => return GNAT_Unknown;
                           end case;
                        when 8 =>
                           case Semver.Minor (V) is
                              when 0      => return GNAT_FSF_8_0;
                              when 1      => return GNAT_FSF_8_1;
                              when 2      => return GNAT_FSF_8_2;
                              when 3      => return GNAT_FSF_8_3;
                              when others => return GNAT_Unknown;
                           end case;
                        when 9 =>
                           case Semver.Minor (V) is
                              when 0      => return GNAT_FSF_9_0;
                              when 1      => return GNAT_FSF_9_1;
                              when others => return GNAT_FSF_9_2_Or_Newer;
                           end case;
                        when others =>
                           return GNAT_FSF_9_2_Or_Newer;
                     end case;
                  exception
                     when others =>
                        --  Not a plain semantic version like FSF uses
                        return GNAT_Unknown;
                  end;
               end if;
            end;
         end if;
      end loop;

      return GNAT_Unknown;
   exception
      when E : Alire.Checked_Error =>
         Alire.Log_Exception (E);
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
