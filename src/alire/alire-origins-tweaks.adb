with Ada.Directories;

with Alire.OS_Lib;
with Alire.Utils;

package body Alire.Origins.Tweaks is

   ------------------
   -- Fixed_Origin --
   ------------------

   function Fixed_Origin (TOML_Path : String;
                          This      : Origin) return Origin
   is
      use OS_Lib.Operators;

      --------------------
      -- Fix_Filesystem --
      --------------------

      function Fix_Filesystem return Origin is
         use Ada.Directories;
      begin
         if Check_Absolute_Path (This.Path) then
            return This;
         end if;

         return Fixed : Origins.Origin := This do -- Copy contents
            Fixed.Data.Path := +Full_Name (TOML_Path / This.Path);
         end return;
      end Fix_Filesystem;

      -------------
      -- Fix_VCS --
      -------------

      function Fix_VCS return Origin is
         use Ada.Directories;
         use Utils;
         URL : constant String := This.URL; -- Doesn't include @commit
      begin
         --  Check for "xxx+file://" or return as-is:
         if not Starts_With (URL, Prefix_File) then
            return This;
         end if;

         declare
            Rel_Path : constant Relative_Path :=
                         Tail (Tail (URL, '/'), '/'); -- TODO superflaky
            Absolute : Origin := This;
         begin
            --  Check that path is indeed relative...
            if Check_Absolute_Path (Rel_Path) then
               return This;
            end if;

            --  Rebuild the filesystem path as absolute for the VCS in hand:
            Absolute.Data.Repo_URL := + -- Unbounded string
              (Prefix_File & Full_Name (TOML_Path / Rel_Path));

            return Absolute;
         end;
      end Fix_VCS;

   begin
      --  If we receive the manifest file path instead of its parent folder:
      if Ada.Directories.Kind (TOML_Path) in Ada.Directories.Ordinary_File then
         return Fixed_Origin (Ada.Directories.Containing_Directory (TOML_Path),
                              This);
      end if;

      --  We must fix filesystem, or VCSs with a filesystem upstream (that are
      --  used in the testsuite).
      case This.Kind is
         when Filesystem =>
            return Fix_Filesystem;
         when VCS_Kinds =>
            return Fix_VCS;
         when others =>
            return This;
      end case;
   end Fixed_Origin;

end Alire.Origins.Tweaks;
