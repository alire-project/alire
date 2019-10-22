with Alire.Utils;

with GNATCOLL.VFS;

package body Alire.Origins.Tweaks is

   ------------------
   -- Fixed_Origin --
   ------------------

   function Fixed_Origin (TOML_Path : String;
                          This      : Origin) return Origin
   is
      use GNATCOLL.VFS;

      --------------------
      -- Fix_Filesystem --
      --------------------

      function Fix_Filesystem return Origin is
      begin
         if Create (+This.Path).Is_Absolute_Path then
            return This;
         end if;

         declare
            Base   : constant Virtual_File := Create (+TOML_Path);
            Target : constant Virtual_File := Create (+This.Path);
            Fixed  : Origins.Origin := This; -- Copy contents
         begin
            Fixed.Data.Path :=
              +(+Full_Name (Base.Dir / Target, Normalize => True));

            return Fixed;
         end;
      end Fix_Filesystem;

      -------------
      -- Fix_VCS --
      -------------

      function Fix_VCS return Origin is
         use Utils;
         URL : constant String := This.URL; -- Doesn't include @commit
      begin
         --  Check for "xxx+file://" or return as-is:
         if not Starts_With (URL, Prefix_File) then
            return This;
         end if;

         declare
            Base     : constant Virtual_File := Create (+TOML_Path);
            Rel_Path : constant Platform_Independent_Path :=
                         Tail (Tail (URL, '/'), '/');
            Target   : constant Virtual_File := Create (+Rel_Path);
            Absolute : Origin := This;
         begin
            --  Check that path is indeed relative...
            if Target.Is_Absolute_Path then
               return This;
            end if;

            --  Rebuild the filesystem path as absolute for the VCS in hand:
            Absolute.Data.Repo_URL := + -- Unbounded string
              (Prefix_File &
                (+Full_Name (Base.Dir / Target, Normalize => True)));

            return Absolute;
         end;
      end Fix_VCS;

   begin
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
