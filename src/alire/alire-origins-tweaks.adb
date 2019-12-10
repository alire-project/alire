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

   begin
      --  We must fix filesystem, or VCSs with a filesystem upstream (that are
      --  used in the testsuite).
      case This.Kind is
         when Filesystem =>
            return Fix_Filesystem;
         when others =>
            return This;
      end case;
   end Fixed_Origin;

end Alire.Origins.Tweaks;
