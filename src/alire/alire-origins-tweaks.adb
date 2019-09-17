with GNATCOLL.VFS;

package body Alire.Origins.Tweaks is

   ------------------
   -- Fixed_Origin --
   ------------------

   function Fixed_Origin (TOML_Path : String;
                          This      : Origin) return Origin
   is
      use GNATCOLL.VFS;
   begin
      if This.Kind /= Filesystem then
         return This;
      end if;

      if Create (+This.Path).Is_Absolute_Path then
         return This;
      end if;

      declare
         Base   : constant Virtual_File := Create (+TOML_Path);
         Target : constant Virtual_File := Create (+This.Path);
      begin
         return New_Filesystem (+Full_Name (Base.Dir / Target,
                                            Normalize => True));
      end;
   end Fixed_Origin;

end Alire.Origins.Tweaks;
