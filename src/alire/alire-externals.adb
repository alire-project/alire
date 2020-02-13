with Alire.Crates;
with Alire.TOML_Load;

with TOML;

package body Alire.Externals is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return External'Class is
      pragma Warnings (Off);
      Ext        : External'Class := From_TOML (From); -- Recursive until impl
      pragma Warnings (On);

      Deps : Conditional.Dependencies;

      Result : constant Outcome :=
                     TOML_Load.Load_Crate_Section
                       (Section => Crates.External_Section,
                        From    => From,
                        Props   => Ext.Properties,
                        Deps    => Deps,
                        Avail   => Ext.Available);
   begin
      Assert (Result);

      --  Ensure that no dependencies have been defined for the external. This
      --  may be handy in the future, but until the need arises better not have
      --  it complicating things.

      if not Deps.Is_Empty then
         From.Checked_Error ("externals cannot have dependencies");
      end if;

      raise Unimplemented; -- No concrete externals defined yet
      return Ext;
   end From_TOML;

end Alire.Externals;
