with Alire.Directories;
with Alire.Properties.Platform;

package body Alire.Root is

   -------------
   -- Current --
   -------------

   function Current return Roots.Optional.Root
   is (Roots.Optional.Detect_Root (Directories.Detect_Root_Path));

   -------------
   -- Current --
   -------------

   function Current return Roots.Root
   is (Roots.Optional.Detect_Root (Directories.Detect_Root_Path).Value);

   -------------------------
   -- Platform_Properties --
   -------------------------

   Environment : Properties.Vector;
   OS          : Platforms.Operating_Systems := Platforms.OS_Unknown;

   -------------------------
   -- Platform_Properties --
   -------------------------

   function Platform_Properties return Properties.Vector
   is (Environment);

   -----------------
   -- Platform_OS --
   -----------------

   function Platform_OS return Platforms.Operating_Systems
   is (OS);

   -----------------------------
   -- Set_Platform_Properties --
   -----------------------------

   procedure Set_Platform_Properties (Env : Properties.Vector) is
   begin
      Environment := Env;

      --  Extract the current OS for easier use

      for Prop of Env loop
         if Prop in Properties.Platform.Operating_Systems.Property'Class then
            OS := Properties.Platform.Operating_Systems.Property'Class (Prop)
              .Element;
         end if;
      end loop;
   end Set_Platform_Properties;

end Alire.Root;
