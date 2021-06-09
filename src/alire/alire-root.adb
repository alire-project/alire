with Alire.Directories;

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

   function Platform_Properties return Properties.Vector
   is (Environment);

   -----------------------------
   -- Set_Platform_Properties --
   -----------------------------

   procedure Set_Platform_Properties (Env : Properties.Vector) is
   begin
      Environment := Env;
   end Set_Platform_Properties;

end Alire.Root;
