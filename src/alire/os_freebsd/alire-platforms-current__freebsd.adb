
package body Alire.Platforms.Current is

   --  FreeBSD implementation (very close to Linux)

   ---------------------------
   -- Detected_Distribution --
   ---------------------------

   function Detected_Distribution return Platforms.Distributions is
      (Platforms.Distro_Unknown);

   -----------------------
   -- Distribution_Root --
   -----------------------

   function Distribution_Root return Absolute_Path
   is ("/");

   ----------------------
   -- Load_Environment --
   ----------------------

   procedure Load_Environment (Ctx : in out Alire.Environment.Context)
   is null;

   ----------------------
   -- Operating_System --
   ----------------------

   function Operating_System return Alire.Platforms.Operating_Systems
   is (Alire.Platforms.FreeBSD);

end Alire.Platforms.Current;
