with Alire.Properties.Platform;
with Alire.Utils;

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
   is (Platprop.Distribution_Is (Distribution) and
         Platprop.System_Is (Operating_System) and
         Platprop.Target_Is (Target) and
         Platprop.Word_Size_Is (Word_Size));

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
