with Alire.Platforms;
with Alire.Properties;

with Alr.Platforms;

with System;

package Alr.Platform is

   --  This package is the gateway to a platform specific information
   --  It's set up first after elaboration and is hereafter usable

   function Properties return Alire.Properties.Vector
     with Pre => Platform'Elaborated;

   ----------------------------------
   --  Portably detectable properties

   function Target return Alire.Platforms.Targets is (Alire.Platforms.Native);
   --  Paths for now

   function Word_Size return Alire.Platforms.Word_Sizes;

   ---------
   --  Proxy some functions provided by the current platform

   function Cache_Folder return String;

   function Config_Folder return String;

   function Distribution return Alire.Platforms.Distributions;

   function Operating_System return Alire.Platforms.Operating_Systems;

private

   procedure Set (P : Platforms.Supported'Class);

   ---------
   -- Get --
   ---------

   function Get return Platforms.Supported'Class
     with Pre => Platform'Elaborated;

   ------------------
   -- Cache_Folder --
   ------------------

   function Cache_Folder return String is (Get.Cache_Folder);

   -------------------
   -- Config_Folder --
   -------------------

   function Config_Folder return String is (Get.Config_Folder);

   ------------------
   -- Distribution --
   ------------------

   function Distribution return Alire.Platforms.Distributions
   is (Get.Distribution);

   ----------------------
   -- Operating_System --
   ----------------------

   function Operating_System return Alire.Platforms.Operating_Systems
   is (Get.Operating_System);

   ---------------
   -- Word_Size --
   ---------------

   function Word_Size return Alire.Platforms.Word_Sizes is
     (case System.Word_Size is
         when 32 => Alire.Platforms.Bits_32,
         when 64 => Alire.Platforms.Bits_64,
         when others => Alire.Platforms.Bits_Unknown);

end Alr.Platform;
