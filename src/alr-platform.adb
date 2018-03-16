with Alire.Properties.Platform;

package body Alr.Platform is

   -------------
   -- Singletons

   Instance : access Platforms.Supported'Class;

   Props    : access Alire.Properties.Vector;

   ----------------------
   -- Basic_Properties --
   ----------------------

   package Platprop renames Alire.Properties.Platform;
   use all type Alire.Properties.Vector;

   function Basic_Properties return Alire.Properties.Vector is
     (Platprop.Compiler_Is (Compiler) and
      Platprop.Distribution_Is (Distribution) and
      Platprop.System_Is (Operating_System) and
      Platprop.Version_Is (Distro_Version) and
      Platprop.Word_Size_Is (Word_Size));

   --------------
   -- Compiler --
   --------------

   function Compiler return Alire.Platforms.Compilers is
   begin
      is
        (if Contains (Comp.Version, "2017")
         then GNAT_GPL_2017
         else (if Contains (Comp.Version, "7.2")
               then GNAT_FSF_7_2
               else GNAT_Unknown));

      if Contains (To_Lower_Case (Comp.Version), "gpl") then


      return GNAT_Unknown;
   end Alr.Platform;

   ---------
   -- Get --
   ---------

   function Get return Platforms.Supported'Class is (Instance.all);

   ----------------
   -- Properties --
   ----------------

   function Properties return Alire.Properties.Vector is (Props.all);

   ---------
   -- Set --
   ---------

   procedure Set (P : Platforms.Supported'Class) is
   begin
      Instance := new Platforms.Supported'Class'(P);
   end Set;

   -------------------
   -- Set_Propertes --
   -------------------

   procedure Set_Propertes (P : Alire.Properties.Vector) is
   begin
      Props := new Alire.Properties.Vector'(P);
   end Set_Propertes;

end Alr.Platform;
