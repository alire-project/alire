with Alire.Origins;

package Alire.Platform with Preelaborate is

   --  This interface encapsulates what a supported platform must provide for use in Alire, and a way
   --    to hook it after elaboration

   type Supported_Platform is interface;

   function Package_Version (P      : Supported_Platform;
                             Origin : Origins.Origin)
                             return String is abstract;

   procedure Set (P : Supported_Platform'Class);

   function Current return Supported_Platform'Class
     with Pre => Platform'Elaborated;
   --  Always valid, because at worst a dummy do-nothign one is returned

end Alire.Platform;
