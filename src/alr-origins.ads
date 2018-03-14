with Alire.Origins;

package Alr.Origins is

   --  Actual fetching logic

   type Origin is abstract tagged private;

   function New_Origin (From : Alire.Origins.Origin) return Origin'Class; -- factory

   function Already_Installed (This : Origin) return Boolean is abstract
     with Pre'Class => This.Is_Native;
   --  Say if a native package is already installed in this system
   --  Unneeded otherwise

   function Base (This : Origin) return Alire.Origins.Origin;

   function Exists (This : Origin) return Boolean is abstract
     with Pre'Class => This.Is_Native;
   --  Says if a native package exists in this system
   --  Uneeded otherwise

   procedure Fetch (This : Origin; Folder : String) is abstract
     with Pre'Class => not This.Is_Native;

   procedure Install (This : Origin) is abstract
     with Pre'Class => This.Is_Native;

   function Is_Native (This : Origin) return Boolean;



   function Already_Available_Native (Origin : Alire.Origins.Origin) return Boolean;
   --  Says if already installed

   function Native_Package_Exists (Name : String) return Boolean;
   --  Says if exists in the system

   procedure Fetch (From : Alire.Origins.Origin; Folder : String);

   procedure Fetch_Native (From : Alire.Origins.Origin);

private

   type Origin is abstract tagged record
      Base : Alire.Origins.Origin;
   end record;

   function Base (This : Origin) return Alire.Origins.Origin is (This.Base);

   function Is_Native (This : Origin) return Boolean is (This.Base.Is_Native);

end Alr.Origins;
