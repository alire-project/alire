with Alire.Origins;

package Alr.Origins is

   --  Actual fetching logic

   -----------
   -- Fetch --
   -----------

   --  Entry points from elsewhere

   procedure Fetch (From : Alire.Origins.Origin; Folder : String)
     with Pre => not From.Is_Native;

   procedure Install_Native (From : Alire.Origins.Origin)
     with Pre => From.Is_Native;

   procedure Fetch_Or_Install (From : Alire.Origins.Origin; Folder : String);
   --  Dispatch to proper one


   ------------
   -- Origin --
   ------------

   --  Type that encapsulates the particulars of every origin

   type Origin is tagged private;
   --  This should be abstract but I'm hitting many funny things

   function New_Origin (From : Alire.Origins.Origin) return Origin'Class; -- factory

   function Already_Installed (This : Origin) return Boolean is (False)
     with Pre'Class => This.Is_Native;
   --  Say if a native package is already installed in this system
   --  Unneeded otherwise

   function Base (This : Origin) return Alire.Origins.Origin;

   function Exists (This : Origin) return Boolean is (False)
     with Pre'Class => This.Is_Native;
   --  Says if a native package exists in this system
   --  Uneeded otherwise

   procedure Fetch (This : Origin; Folder : String) is null
     with Pre'Class => not This.Is_Native;

   procedure Install (This : Origin) is null
     with Pre'Class => This.Is_Native;

   function Is_Native (This : Origin) return Boolean;

private

   type Origin is tagged record
      Base : Alire.Origins.Origin;
   end record;

   function Base (This : Origin) return Alire.Origins.Origin is (This.Base);

   function Is_Native (This : Origin) return Boolean is (This.Base.Is_Native);

end Alr.Origins;
