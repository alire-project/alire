with Alire.Origins;

package Alr.Origins is

   --  Actual fetching logic

   function Already_Available_Native (Origin : Alire.Origins.Origin) return Boolean;
   --  Says if already installed

   function Native_Package_Exists (Name : String) return Boolean;
   --  Says if exists in the system

   procedure Fetch (From : Alire.Origins.Origin; Folder : String);

   procedure Fetch_Native (From : Alire.Origins.Origin);

end Alr.Origins;
