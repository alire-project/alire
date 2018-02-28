with Alire.Origins;

package Alr.Origins is

   --  Actual fetching logic

   function Already_Available_Native (Origin : Alire.Origins.Origin) return Boolean;

   procedure Fetch (From : Alire.Origins.Origin; Folder : String);

   procedure Fetch_Native (From : Alire.Origins.Origin);

end Alr.Origins;
