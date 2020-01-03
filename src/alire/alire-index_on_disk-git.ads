with Alire.Utils;

package Alire.Index_On_Disk.Git is

   --  Local management of remote indexes stored in git repositories

   type Index is new Index_On_Disk.Index with private;

   overriding
   function New_Handler (Origin : URL;
                         Name   : Restricted_Name;
                         Parent : Any_Path) return Index with
     Pre => Utils.Starts_With (Origin, "git+");

   overriding
   function Add (This : Index) return Outcome;
   --  Clones the index repository

   overriding
   function Update (This : Index) return Outcome;
   --  Pulls the repository, unless it had a specific commit on checkout.
   --  In that case, silently do nothing and return success.

private

   type Index is new Index_On_Disk.Index with null record;

end Alire.Index_On_Disk.Git;
