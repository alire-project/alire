with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Index_On_Disk;

package Alire.Features.Index is

   package Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Index_On_Disk.Index'Class, Index_On_Disk."<", Index_On_Disk."=");

   subtype Index_On_Disk_Set is Sets.Set;

   function Find_All
     (Under  : Absolute_Path;
      Result : out Outcome) return Index_On_Disk_Set;
   --  Find all indexes available on a disk location. If valid indexes are
   --  found or none, set Result to Outcome_Success and return the
   --  corresponding set. If at least one found index is invalid, set Result to
   --  an outcome failure and return en empty set.
   --
   --  We abort at the first invalid index as it's likely in this case that
   --  users misconfigured something, so this helps them notice the issue
   --  instead of proceeding with default behaviors, such as getting the
   --  community index.

   function Load_All (From : Absolute_Path) return Outcome;
   --  Load all indexes available at the given location

   function Update_All (Under : Absolute_Path) return Outcome;
   --  Find and update all indexes at given location

   function Add (Origin : URL;
                 Name   : String;
                 Under  : Absolute_Path;
                 Before : String := "") return Outcome;
   --  Add a new remote index under a folder that possibly contains more
   --    indexes in child folders. That is, Under is the parent of all indexes
   --  Index will be set as last one, or before given index name

end Alire.Features.Index;
