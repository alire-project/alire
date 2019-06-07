package body Alire.Index_On_Disk.Directory is

   ---------------------
   -- Index_Directory --
   ---------------------

   overriding
   function Index_Directory (This : Index) return String is
     (This.Origin
        (This.Origin'First + File_Prefix'Length .. This.Origin'Last));

   -----------------
   -- New_Handler --
   -----------------

   overriding
   function New_Handler (From   : URL;
                         Name   : Restricted_Name;
                         Parent : Platform_Independent_Path) return Index is
   begin
      return Idx : constant Index := Index'(URL_Len  => From'Length,
                                            Name_Len => Name'Length,
                                            Dir_Len  => Parent'Length,
                                            Origin   => From,
                                            Name     => Name,
                                            Parent   => Parent,
                                            Priority => <>);
   end New_Handler;

end Alire.Index_On_Disk.Directory;
