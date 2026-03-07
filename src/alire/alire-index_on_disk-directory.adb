package body Alire.Index_On_Disk.Directory is

   ---------------------
   -- Index_Directory --
   ---------------------

   overriding
   function Index_Directory (This : Index) return String
   is (URI.Local_Path (This.Origin));

   -----------------
   -- New_Handler --
   -----------------

   overriding
   function New_Handler (From   : URL;
                         Name   : Restricted_Name;
                         Parent : Any_Path)
                         return Index
   is
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
