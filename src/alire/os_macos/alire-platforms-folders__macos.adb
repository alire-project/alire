with Alire.Platforms.Common;

package body Alire.Platforms.Folders is

   --  macOS implementation

   -----------
   -- Cache --
   -----------

   function Cache return String is (Common.XDG_Config_Folder);

   -----------
   -- Config--
   -----------

   function Config return String is (Common.XDG_Config_Folder);

end Alire.Platforms.Folders;
