with Alire.OS_Lib;

package body Alire.Platform is

   --  Linux implementation

   function Default_Config_Folder return String is
      use OS_Lib;
   begin
     return (OS_Lib.Getenv ("XDG_CONFIG_HOME",
             Default => OS_Lib.Getenv ("HOME") / ".config" / "alire"));
   end Default_Config_Folder;

   function Name return Supported is (Linux);

end Alire.Platform;
