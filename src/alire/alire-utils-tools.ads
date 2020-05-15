package Alire.Utils.Tools is

   type Tool_Kind is (Git, Tar, Unzip, Curl, Mercurial, Subversion);

   procedure Check_Tool (Tool : Tool_Kind);
   --  Check if a required executable tool is available in PATH.
   --  If not, try to install it or abort.

end Alire.Utils.Tools;
