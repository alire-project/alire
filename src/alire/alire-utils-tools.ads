package Alire.Utils.Tools is

   type Tool_Kind is
     (Easy_Graph, Git, Tar, Unzip, Curl, Mercurial, Subversion);

   function Available (Tool : Tool_Kind) return Boolean;
   --  Say if tool is already available (attemps detection for the tool, but
   --  does not install it if missing).

   procedure Check_Tool (Tool : Tool_Kind; Fail : Boolean := True);
   --  Check if a required executable tool is available in PATH.
   --  If not, try to install it. If unable and Fail, abort, otherwise return

end Alire.Utils.Tools;
