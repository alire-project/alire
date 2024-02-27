package Alire.Utils.Tools is

   type Tool_Kind is
     (Curl,       -- For downloads
      Easy_Graph, -- For `--graph`
      Git,        -- Release origin
      Mercurial,  -- Release origin
      Subversion, -- Release origin
      Tar,        -- For source archive origins
      Unzip       -- For source archive origins
     );

   function Available (Tool : Tool_Kind) return Boolean;
   --  Say if tool is already available (attempts detection for the tool, but
   --  does not install it if missing).

   procedure Check_Tool (Tool : Tool_Kind; Fail : Boolean := True);
   --  Check if a required executable tool is available in PATH.
   --  If not, try to install it. If unable and Fail, abort, otherwise return

   function Is_BSD_Tar return Boolean
     with Pre => Available (Tar);
   --  Say if the tar in PATH is the bsdtar variant, which lacks some features

end Alire.Utils.Tools;
