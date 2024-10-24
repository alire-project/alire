package Alire.VCSs is

   type VCS is interface;

   function Clone (This   : VCS;
                   From   : URL;
                   Into   : Directory_Path;
                   Commit : String := "")
                   return Outcome is abstract;

   function Update (This : VCS;
                    Repo : Directory_Path)
                    return Outcome is abstract;

   -----------------------
   -- General utilities --
   -----------------------

   function Repo_URL (Origin : URL) return String;
   --  Return a URL suitable for passing directly to a VCS
   --
   --  Strips any "vcs+" prefix, converts "file:" URLs to paths and removes any
   --  fragment part (separated by #).
   --
   --  Returns non-URLs unmodified.

   function Commit (Origin : URL) return String;
   --  The fragment of the URL (separated by #)
   --
   --  Returns empty string for non-URLs or if there is no fragment.

end Alire.VCSs;
