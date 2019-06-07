package Alire.VCSs is

   type VCS is interface;

   type Kinds is (VCS_Git, VCS_Unknown);

   subtype Known_Kinds is Kinds range Kinds'First .. Kinds'Pred (VCS_Unknown);

   --  URL format:
   --  vcs+http[s]://path/to/repo[@commit]

   function Clone (This : VCS;
                   From : URL;
                   Into : Platform_Independent_Path)
                   return Outcome is abstract;

   function Update (This : VCS;
                    Repo : Platform_Independent_Path)
                    return Outcome is abstract;

   -----------------------
   -- General utilities --
   -----------------------

   function Kind (Origin : URL) return Kinds;

   function Repo (Origin : URL) return String;
   --  Without kind and commit

   function Repo_And_Commit (Origin : URL) return String;
   --  Without Kind and with optional Commit

   function Commit (Origin : URL) return String;
   --  Empty string if no commit part

   ------------------------
   -- Classwide versions --
   ------------------------
   --  Those redispatch to the appropriate descendant

   function Clone (From : URL;
                   Into : Platform_Independent_Path)
                   return Outcome
     with Pre => Kind (From) in Known_Kinds;

end Alire.VCSs;
