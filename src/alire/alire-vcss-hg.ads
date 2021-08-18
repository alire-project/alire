package Alire.VCSs.Hg is

   subtype Hg_Commit is String (1 .. 40);

   type VCS (<>) is new VCSs.VCS with private;

   function Handler return VCS;

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path)
                   return Outcome;

   overriding
   function Update (This : VCS;
                    Repo : Directory_Path)
                    return Outcome;

private

   type VCS is new VCSs.VCS with null record;

   function Handler return VCS is (null record);

end Alire.VCSs.Hg;
