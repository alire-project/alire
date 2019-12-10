package Alire.VCSs.Git is

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

end Alire.VCSs.Git;
