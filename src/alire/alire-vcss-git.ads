package Alire.VCSs.Git is

   type VCS (<>) is new VCSs.VCS with private;

   function Handler return VCS;

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Platform_Independent_Path)
                   return Outcome;

   overriding
   function Update (This : VCS;
                    Repo : Platform_Independent_Path)
                    return Outcome;

private

   type VCS is new VCSs.VCS with null record;

   function Handler return VCS is (null record);

end Alire.VCSs.Git;
