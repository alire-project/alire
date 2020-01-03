package Alire.VCSs.Git is

   type VCS (<>) is new VCSs.VCS with private;

   function Handler return VCS;

   not overriding
   function Branch (This : VCS;
                    Path : Directory_Path)
                    return String;
   --  Returns the branch name of the repo checked out at Path.

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path)
                   return Outcome;

   not overriding
   function Is_Detached (This : VCS;
                         Path : Directory_Path) return Boolean;
   --  Says if the repo checked out at Path is in a detached HEAD state.

   overriding
   function Update (This : VCS;
                    Repo : Directory_Path)
                    return Outcome;

private

   type VCS is new VCSs.VCS with null record;

   function Handler return VCS is (null record);

end Alire.VCSs.Git;
