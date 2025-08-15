with Alire.Utils;

package Alire.VCSs.Hg is

   subtype Hg_Commit is String (1 .. 40) with
     Dynamic_Predicate =>
       (for all Char of Hg_Commit => Char in Utils.Hexadecimal_Character);

   function Is_Valid_Commit (S : String) return Boolean
   is (S'Length = Hg_Commit'Length and then
         (for all Char of S => Char in Utils.Hexadecimal_Character));

   type VCS (<>) is new VCSs.VCS with private;

   function Handler return VCS;

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path;
                   Commit : String := "")
                   return Outcome;

   overriding
   function Update (This : VCS;
                    Repo : Directory_Path)
                    return Outcome;

private

   type VCS is new VCSs.VCS with null record;

   function Handler return VCS is (null record);

end Alire.VCSs.Hg;
