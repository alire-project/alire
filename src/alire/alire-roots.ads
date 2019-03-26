with Alire.Releases;

package Alire.Roots is

   --  Type used to encapsulate the information about the root release/working copy

   type Root is new Releases.Release with null record;

   function New_Root (Name : Alire.Project) return Root is
     (Releases.New_Working_Release (Name) with null record);

   function New_Root (R : Releases.Release) return Root is
     (R with null record);

   function Release (This : Root) return Releases.Release is
     (Releases.Release (This));

end Alire.Roots;
