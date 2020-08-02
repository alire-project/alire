with Alire.Outcomes.Indefinite;

package Alire.Roots.Optional is

   package Root_Outcomes is new Outcomes.Indefinite (Root);

   type Root is new Root_Outcomes.Outcome with null record;

   function Detect_Root (Path : Any_Path) return Optional.Root;

   function Exists (This : Root) return Boolean renames Success;

end Alire.Roots.Optional;
