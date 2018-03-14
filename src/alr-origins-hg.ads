package Alr.Origins.Hg is

   type Origin is new Alr.Origins.Origin with null record;

   function Already_Installed (This : Origin) return Boolean is (raise Program_Error);

   overriding function Exists (This : Origin) return Boolean is (raise Program_Error);

   overriding procedure Fetch (This : Origin; Folder : String);

   overriding procedure Install (This : Origin);

end Alr.Origins.Hg;
