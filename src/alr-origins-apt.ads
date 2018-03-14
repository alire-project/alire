package Alr.Origins.Apt is

   type Origin is new Alr.Origins.Origin with null record;

   function Already_Installed (This : Origin) return Boolean;

   overriding function Exists (This : Origin) return Boolean;

   overriding procedure Fetch (This : Origin; Folder : String);

   overriding procedure Install (This : Origin);

end Alr.Origins.Apt;
