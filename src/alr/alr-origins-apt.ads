package Alr.Origins.Apt is

   type Origin is new Alr.Origins.Origin with null record;

   overriding function Already_Installed (This : Origin) return Boolean;

   overriding function Exists (This : Origin) return Boolean;

   overriding procedure Fetch (This : Origin; Folder : String);

   overriding procedure Install (This : Origin);

   overriding function Native_Version (This : Origin) return String;

end Alr.Origins.Apt;
