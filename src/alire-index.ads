with Alire.Repositories;

with Semver;

package Alire.Index with Preelaborate is  
   
   type Release (<>) is private;    
   
   type Release_Range (<>) is private;
   
   type Dependencies (<>) is private;
   
   function New_Release (Name       : String;
                         Version    : Semver.Version;
                         Source     : Repositories.Repository;
                         Depends_On : Dependencies) return Release;
   
private
   
   type Release is null record;
   
   type Release_Range is null record;
   
   type Dependencies is null record;

end Alire.Index;
