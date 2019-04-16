with Ada.Containers.Indefinite_Ordered_Maps;

package Alire.Projects with Preelaborate is

   package Project_Description_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Alire.Project, Description_String);

   --  TODO: combine Index, Descriptions in a single data structure
   Descriptions : Project_Description_Maps.Map;
   -- Master list of known projects & descriptions

   type Named is limited interface;

   function Project (N : Named) return Alire.Project is abstract;

end Alire.Projects;
