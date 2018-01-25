package Alire.Repositories with Preelaborate is

   type Kinds is (Git);
   
   type Repository (Kind : Kinds := Git) is record 
      case Kind is
         when Git => 
            Git_Hash : String (1 .. 40);
      end case;     
   end record;
   

end Alire.Repositories;
