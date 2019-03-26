with Ada.Text_IO;

package Alr.Testing.Text is

   type Reporter is new Testing.Reporter with private;
   
   overriding 
   function New_Reporter return Reporter; 
   
   overriding 
   procedure Start_Run (This  : in out Reporter; 
                        Name  :        String;
                        Tests :        Natural);   
   
   overriding 
   procedure End_Run   (This : in out Reporter);
   --  These refer to a whole run of tests
   
   overriding 
   procedure End_Test (This    : in out Reporter;
                       Rel     :        Alire.Types.Release;
                       Outcome :        Outcomes;
                       Elapsed :        Duration;
                       Log     :        Utils.String_Vector);
   
private
      
   type Reporter is new Testing.Reporter with record
      File : access Ada.Text_IO.File_Type;
   end record;
   
   overriding 
   function New_Reporter return Reporter is (others => <>);

end Alr.Testing.Text;
