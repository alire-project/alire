
package Alire.Environment.Formatting is

   function Format (Release_Dir : Any_Path;
                    Value       : String)
                    return String;
   --  Format the environment variable value with ${} replacement patterns

   Unknown_Formatting_Key : exception;

end Alire.Environment.Formatting;
