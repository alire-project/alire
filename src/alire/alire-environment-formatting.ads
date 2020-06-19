with Alire.Releases;

package Alire.Environment.Formatting is

   function Format (Rel             : Releases.Release;
                    Value           : String;
                    Is_Root_Release : Boolean)
                    return String;
   --  Format the environment variable falue with ${} replacement patterns

   Unknown_Formatting_Key : exception;

end Alire.Environment.Formatting;
