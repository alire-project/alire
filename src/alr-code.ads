with Alr.Utils;

package Alr.Code is

   --  Code generators from data structures

   function Generate (Deps : Types.Platform_Dependencies) 
                      return Utils.String_Vector;
   
end Alr.Code;
