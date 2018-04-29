with Alire.Dependencies;

with Alr.Utils;

package Alr.Code is

   --  Code generators from data structures

   function Generate (Deps : Alire.Dependencies.Vector) 
                      return Utils.String_Vector;
   
end Alr.Code;
