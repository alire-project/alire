with GNAT.Strings;

package Alr.Commands.Testing is

   function To_Boolean (Image   : GNAT.Strings.String_Access;
                        Switch  : String;
                        Default : Boolean)
                        return Boolean;
   --  Public wrapper for the private Alr.Commands.To_Boolean function

end Alr.Commands.Testing;