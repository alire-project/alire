with Ada.Exceptions;

package Alr.Exceptions with Preelaborate is

   procedure Report (Preamble : String; E : Ada.Exceptions.Exception_Occurrence);

end Alr.Exceptions;
