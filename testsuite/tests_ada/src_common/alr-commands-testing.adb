package body Alr.Commands.Testing is

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Image   : GNAT.Strings.String_Access;
                        Switch  : String;
                        Default : Boolean)
                        return Boolean renames Alr.Commands.To_Boolean;

end Alr.Commands.Testing;