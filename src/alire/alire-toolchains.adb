with Alire.Utils.User_Input;

package body Alire.Toolchains is

   ---------------
   -- Assistant --
   ---------------

   procedure Assistant is
      Choices : Utils.String_Vector;
   begin
      for I in 1 .. 21 loop
         Choices.Append ("Option" & I'Image);
      end loop;

      Put_Info ("Selected:"
                & Utils.User_Input.Query_Multi
                  ("BLAH BLAH",
                   Choices)'Image);
   end Assistant;

end Alire.Toolchains;
