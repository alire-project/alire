with GNAT.OS_Lib;

package body Alire.OS_Lib is

   ----------------
   -- Exe_Suffix --
   ----------------

   function Exe_Suffix return String is
      --  Shenanigans needed to stay preelaborable
      use GNAT.OS_Lib;

      Suffix : String_Access := Get_Executable_Suffix;
   begin
      return S : constant String := Suffix.all do
         Free (Suffix);
      end return;
   end Exe_Suffix;

end Alire.OS_Lib;
