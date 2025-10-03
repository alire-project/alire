with Ada.Directories;
with Ada.Numerics.Discrete_Random;

package body Alr.Utils.Temp_File is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out File) is
      use Ada.Directories;
   begin
      if Exists (This.Name) then
         Delete_File (This.Name);
      end if;
   exception
      when E : others =>
         Alire.Utils.Finalize_Exception (E);
   end Finalize;

   --------------
   -- New_File --
   --------------

   function New_File return File is
      type Parts is ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
                     'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
                     'u', 'v', 'w', 'x', 'y', 'z');

      package Rand is new Ada.Numerics.Discrete_Random (Parts);
      Gen : Rand.Generator;

      Suffix : String (1 .. 10);
   begin
      Rand.Reset (Gen);

      for C of Suffix loop
         C := Parts'Image (Rand.Random (Gen))(2);
      end loop;

      declare
         Name : constant String := "alrtmp." & Suffix;
      begin
         return (Ada.Finalization.Limited_Controlled with Name'Length, Name);
      end;
   end New_File;

end Alr.Utils.Temp_File;
