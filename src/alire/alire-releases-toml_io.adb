with Ada.Text_IO;

with TOML.Text_IO;

package body Alire.Releases.TOML_IO is

   -------------
   -- To_File --
   -------------

   procedure To_File (R : Release; Filename : String) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Filename);
      TOML.Text_IO.Dump_To_File (R.To_TOML, File);
      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end To_File;

end Alire.Releases.TOML_IO;
