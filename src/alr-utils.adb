with Ada.Streams;
with Ada.Streams.Stream_IO;

with GNAT.Case_Util;
with GNAT.SHA1;

package body Alr.Utils is

-------------------
-- To_Mixed_Case --
-------------------

   function To_Mixed_Case (S : String) return String is
   begin
      return SMC : String := S do
         GNAT.Case_Util.To_Mixed (SMC);
      end return;
   end To_Mixed_Case;

   ---------------
   -- Hash_File --
   ---------------

   function Hash_File (Path : String) return String is
      use Ada.Streams;

      Buffer  : Stream_Element_Array (1 .. 1024);
      File    : Stream_IO.File_Type;
      Context : aliased GNAT.SHA1.Context;
      Hasher  : GNAT.SHA1.Hash_Stream (Context'Access);
      Last    : Stream_Element_Offset;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, Path);

      while not Stream_IO.End_Of_File (File) loop
         Stream_IO.Read (File, Buffer, Last);
         Hasher.Write (Buffer (Buffer'First .. Last));
      end loop;

      Stream_IO.Close (File);

      return GNAT.SHA1.Digest (Context);
   end Hash_File;

end Alr.Utils;
