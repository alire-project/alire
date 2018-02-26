with Ada.Streams;
with Ada.Streams.Stream_IO;

with GNAT.Case_Util;
with GNAT.SHA1;

package body Alr.Utils is

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

   -------------
   -- Replace --
   -------------

   function Replace (Text : String; Match : String; Subst : String) return String is
      use Ada.Strings.Fixed;
      First : Natural;
   begin
      First := Index (Text, Match);
      if First = 0 then
         return Text;
      else
         return Replace (Replace_Slice (Text, First, First + Match'Length - 1, Subst), Match, Subst);
      end if;
   end Replace;

end Alr.Utils;
