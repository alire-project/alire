with Ada.Streams.Stream_IO;

package body Alire.Hashes.Common is

   ---------------
   -- Hash_File --
   ---------------

   function Hash_File (Path : File_Path) return Any_Hash is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      Ctxt : Context;
      File : File_Type;
      Buff : Stream_Element_Array (1 .. 1024 * 1024); -- 1MiB
      Last : Stream_Element_Offset;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Read (File, Buff, Last);
         Update (Ctxt, Buff (Buff'First .. Last));
      end loop;
      Close (File);

      return New_Hash (Kind, Any_Digest (Digest (Ctxt)));

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Hash_File;

   ------------
   -- Update --
   ------------

   procedure Update (C          : in out Context;
                     S          : String;
                     Append_Nul : Boolean := True)
   is
      use Ada.Streams;
      Bytes : Stream_Element_Array (1 .. S'Length)
        with Address => S (S'First)'Address, Import;
      pragma Assert (Bytes'Size = S (S'Range)'Size);
   begin
      Update (C, Bytes);
      if Append_Nul then
         Update (C, Stream_Element_Array'(1 .. 1 => 0));
      end if;
   end Update;

begin
   Hashes.Hash_Functions (Kind) := Hash_File'Access;
end Alire.Hashes.Common;
