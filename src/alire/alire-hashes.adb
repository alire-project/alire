package body Alire.Hashes is

   ------------
   -- Digest --
   ------------

   function Digest (Hash : Any_Hash) return Any_Digest is
   begin
      return Any_Digest (Utils.Tail (String (Hash), ':'));
   exception
      when E : others =>
         Log_Exception (E);
         Raise_Checked_Error ("Malformed hash: " & String (Hash));
   end Digest;

end Alire.Hashes;
