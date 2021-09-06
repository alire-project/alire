with AAA.Strings; use AAA.Strings;

package Alire.Hashes with Preelaborate is

   --  To allow use of various hashes, we expect that any string in the index
   --  providing a hash has the format: "kind:value" where kind is the hash
   --  type and value is the actual hash representation emitted by the GNAT.*
   --  hashing functions. E.g.: "sha1:5c16c1c74ae8236770644b69f2e4cf1ccc88adad"

   type Kinds is (SHA256, SHA512);
   --  Recognized hashes that we are able to compute/verify. To add a new kind,
   --  instance the Alire.Hashes.Common generic and with it in Alire.TOML_Index
   --  body.

   Default : constant Kinds := SHA512;
   --  In the event we introduce several hashes, this default is considered the
   --  strongest/preferred one for any new hashes to be generated going forward

   type Any_Digest is new String with
     Dynamic_Predicate =>
       (for all Char of Any_Digest => Char in 'a' .. 'f' | '0' .. '9');
   --  Just the actual hash part, in hexadecimal encoding.

   type Any_Hash is new String with
     Dynamic_Predicate => Is_Well_Formed (String (Any_Hash));
   --  A string with "kind:digest" format.

   function New_Hash (Kind : Kinds; Digest : Any_Digest) return Any_Hash;

   function Digest (Hash : Any_Hash) return Any_Digest;
   --  Return the actual fingerprint without the kind prefix.

   function Hash_File (Kind : Kinds;
                       Path : File_Path) return Any_Hash;
   --  Compute a particular hash kind. May raise usual file exceptions.

   function Is_Known (Kind_Img : String) return Boolean;
   --  Check if a string is one of our understood hash types.

   function Is_Well_Formed (Hash_Img : String) return Boolean;
   --  Check if a string follows proper syntax ("kind:digest").

   function Kind (Hash : Any_Hash) return Kinds;
   --  Given a well-formed Hash, extract its typed kind.

private

   --------------------
   -- Hash_Functions --
   --------------------

   --  The following function array is initialized by each instance of a
   --  known hash in child packages.

   Hash_Functions : array (Kinds) of access
     function (File : File_Path) return Any_Hash;

   ---------------
   -- Hash_File --
   ---------------

   function Hash_File (Kind : Kinds;
                       Path : File_Path) return Any_Hash is
     (Hash_Functions (Kind) (Path)); -- Dispatch to a particular implementation

   --------------
   -- Is_Known --
   --------------

   function Is_Known (Kind_Img : String) return Boolean is
     (for some Kind in Kinds =>
         To_Lower_Case (Kind'Img) = To_Lower_Case (Kind_Img));

   --------------------
   -- Is_Well_Formed --
   --------------------

   function Is_Well_Formed (Hash_Img : String) return Boolean is
     (for some Char of Hash_Img => Char = ':' and then
      Is_Known (Head (Hash_Img, ':')));

   ----------
   -- Kind --
   ----------

   function Kind (Hash : Any_Hash) return Kinds is
     (Kinds'Value (Head (String (Hash), ':')));

   --------------
   -- New_Hash --
   --------------

   function New_Hash (Kind : Kinds; Digest : Any_Digest) return Any_Hash is
     (Any_Hash (String'(To_Lower_Case (Kind'Img) & ":" & String (Digest))));

end Alire.Hashes;
