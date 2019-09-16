with Alire.Utils;

package Alire.Hashes with Preelaborate is

   --  To allow use of various hashes, we expect that any string in the index
   --  providing a hash has the format: "kind:value" where kind is the hash
   --  type and value is the actual hash representation emitted by the GNAT.*
   --  hashing functions. E.g.: "sha1:5c16c1c74ae8236770644b69f2e4cf1ccc88adad"

   type Kinds is (SHA512);
   --  Recognized hashes that we are able to compute/verify.
   --  To add a new kind, instance the Alire.Hashes.Common generic and with it
   --  in Alire.TOML_Index body.

   subtype Any_Hash is String with
     Dynamic_Predicate => Is_Well_Formed (Any_Hash);

   function Digest (Hash : Any_Hash) return String;
   --  Return the actual fingerprint without the kind prefix.

   function Hash_File (Kind : Kinds;
                       Path : Platform_Independent_Path) return Any_Hash;
   --  Compute a particular hash kind. May raise usual file exceptions.

   function Is_Known (Kind_Img : String) return Boolean;
   --  Check if a string is one of our understood hash types.

   function Is_Well_Formed (Hash_Img : String) return Boolean;
   --  Check if a string follows proper syntax ("kind:digest").

   function Kind (Hash : Any_Hash) return Kinds;
   --  Given a well-formed Hash, extract its typed kind.

private

   ------------
   -- Digest --
   ------------

   function Digest (Hash : Any_Hash) return String is
     (Utils.Tail (Hash, ':'));

   --------------------
   -- Hash_Functions --
   --------------------

   --  The following function array is initialized by each instance of a
   --  known hash in child packages.

   Hash_Functions : array (Kinds) of access
     function (File : Platform_Independent_Path) return Any_Hash;

   ---------------
   -- Hash_File --
   ---------------

   function Hash_File (Kind : Kinds;
                       Path : Platform_Independent_Path) return Any_Hash is
     (Hash_Functions (Kind) (Path)); -- Dispatch to a particular implementation

   --------------
   -- Is_Known --
   --------------

   function Is_Known (Kind_Img : String) return Boolean is
     (for some Kind in Kinds =>
         Utils.To_Lower_Case (Kind'Img) = Utils.To_Lower_Case (Kind_Img));

   --------------------
   -- Is_Well_Formed --
   --------------------

   function Is_Well_Formed (Hash_Img : String) return Boolean is
     (for some Char of Hash_Img => Char = ':' and then
      Is_Known (Utils.Head (Hash_Img, ':')));

   ----------
   -- Kind --
   ----------

   function Kind (Hash : Any_Hash) return Kinds is
     (Kinds'Value (Utils.Head (Hash, ':')));

end Alire.Hashes;
