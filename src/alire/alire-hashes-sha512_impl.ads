with Alire.Hashes.Common;

with GNAT.SHA512;

package Alire.Hashes.SHA512_Impl is new Alire.Hashes.Common
  (Kind    => SHA512,
   Context => GNAT.SHA512.Context,
   Update  => GNAT.SHA512.Update,
   Digest  => GNAT.SHA512.Digest);
