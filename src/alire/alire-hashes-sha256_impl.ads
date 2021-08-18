with Alire.Hashes.Common;

with GNAT.SHA256;

package Alire.Hashes.SHA256_Impl is new Alire.Hashes.Common
  (Kind    => SHA256,
   Context => GNAT.SHA256.Context,
   Update  => GNAT.SHA256.Update,
   Digest  => GNAT.SHA256.Digest);
