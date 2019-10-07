with Ada.Streams;

generic
   Kind : Kinds; -- The Kind to be computed.

   --  Following types are provided by GNAT.Secure_Hashes.H. Since that is an
   --  internal unit, we extract here the needed types/subprograms instead of
   --  getting a package formal.
   type Context is private;
   with procedure Update (C     : in out Context;
                          Input : Ada.Streams.Stream_Element_Array) is <>;
   with function Digest (C : Context) return String is <>;
package Alire.Hashes.Common is

   function Hash_Directory (Path   : Platform_Independent_Path;
                            Except : Platform_Independent_Path := "")
                            return Any_Hash;
   --  See documentation in Alire.Hashes. Path must designate a folder or
   --  Checked_Error will be raised.

   function Hash_File (Path : Platform_Independent_Path) return Any_Hash;
   --  This function does not need to be visible (it is not used directly), but
   --  hiding it in the body results in the following error in FSF compilers:
   --
   --     Bind
   --     [gprbind]      alr-main.bexch
   --     [Ada]          alr-main.ali
   --
   --  raised SYSTEM.ASSERTIONS.ASSERT_FAILURE : binde.adb:1117
   --  gprbind: invocation of gnatbind failed
   --  gprbuild: unable to bind alr-main.adb

end Alire.Hashes.Common;
