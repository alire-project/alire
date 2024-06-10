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

   subtype Hashing_Context is Context;
   function Get_Digest (C : Context) return String renames Digest;
   --  Reexpose formals to gain visibility outside the generic

   function Hash_File (Path : File_Path) return Any_Hash;
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

   procedure Update (C          : in out Context;
                     S          : String;
                     Append_Nul : Boolean := True);
   --  Convenience to directly hash lists of strings. To avoid ambiguities, by
   --  default a NUL char is used to separate such strings.

end Alire.Hashes.Common;
