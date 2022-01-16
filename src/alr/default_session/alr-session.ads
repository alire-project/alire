package Alr.Session is

   --  Warning: withing this file will cause the dependent file to be recompiled in every rebuild
   --  Try to minimize dependencies (currently Alr.Self isolates them all, so is the only file rebuilt)

--  private -- To prevent use besides from Alr.Self

   --  Making this private was ideal, but that causes the recompilation of every file depending
   --  on Alr.Session.Child, since it has visibility of the private part. So, in all, it's worse.

   Alr_Src_Folder : constant String  := "";
   --  For alr instances that are session specific, we need a way to locate the src folder
   --    (just for the case where it is not the canonical one, that is: while developing)

   Hash           : constant String := "bootstrap";
   --  In the current per-session setup, this should always match unless the dependencies files has been
   --    tampered with in such a way that its timestamp has not been updated

   Full_Index     : constant Boolean := False;
   --  Some commands require a full index and some others not.
   --  We use this to separate bootstrap from index status

   Session_Build  : constant Boolean := False;
   --  True in session-specific builds

end Alr.Session;
