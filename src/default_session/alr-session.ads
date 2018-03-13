package Alr.Session is

   --  NOTE: ALIASED & VOLATILE IS NECESSARY TO AVOID OPTIMIZATIONS
   --  Otherwise, the reuse of object code among alr builds can cause improper behavior

   --  Still, I'm seeing warnings about expressions being always true/false about these...
   --  Not sure what's going on

   --  Warning: withing this file will cause the dependent file to be recompiled in every rebuild
   --  Try to minimize dependencies (currently Alr.Self isolates them all, so is the only file rebuilt)

private -- To prevent use besides from Alr.Self

   Alr_Src_Folder : aliased String  := "" with Volatile;
   --  For alr instances that are session specific, we need a way to locate the src folder
   --    (just for the case where it is not the canonical one, that is: while developing)

   Hash           : aliased String := "bootstrap" with Volatile;
   --  In the curren per-session setup, this should always match unless the dependencies files has been
   --    tampered with in such a way that its timestamp has not been updated

   Full_Index     : aliased Boolean := False with Volatile;
   --  Some commands require a full index and some others not.
   --  We use this to separate bootstrap from index status

   Session_Build  : aliased Boolean := False with Volatile;
   --  True in session-specific builds

end Alr.Session;
