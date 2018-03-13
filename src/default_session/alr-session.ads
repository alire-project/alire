package Alr.Session is

   --  Warning: withing this file will cause the dependent file to be recompiled in every rebuild
   --  Try to minimize dependencies (currently Alr.Self isolates them all, so is the only file rebuilt)

   Alr_Src_Folder : constant String  := "" with Warnings => Off;
   --  For alr instances that are session specific, we need a way to locate the src folder
   --    (just for the case where it is not the canonical one, that is: while developing)

   Hash           : constant String := "bootstrap";
   --  In the curren per-session setup, this should always match unless the dependencies files has been
   --    tampered with in such a way that its timestamp has not been updated

   Full_Index     : constant Boolean := False;
   --  Some commands require a full index and some others not.
   --  We use this to separate bootstrap from index status

   Session_Build  : constant Boolean := False;
   --  True in session-specific builds

end Alr.Session;
