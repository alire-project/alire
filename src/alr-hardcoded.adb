package body Alr.Hardcoded is

   --------------------
   -- Session_Folder --
   --------------------

   function Session_Folder return String is
      Path : constant String := OS.Cache_Folder / "sessions" / "common";
      --  FIXME: right now there are no sessions, only this one for everything
      --  Might not be a problem if alr is rebuild whenever run within an alire project
   begin
      if not Ada.Directories.Exists (Path) then
         OS.Create_Folder (Path);
      end if;
      return Path;
   end Session_Folder;

end Alr.Hardcoded;
