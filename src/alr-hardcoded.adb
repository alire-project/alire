with Alr.OS_Lib;

package body Alr.Hardcoded is

   --------------------
   -- Session_Folder --
   --------------------

   function Session_Folder return String is
      Path : constant String := OS.Cache_Folder / "sessions" / "common";
      --  FIXME: right now there are no sessions, only this one for everything
      --  Not a problem as long as alr remains an interactive, single-user tool
   begin
      if not Ada.Directories.Exists (Path) then
         OS_Lib.Create_Folder (Path);
      end if;
      return Path;
   end Session_Folder;

end Alr.Hardcoded;
