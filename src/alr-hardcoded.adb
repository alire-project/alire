with Alr.Utils;

package body Alr.Hardcoded is

   Cached : access String;

   --------------------
   -- Session_Folder --
   --------------------

   function Session_Folder (Metafile : String) return String is
   begin
      if Cached /= null then
         return Cached.all;
      else
         declare
            use Ada.Directories;

            Base : constant String := Containing_Directory (Full_Name (Metafile));
            Path : constant String := Platform.Cache_Folder / "sessions" /
                                      Platform.Compiler'Img /
                                      Utils.Hash_String (Base) & "_" & Base_Name (Metafile);
            --  FIXME: right now there are no sessions, only this one for everything
            --  Not a problem as long as alr remains an interactive, single-user tool
         begin
            Trace.Debug ("Session folder is " & Path);

            Cached := new String'(Path);

            return Path;
         end;
      end if;
   end Session_Folder;

end Alr.Hardcoded;
