with Alr.Spawn;

with GNAT.OS_Lib;

package body Alr.Origins.Hg is

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch (This : Origin; Folder : String) is
      use GNAT.OS_Lib;
   begin
      if Locate_Exec_On_Path ("hg") = null then
         Trace.Error ("hg not found in path, aborting");
         raise Command_Failed;
      end if;

      Trace.Detail ("Checking out: " & This.Base.URL);
      Spawn.Command ("hg", "clone -v -y -u " & This.Base.Commit & " " & This.Base.URL & " " & Folder);
   exception
      when others =>
         raise Command_Failed;
   end Fetch;

   -------------
   -- Install --
   -------------

   overriding procedure Install (This : Origin) is
   begin
      raise Program_Error;
   end Install;

end Alr.Origins.Hg;
