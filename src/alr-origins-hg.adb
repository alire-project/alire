with Alr.Spawn;

package body Alr.Origins.Hg is

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch (This : Origin; Folder : String) is
   begin
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
