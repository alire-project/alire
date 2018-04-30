with Alr.Interactive;
with Alr.OS_Lib;
with Alr.Spawn;

package body Alr.Origins.Git is

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch (This : Origin; Folder : String) is
      Extra : constant String :=
                (if Interactive.Not_Interactive
                 then ""
                 else "--progress ");
   begin
      Trace.Detail ("Checking out: " & This.Base.URL);
      Spawn.Command ("git", "clone -n -q " & Extra & This.Base.URL & " " & Folder);
      declare
         Guard : constant OS_Lib.Folder_Guard := Os_Lib.Enter_Folder (Folder) with Unreferenced;
      begin
         Spawn.Command ("git", "reset --hard -q " & This.Base.Commit);
      end;
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

end Alr.Origins.Git;
