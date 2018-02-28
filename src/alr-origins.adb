with Ada.Directories;

with Alr.OS_Lib;
with Alr.Spawn;
with Alr.Utils;

with GNAT.IO;

package body Alr.Origins is

   use all type Alire.Origins.Kinds;

   type Fetcher is access procedure (From : Alire.Origins.Origin; Folder : String);

   ------------------------------
   -- Already_Available_Native --
   ------------------------------

   function Already_Available_Native (Origin : Alire.Origins.Origin) return Boolean is
      Output : Utils.String_Vector;
   begin
      case Origin.Kind is

         when Apt =>
            Output := OS_Lib.Spawn_And_Capture ("apt-cache", "policy " & origin.id);
            for Line of Output loop
               if Utils.Contains (Line, "Installed") and then not Utils.Contains (Line, "none") then
                  return True;
               end if;
            end loop;

         when others =>
            raise Program_Error with "Shouldn't be used";
      end case;

      return False;
   end Already_Available_Native;

   Native_Proceed : Boolean := False;

   ---------
   -- Apt --
   ---------

   procedure Apt (From : Alire.Origins.Origin; Folder : String) is
      pragma Unreferenced (Folder);
      use GNAT.IO;

      Foo : String := "bar";
      Bar : Integer;
   begin
      if Already_Available_Native (From) then
         Trace.Detail ("Package " & From.Id & " is already installed");
      elsif not Native_Proceed then
         New_Line;
         Put_Line ("The native package " & From.Id & " is about to be installed using apt");
         Put_Line ("This action requires sudo privileges and might impact your system installation");
         New_Line;
         Put_Line ("Press Enter to continue or Ctrl-C to abort");
         Get_Line (Foo, Bar);
         Native_Proceed := True;
      end if;

      OS_Lib.Spawn_Raw ("sudo", "apt-get install -q -q -y " & From.Id);
   exception
      when others =>
         Trace.Error ("Installation of native package " & From.Id & " failed");
         raise Command_Failed;
   end Apt;

   ----------
   -- Fail --
   ----------

   procedure Fail (From : Alire.Origins.Origin; Folder : String) is
   begin
      raise Program_Error with "Should never be requested";
   end Fail;

   ---------
   -- Git --
   ---------

   procedure Git (From : Alire.Origins.Origin; Folder : String) is
   begin
      Trace.Info ("Checking out: " & From.URL);
      Spawn.Command ("git", "clone -n -q --progress " & From.URL & " " & Folder);
      declare
         Guard : constant OS_Lib.Folder_Guard := Os_Lib.Enter_Folder (Folder) with Unreferenced;
      begin
         Spawn.Command ("git", "reset --hard -q " & From.Id);
      end;
   exception
      when others =>
         raise Command_Failed;
   end Git;

   --------
   -- Hg --
   --------

   procedure Hg (From : Alire.Origins.Origin; Folder : String) is
   begin
      Trace.Info ("Checking out: " & From.URL);
      Spawn.Command ("hg", "clone -v -y -u " & From.Id & " " & From.URL & " " & Folder);
   exception
      when others =>
         raise Command_Failed;
   end Hg;

   Fetchers : constant array (Alire.Origins.Kinds) of Fetcher :=
                (Filesystem => Fail'Access,
                 Git        => Git'Access,
                 Hg         => Hg'Access,
                 Apt        => Apt'Access);

   -----------
   -- Fetch --
   -----------

   procedure Fetch (From : Alire.Origins.Origin; Folder : String) is
   begin
      Fetchers (From.Kind).all (From, Folder);
   exception
      when others =>
         Trace.Error ("Deployment of " & From.Id & " from " & From.URL & " to " & Folder & " failed");
         if Ada.Directories.Exists (Folder) then
            Ada.Directories.Delete_Tree (Folder);
         end if;
         raise;
   end Fetch;

   ------------------
   -- Fetch_Native --
   ------------------

   procedure Fetch_Native (From : Alire.Origins.Origin) is
   begin
      Fetch (From, "<platform native>");
   end Fetch_Native;

end Alr.Origins;
