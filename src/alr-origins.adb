with Ada.Directories;

with Alire.Platforms;

with Alr.Interactive;
with Alr.Origins.Git;
with Alr.OS_Lib;
with Alr.OS;
with Alr.Spawn;
with Alr.Utils;

with GNAT.IO;

package body Alr.Origins is

   use all type Alire.Origins.Kinds;

   function New_Origin (From : Alire.Origins.Origin) return Origin'Class is
   begin
      case From.Kind is
         when Alire.Origins.Git =>
            declare
               O : Alr.Origins.Git.Origin;
            begin
            O.Base := From;
               return O;
            end;
         when others =>
            raise Program_Error;
      end case;
   end New_Origin;

   use all type Alire.Platforms.Package_Managers;

   type Fetcher is access procedure (From : Alire.Origins.Origin; Folder : String);

   ------------------------------
   -- Already_Available_Native --
   ------------------------------

   function Already_Available_Native (Origin : Alire.Origins.Origin) return Boolean is
      Output : Utils.String_Vector;
   begin
      case Origin.Kind is

         when Native =>
            case Alire.Platforms.Package_Manager (OS.Distribution) is
               when Apt =>
                  Output := OS_Lib.Spawn_And_Capture ("apt-cache", "policy " &
                                                        Origin.Package_Name (OS.Distribution));
                  for Line of Output loop
                     if Utils.Contains (Line, "Installed") and then not Utils.Contains (Line, "none") then
                        return True;
                     end if;
                  end loop;
               when others =>
                  raise Program_Error with "Unsupported platform";
            end case;

         when others =>
            raise Program_Error with "Shouldn't be used";
      end case;

      return False;
   end Already_Available_Native;

   ---------------------------
   -- Native_Package_Exists --
   ---------------------------

   function Native_Package_Exists (Name : String) return Boolean is
      Output : constant Utils.String_Vector :=
                 OS_Lib.Spawn_And_Capture ("apt-cache", "-q policy " & Name);
      use Utils;
   begin
      for Line of Output loop
         if Contains (To_Lower_Case (Line), "candidate:") and then
           not Contains (To_Lower_Case (Line), "none") then
            return True;
         end if;
      end loop;

      return False;
   end Native_Package_Exists;

   Native_Proceed : Boolean := False;

   ------------
   -- Native --
   ------------

   procedure Native (From : Alire.Origins.Origin; Folder : String) is
      pragma Unreferenced (Folder);
      use GNAT.IO;

      Native_Name : constant String := From.Package_Name (OS.Distribution);
   begin
      if Already_Available_Native (From) then
         Trace.Detail ("Package " & Native_Name & " is already installed");
      elsif not Native_Proceed then
         New_Line;
         Put_Line ("The native package " & Native_Name & " is about to be installed using apt");
         Put_Line ("This action requires sudo privileges and might impact your system installation");
         New_Line;
         Interactive.Enter_Or_Ctrl_C;
         Native_Proceed := True;
      end if;

      case Alire.Platforms.Package_Manager (OS.Distribution) is
         when Apt =>
            OS_Lib.Spawn_Raw ("sudo", "apt-get install -q -q -y " & Native_Name);
         when others =>
            raise Program_Error with "Unsupported platform";
      end case;
   exception
      when others =>
         Trace.Error ("Installation of native package " & Native_Name & " failed");
         raise Command_Failed;
   end Native;

   ----------
   -- Fail --
   ----------

   procedure Fail (From : Alire.Origins.Origin; Folder : String) is
   begin
      raise Program_Error with "Should never be requested";
   end Fail;

   --------
   -- Hg --
   --------

   procedure Hg (From : Alire.Origins.Origin; Folder : String) is
   begin
      Trace.Detail ("Checking out: " & From.URL);
      Spawn.Command ("hg", "clone -v -y -u " & From.Commit & " " & From.URL & " " & Folder);
   exception
      when others =>
         raise Command_Failed;
   end Hg;

   Fetchers : constant array (Alire.Origins.Kinds) of Fetcher :=
                (Filesystem => Fail'Access,
                 Hg         => Hg'Access,
                 Native     => Native'Access,
                 others     => null);

   -----------
   -- Fetch --
   -----------

   procedure Fetch (From : Alire.Origins.Origin; Folder : String) is
   begin
      Fetchers (From.Kind).all (From, Folder);
   exception
      when others =>
         if From.Kind = Native then
            Trace.Error ("Deployment of " & From.Package_Name (OS.Distribution) & " failed");
         else
            Trace.Error ("Deployment of " & From.Image & " to " & Folder & " failed");
            if Ada.Directories.Exists (Folder) then
               Ada.Directories.Delete_Tree (Folder);
            end if;
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
