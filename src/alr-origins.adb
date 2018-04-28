with Ada.Directories;

with Alire.Platforms;

with Alr.Interactive;
with Alr.Origins.Apt;
with Alr.Origins.Git;
with Alr.Origins.Hg;
with Alr.Platform;

with GNAT.IO;

package body Alr.Origins is

   use all type Alire.Origins.Kinds;

   ----------------
   -- New_Origin --
   ----------------

   function New_Origin (From : Alire.Origins.Origin) return Origin'Class is
   begin
      case From.Kind is
         when Alire.Origins.Git =>
            return Git.Origin'(Origin'(Base => From) with null record);

         when Alire.Origins.Hg =>
            return Hg.Origin'(Origin'(Base => From) with null record);

         when Alire.Origins.Native =>
            case Alire.Platforms.Package_Manager (Platform.Distribution) is
               when Alire.Platforms.Apt =>
                  return Apt.Origin'(Origin'(Base => From) with null record);

               when others =>
                  raise Program_Error with "Unsupported package manager";
            end case;
         when others =>
            raise Program_Error with "Unsupported origin";
      end case;
   end New_Origin;

   -----------
   -- Fetch --
   -----------

   procedure Fetch (From : Alire.Origins.Origin; Folder : String) is
   begin
      New_Origin (From).Fetch (Folder);
   exception
      when others =>
         Trace.Error ("Deployment of " & From.Image & " to " & Folder & " failed");
         if Ada.Directories.Exists (Folder) then
            Ada.Directories.Delete_Tree (Folder);
         end if;
         raise;
   end Fetch;

   ----------------------
   -- Fetch_Or_Install --
   ----------------------

   procedure Fetch_Or_Install (From : Alire.Origins.Origin; Folder : String) is
   begin
      if From.Is_Native then
         Install_Native (From);
      else
         Fetch (From, Folder);
      end if;
   end Fetch_Or_Install;

   Native_Proceed : Boolean := False;

   ---------------------
   -- Install_Warning --
   ---------------------

   procedure Install_Warning (From : Origin'Class) is
      use GNAT.IO;

      Native_Name : constant String := From.Base.Package_Name (Platform.Distribution);
   begin
      if From.Already_Installed then
         Trace.Detail ("Package " & Native_Name & " is already installed");
      elsif not Native_Proceed then
         New_Line;
         Put_Line ("The native package " & Native_Name & " is about to be installed");
         Put_Line ("This action requires sudo privileges and might impact your system installation");
         New_Line;
         Interactive.Enter_Or_Ctrl_C;
         Native_Proceed := True;
      end if;
   end Install_Warning;

   --------------------
   -- Install_Native --
   --------------------

   procedure Install_Native (From : Alire.Origins.Origin) is
      Orig : constant Origin'Class := New_Origin (From);
   begin
      if Orig.Already_Installed then
         Trace.Detail (From.Package_Name (Platform.Distribution) & " already installed");
      else
         Install_Warning (Orig);
         Orig.Install;
      end if;
   exception
      when others =>
         Trace.Error ("Installation of " & From.Package_Name (Platform.Distribution) & " failed");
         raise Command_Failed;
   end Install_Native;

end Alr.Origins;
