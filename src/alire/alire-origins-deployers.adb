with Ada.Directories;

with Alire.Config;
with Alire.Origins.Deployers.APT;
with Alire.Origins.Deployers.Filesystem;
with Alire.Origins.Deployers.Git;
with Alire.Origins.Deployers.Hg;
with Alire.Origins.Deployers.Source_Archive;
with Alire.Origins.Deployers.SVN;
with Alire.Platform;

with GNAT.IO;

package body Alire.Origins.Deployers is

--     use all type Alire.Origins.Kinds;

   ------------------
   -- New_Deployer --
   ------------------

   function New_Deployer (From : Origin) return Deployer'Class is
   begin
      case From.Kind is
         when Origins.Filesystem =>
            return Filesystem.Deployer'(Deployer'(Base => From)
                                        with null record);
         when Origins.Git =>
            return Git.Deployer'(Deployer'(Base => From) with null record);

         when Alire.Origins.Hg =>
            return Hg.Deployer'(Deployer'(Base => From) with null record);

         when Alire.Origins.SVN =>
            return SVN.Deployer'(Deployer'(Base => From) with null record);

         when Alire.Origins.Source_Archive =>
            return Source_Archive.Deployer'(Deployer'(Base => From)
                                            with null record);
         when Alire.Origins.Native =>
            --  TODO: during native refactoring, deal with non-apt pkg managers
            return APT.Deployer'(Deployer'(Base => From) with null record);

      end case;
   end New_Deployer;

   -----------------------
   -- Deploy_Not_Native --
   -----------------------

   function Deploy_Not_Native (From   : Origin;
                               Folder : String) return Outcome is
   begin
      return New_Deployer (From).Deploy (Folder);
   exception
      when E : others =>
         Log_Exception (E);
         if Ada.Directories.Exists (Folder) then
            Ada.Directories.Delete_Tree (Folder);
         end if;
         return Outcome_Failure ("Deployment of " & From.Image
                                 & " to " & Folder & " failed");
   end Deploy_Not_Native;

   Native_Proceed : Boolean := False;

   ---------------------
   -- Install_Warning --
   ---------------------

   procedure Install_Warning (From : Deployer'Class) is
      use GNAT.IO;

      Native_Name : constant String :=
        From.Base.Package_Name (Platform.Distribution);
   begin
      if From.Already_Installed then
         Trace.Detail ("Package " & Native_Name & " is already installed");
      elsif not Native_Proceed then
         New_Line;
         Put_Line ("The native package " & Native_Name &
                     " is about to be installed");
         Put_Line ("This action requires sudo privileges " &
                     "and might impact your system installation");
         New_Line;
         Config.Enter_Or_Ctrl_C;
         Native_Proceed := True;
      end if;
   end Install_Warning;

   --------------------
   -- Install_Native --
   --------------------

   function Install_Native (Release : Releases.Release) return Outcome is
      From : constant Origin         := Release.Origin;
      Orig : constant Deployer'Class := New_Deployer (From);
   begin
      if From.All_Native_Names (Platform.Distribution) = Origins.Unavailable
      then
         return Outcome_Failure
           ("No native package known in current platform for "
            & Release.Milestone.Image);
      elsif Orig.Already_Installed then
         Trace.Detail (From.Package_Name (Platform.Distribution) &
                         " already installed");
         return Outcome_Success;
      else
         Install_Warning (Orig);
         return Orig.Deploy (Folder => "");
      end if;
   exception
      when others =>
         return Outcome_Failure
           ("Installation of " &
              From.Package_Name (Platform.Distribution) & " failed");
   end Install_Native;

   ------------
   -- Deploy --
   ------------

   function Deploy (Release : Releases.Release;
                    Folder  : String := "") return Outcome
   is
      From : constant Origin := Release.Origin;
   begin
      if From.Is_Native then
         if Platform.Distribution_Is_Known then
            return Install_Native (Release);
         else
            return Outcome_Failure
              ("Unknown distribution: cannot provide native package for "
               & Release.Milestone.Image);
         end if;
      else
         return Deploy_Not_Native (From, Folder);
      end if;
   end Deploy;

   ------------
   -- Deploy --
   ------------

   function Deploy (This : Deployer; Folder : String) return Outcome
   is (raise Program_Error with "should never be called for base class");

end Alire.Origins.Deployers;
