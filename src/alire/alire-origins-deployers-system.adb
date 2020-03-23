with Alire.Config;
with Alire.Origins.Deployers.System.Apt;
with Alire.Origins.Deployers.System.Pacman;
with Alire.Platform;
with Alire.Platforms;

with GNAT.IO;

package body Alire.Origins.Deployers.System is

   procedure Install_Warning (Pkg : String);

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
      pragma Unreferenced (Folder);
      Tool : constant System.Deployer'Class := Platform_Deployer (This.Base);
      Pkg  : constant String                := This.Base.Package_Name;
   begin
      if Tool.Already_Installed then
         Trace.Detail (Pkg & " already installed natively");
         return Outcome_Success;
      else
         Install_Warning (Pkg);
         return Tool.Install;
      end if;
   exception
      when others =>
         return Outcome_Failure
           ("Installation of " & Pkg & " failed");
   end Deploy;

   System_Proceed : Boolean := False;

   ---------------------
   -- Install_Warning --
   ---------------------

   procedure Install_Warning (Pkg : String) is
      use GNAT.IO;
   begin
      if not System_Proceed then
         New_Line;
         Put_Line ("The system package " & Pkg &
                     " is about to be installed");
         Put_Line ("This action requires sudo privileges " &
                     "and might impact your system installation");
         New_Line;
         Config.Enter_Or_Ctrl_C;
         System_Proceed := True;
      end if;
   end Install_Warning;

   -----------------------
   -- Platform_Deployer --
   -----------------------

   function Platform_Deployer (From : Origins.Origin) return Deployer'Class is
     (case Platforms.Distro_Manager (Platform.Distribution) is
         when Platforms.Apt | Platforms.Packager_Unknown =>
            System.Apt.Deployer'(Deployers.Deployer'(Base => From)
                                 with null record),
         when Platforms.Pacman =>
            System.Pacman.Deployer'(Deployers.Deployer'(Base => From)
                                    with null record));
      --  TODO: add here other native package managers as they get
      --  implemented.

end Alire.Origins.Deployers.System;
