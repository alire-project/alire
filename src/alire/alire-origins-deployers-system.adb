with Alire.Origins.Deployers.System.Apt;
with Alire.Origins.Deployers.System.Homebrew;
with Alire.Origins.Deployers.System.Macports;
with Alire.Origins.Deployers.System.Pacman;
with Alire.Origins.Deployers.System.RPM_Wrappers;
with Alire.Origins.Deployers.System.Unknown;
with Alire.Origins.Deployers.System.Zypper;
with Alire.OS_Lib;

with CLIC.User_Input;

with GNAT.IO;

package body Alire.Origins.Deployers.System is

   function Query_User (Pkg : String) return Boolean;

   Always_Install : Boolean := False;

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

         if not This.Ask_Permission or else Query_User (Pkg) then
            return Tool.Install;
         else
            --  User rejected the installation
            return Outcome_Success;
         end if;
      end if;
   exception
      when others =>
         return Outcome_Failure
           ("Installation of " & Pkg & " failed");
   end Deploy;

   ----------------
   -- Query_User --
   ----------------

   function Query_User (Pkg : String) return Boolean is
      use GNAT.IO;
      use CLIC.User_Input;
   begin
      Put_Line ("The system package '" & Pkg &
                  "' is about to be installed.");

      if Always_Install then
         return True;
      end if;

      Put_Line ("This action might require admin privileges " &
                  "and impact your system installation.");

      case Query ("Do you want Alire to install this system package?",
                  Valid   => (Yes | No | Always => True),
                  Default => Yes)
      is
         when Yes =>
            return True;

         when No =>
            Trace.Warning ("Without this system package " &
                             "the build is likely to fail.");
            Continue_Or_Abort;
            return False;

         when Always =>
            Always_Install := True;
            return True;
      end case;

   end Query_User;

   -----------------------
   -- Platform_Deployer --
   -----------------------

   function Platform_Deployer
     (From   : Origins.Origin;
      Distro : Platforms.Distributions := Platforms.Current.Distribution)
      return Deployer'Class
   is (case Platforms.Distro_Manager (Distro) is
         when Platforms.Apt =>
            System.Apt.Deployer'(Deployers.Deployer'(Base => From)
                                 with others => <>),
         when Platforms.Pacman =>
            System.Pacman.Deployer'(Deployers.Deployer'(Base => From)
                                    with others => <>),
         when Platforms.Yum =>
            System.RPM_Wrappers.Deployer'(Deployers.Deployer'(Base => From)
                                          with Wrapper =>
                                             System.RPM_Wrappers.Yum,
                                          others       => <>),
         when Platforms.Dnf =>
            System.RPM_Wrappers.Deployer'(Deployers.Deployer'(Base => From)
                                          with Wrapper =>
                                             System.RPM_Wrappers.Dnf,
                                          others       => <>),
         when Platforms.Zypper =>
            System.Zypper.Deployer'(Deployers.Deployer'(Base => From)
                                    with others => <>),
         when Platforms.Homebrew =>
            System.Homebrew.Deployer'(Deployers.Deployer'(Base => From)
                                      with others => <>),
         when Platforms.Macports =>
            System.Macports.Deployer'(Deployers.Deployer'(Base => From)
                                      with others => <>),
         when Platforms.Packager_Unknown =>
            System.Unknown.Deployer'(Deployers.Deployer'(Base => From)
                                     with others => <>)
     );
      --  NOTE: add here other native package managers as they get
      --  implemented.

   -------------------------
   -- Dont_Ask_Permission --
   -------------------------

   procedure Dont_Ask_Permission (This : in out Deployer) is
   begin
      This.Ask_Permission := False;
   end Dont_Ask_Permission;

   ---------------------
   -- Executable_Name --
   ---------------------

   function Executable_Name return String is
      Make : constant Origin := New_System ("make");
      --  We use a mock system package to be able to obtain a deployer. It
      --  doesn't matter if this system package doesn't exist.
   begin
      return Platform_Deployer
        (Make,
         Distro => Platforms.Current.Detected_Distribution)
        .Executable_Name;
   end Executable_Name;

   ---------------------
   -- Executable_Path --
   ---------------------

   function Executable_Path return Optional_Absolute_Path is
   begin
      if Executable_Name /= "" then
         return OS_Lib.Locate_Exec_On_Path (Executable_Name);
      else
         return "";
      end if;
   end Executable_Path;

end Alire.Origins.Deployers.System;
