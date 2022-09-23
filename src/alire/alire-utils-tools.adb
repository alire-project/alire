with Alire.OS_Lib.Subprocess; use Alire.OS_Lib.Subprocess;
with Alire.OS_Lib;
with Alire.Platforms.Current;
with Alire.Origins.Deployers.System;
with CLIC.User_Input;

package body Alire.Utils.Tools is

   Already_Detected : array (Tool_Kind) of Boolean := (others => False);

   function Exec_For_Tool (Tool : Tool_Kind) return String;

   ---------------
   -- Available --
   ---------------

   function Available (Tool : Tool_Kind) return Boolean is
   begin
      if Already_Detected (Tool) then
         return True;
      end if;

      if Locate_In_Path (Exec_For_Tool (Tool)) /= "" then
         --  The tool is available
         Already_Detected (Tool) := True;
      end if;

      return Already_Detected (Tool);
   end Available;

   -------------------
   -- Exec_For_Tool --
   -------------------

   function Exec_For_Tool (Tool : Tool_Kind) return String
   is (case Tool is
          when Easy_Graph => "graph-easy",
          when Git        => "git",
          when Tar        => "tar",
          when Unzip      => "unzip",
          when Curl       => "curl",
          when Mercurial  => "hg",
          when Subversion => "svn");

   -----------------------------
   -- System_Package_For_Tool --
   -----------------------------

   function System_Package_For_Tool (Tool : Tool_Kind) return String is
      use Alire.Platforms;
      use Alire.Platforms.Current;
   begin
      case Distribution is

         when Distro_Unknown =>
            --  Cannot have package for an unknown distribution
            return "";

         when Msys2 | Debian | Ubuntu | Arch | Centos | Fedora | Rhel | Suse
           | Homebrew =>
            return (case Tool is
                       when Easy_Graph =>
                      (if Distribution = Centos or else
                          Distribution = Fedora or else
                          Distribution = Rhel or else
                          Distribution = Suse
                       then "perl-Graph-Easy"
                       elsif Distribution /= Msys2 and Distribution /= Arch
                       then "libgraph-easy-perl"
                       else ""),
                       when Git | Tar | Unzip | Curl => Exec_For_Tool (Tool),
                       when Mercurial  => "mercurial",
                       when Subversion => "subversion");
      end case;
   end System_Package_For_Tool;

   --------------------------
   -- Install_From_Distrib --
   --------------------------

   procedure Install_From_Distrib (Tool : Tool_Kind; Fail : Boolean) is
      use CLIC.User_Input;

      Pck : constant String := System_Package_For_Tool (Tool);
   begin

      if Pck /= "" then

         if Query ("Do you want Alire to install the required tool?",
                   Valid   => (Yes | No => True, others => False),
                   Default => Yes) = Yes
         then
            declare
               use Alire.Origins.Deployers.System;

               Dep    : Deployer'Class := Platform_Deployer (Pck);
               Result : Alire.Outcome;
            begin

               --  We already asked for permission, so disable the user query
               --  of the deployer.
               Dep.Dont_Ask_Permission;

               Result := Dep.Deploy (Folder => "unused");
               Alire.Assert (Result);

               --  Check if installation worked
               if Locate_In_Path (Exec_For_Tool (Tool)) /= "" then
                  --  Good to go
                  return;
               else
                  --  Error when installation failed
                  Trace.Error ("Cannot proceed.");
                  Trace.Error
                    ("Tool still not available after installation...");
               end if;
            end;
         else
            --  Error when user rejected installation
            if Fail then
               Trace.Error ("Cannot proceed.");
               Trace.Error ("Please install the tool and retry.");
            else
               Trace.Info ("Tool not installed.");
               return;
            end if;
         end if;
      else
         --  Error when Alire doesn't know how to install (unknown distro or
         --  tool not available in distro).
         if Fail then
            Trace.Error ("Cannot proceed.");
            Trace.Error ("Alire is not able to install required tool: '" &
                           Tool'Img & "'");
            Trace.Error ("Please install the tool and retry.");
         else
            Trace.Warning ("Alire is not able to install tool: '" &
                             Tool'Img & "'");
            return;
         end if;
      end if;

      OS_Lib.Bailout (1);
   end Install_From_Distrib;

   ----------------
   -- Check_Tool --
   ----------------

   procedure Check_Tool (Tool : Tool_Kind; Fail : Boolean := True) is
   begin

      if Available (Tool) then
         return;
      end if;

      Trace.Info ("Cannot find required tool: " & Tool'Img);

      Install_From_Distrib (Tool, Fail);
   end Check_Tool;

end Alire.Utils.Tools;
