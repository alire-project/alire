package Alire.Platforms with Preelaborate is

   --  Platform information necessary for some releases

   type Operating_Systems is (Linux,
                              MacOS,
                              Windows,
                              OS_Unknown);
   subtype Known_Operating_Systems is
      Operating_Systems range Linux .. Windows;

   type Targets is (Native,
                    Unknown_Cross_Target);
   --  Minimal preparations for cross-compiling

   type Distributions is (Debian,
                          Ubuntu,
                          Msys2,
                          Distro_Unknown);

   subtype Known_Distributions is
     Distributions range Debian .. Distributions'Pred (Distributions'Last);

   type Word_Sizes is (Bits_32,
                       Bits_64,
                       Bits_Unknown);

   type Package_Managers is (Apt,
                             Pacman,
                             Packager_Unknown);

   Distro_Manager : constant array (Distributions) of Package_Managers :=
     (Debian | Ubuntu => Apt,
      Msys2           => Pacman,
      Distro_Unknown  => Packager_Unknown);

   type Toolchains is (System,
                       --  Provided through system packages, able to use other
                       --  Ada system packages

                       User
                       --  Provided by the user
                      );

   type Shells is (Unix, PowerShell, WinCmd);

end Alire.Platforms;
