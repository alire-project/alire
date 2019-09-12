package Alire.Platforms with Preelaborate is

   --  Platform information necessary for some releases

   type Compilers is (GNAT_Unknown,

                      GNAT_FSF_Old,
                      GNAT_FSF_7_2, -- Has known bugs compared to 2017
                      GNAT_FSF_7_3_Or_Newer,

                      GNAT_GPL_Old,
                      GNAT_GPL_2017,

                      GNAT_Community_2018);
   --  We do a minimum of future proofing. Unless newer version have known
   --  problems we don't need to isolate their versions.

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
                          Distro_Unknown);

   type Word_Sizes is (Bits_32,
                       Bits_64,
                       Bits_Unknown);

   type Package_Managers is (Apt,
                             Packager_Unknown);

   function Package_Manager (D : Distributions) return Package_Managers is
     (case D is
         when Debian | Ubuntu => Apt,
         when others          => Packager_Unknown);

end Alire.Platforms;
