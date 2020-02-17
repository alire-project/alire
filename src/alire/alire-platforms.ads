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
                          Distro_Unknown);

   type Word_Sizes is (Bits_32,
                       Bits_64,
                       Bits_Unknown);

   type Package_Managers is (Apt,
                             Packager_Unknown);

end Alire.Platforms;
