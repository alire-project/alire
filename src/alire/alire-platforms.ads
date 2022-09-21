package Alire.Platforms with Preelaborate is

   --  Platform information necessary for some releases

   type Extended_Architectures is
     (AMD64, -- Equivalent to X86_64 (FreeBSD)
      ARM64, -- Equivalent to AARCH64
      End_Of_Duplicates,
      --  Up to this point, these are architectures that we want to rename to
      --  some of the following because they are equivalent.
      ARM,
      AARCH64,
      AARCH64_BE,
      I386,
      I686,
      X86_64,
      Architecture_Unknown);

   subtype Architectures is Extended_Architectures range
     Extended_Architectures'Succ (End_Of_Duplicates) .. Architecture_Unknown;
   --  See e.g. https://stackoverflow.com/a/45125525/761390

   type Operating_Systems is (FreeBSD,
                              Linux,
                              MacOS,
                              Windows,
                              OS_Unknown);
   subtype Known_Operating_Systems is
     Operating_Systems range
       Operating_Systems'First .. Operating_Systems'Pred (OS_Unknown);

   type Targets is (Native,
                    Unknown_Cross_Target);
   --  Minimal preparations for cross-compiling

   type Distributions is (Debian,
                          Ubuntu,
                          Msys2,
                          Arch,
                          Rhel, -- RedHat Enterprise Linux
                          Centos,
                          Fedora,
                          Suse,
                          Homebrew,
                          Distro_Unknown);

   subtype Known_Distributions is
     Distributions range Distributions'First ..
                         Distributions'Pred (Distributions'Last);

   type Word_Sizes is (Bits_32,
                       Bits_64,
                       Bits_Unknown);

   type Package_Managers is (Apt,
                             Pacman,
                             Yum,
                             Dnf,
                             Zypper,
                             Homebrew,
                             Packager_Unknown);

   Distro_Manager : constant array (Distributions) of Package_Managers :=
     (Debian | Ubuntu => Apt,
      Msys2 | Arch    => Pacman,
      Rhel            => Yum,
      Centos | Fedora => Dnf,
      Suse            => Zypper,
      Homebrew        => Homebrew,
      Distro_Unknown  => Packager_Unknown);

   type Toolchains is (System,
                       --  Provided through system packages, able to use other
                       --  Ada system packages

                       User
                       --  Provided by the user
                      );

   type Shells is (Unix, PowerShell, WinCmd);

private

   --  Should be in sync with testsuite/drivers/helpers.py#L106

   function Arch_Mapping (Arch : Extended_Architectures) return Architectures
   is (case Arch is
          when AMD64         => X86_64,
          when ARM64         => AARCH64,
          when Architectures => Arch,
          when others        =>
             raise Program_Error
               with "Mapping missing for given architecture: " & Arch'Image);

end Alire.Platforms;
