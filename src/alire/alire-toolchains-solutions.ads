with Alire.Dependencies.Containers;
with Alire.Releases;
with Alire.Solutions;

with Semantic_Versioning;

package Alire.Toolchains.Solutions is

   --  Needed to break circularity

   function Add_Toolchain (Solution : Alire.Solutions.Solution;
                           Deploy   : Boolean := True)
                           return Alire.Solutions.Solution;
   --  If no release in the solution is a compiler/builder, add the configured
   --  ones (if defined) to the solution. This is used just before launching
   --  the build, so the configured tools are used despite not being in a
   --  regular solution. If Deploy, the toolchain will be readied if not
   --  already available.

   function Compiler (Solution : Alire.Solutions.Solution)
                      return Releases.Release;
   --  Retrieve the compiler that will be used by this solution, be it because
   --  it contains already one, or else checking the selected toolchain.

   function Is_In_Toolchain (Release : Releases.Release) return Boolean;
   --  Say if this Release is part of the user-configured toolchain

   type Tool_Sources is
     (Dependencies, -- Required by the solution, overrides `alr toolchain`
      Global,       -- Selected with `alr toolchain` in global settings
      Local,        -- Selected with `alr toolchain --local`
      Environment,  -- Unselected, detected as external in the environment
      Missing);     -- No release available, the build will likely fail

   type Effective_Tool (<>) is tagged private;
   --  Stores the info about the actual tool in the solution that will be
   --  be used for the build.

   function Source (This : Effective_Tool) return Tool_Sources;

   function Crate (This : Effective_Tool) return Crate_Name;
   --  Release in use (e.g. gnat_native), or the tool itself when Missing

   function Version (This : Effective_Tool) return Semantic_Versioning.Version
     with Pre => Source (This) /= Missing;

   function Impacting (This : Effective_Tool)
                       return Alire.Dependencies.Containers.List;
   --  Final dependencies in the solution fulfilled by the tool, if any

   function Effective (Solution : Alire.Solutions.Solution;
                       Tool     : Crate_Name)
                       return Effective_Tool
     with Pre => Tools.Contains (Tool);
   --  The Tool release a build with Solution would use, without deploying it

private

   type Effective_Tool (Source   : Tool_Sources;
                        Name_Len : Positive)
   is tagged record
      Crate      : Crate_Name (Name_Len);
      Impacting  : Alire.Dependencies.Containers.List;
      case Source is
         when Missing =>
            null;
         when others =>
            Version : Semantic_Versioning.Version;
      end case;
   end record;

   ------------
   -- Source --
   ------------

   function Source (This : Effective_Tool) return Tool_Sources
   is (This.Source);

   -----------
   -- Crate --
   -----------

   function Crate (This : Effective_Tool) return Crate_Name
   is (This.Crate);

   -------------
   -- Version --
   -------------

   function Version (This : Effective_Tool) return Semantic_Versioning.Version
   is (This.Version);

   ---------------
   -- Impacting --
   ---------------

   function Impacting (This : Effective_Tool)
                       return Alire.Dependencies.Containers.List
   is (This.Impacting);

end Alire.Toolchains.Solutions;
