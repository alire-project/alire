with Ada.Containers.Indefinite_Ordered_Sets;

with AAA.Strings;

with Alire.Config;
with Alire.Dependencies;
with Alire.Errors;
with Alire.Milestones;
with Alire.Releases.Containers;
with Alire.Utils;
with Alire.Utils.TTY;

with CLIC.Config;
with CLIC.User_Input;

package Alire.Toolchains is

   package Name_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Crate_Name);

   Tools : constant Name_Sets.Set :=
             Name_Sets.Empty_Set
               .Union (Name_Sets.To_Set (GNAT_Crate))
               .Union (Name_Sets.To_Set (GPRbuild_Crate));
   --  All crates that are part of the provided binary toolchain

   function Is_Tool (Release : Releases.Release) return Boolean
   is (for some Tool of Tools => Release.Provides (Tool));

   function Any_Tool (Crate : Crate_Name) return Dependencies.Dependency;
   --  Returns a dependency on crate*

   procedure Assistant (Level              : Config.Level;
                        Allow_Incompatible : Boolean := False;
                        First_Run          : Boolean := False);
   --  Runs the interactive assistant to select the default toolchain. By
   --  default, the native Alire-provided compiler for Current_OS is proposed.
   --  This information may apply config-wide or workspace-wide. Installation
   --  goes, in any case, to the config cache location. If First_Run, select
   --  defaults without interacting with the user.

   --  The following functions will transform any `gnat_XXX` dependency on
   --  plain `gnat`. This way we need not to litter the callers with similar
   --  transformations, as we always want whatever gnat_XXX is used for "gnat".

   procedure Set_Automatic_Assistant (Enabled : Boolean; Level : Config.Level);
   --  Enable/Disable the automatic assistant on next run

   function Assistant_Enabled return Boolean;

   procedure Set_As_Default (Release : Releases.Release; Level : Config.Level);
   --  Mark the given release as the default to be used. Does not check that it
   --  be already installed.

   function Tool_Is_Configured (Crate : Crate_Name) return Boolean;
   --  Say if a tool is actually configured by the user

   function Tool_Dependency (Crate : Crate_Name) return Dependencies.Dependency
     with Pre => Tool_Is_Configured (Crate);
   --  Return the configured compiler as an exact compiler=version dependency

   type Info_Kinds is (For_Use, For_Is_External);

   function Tool_Key (Crate : Crate_Name;
                      Kind  : Info_Kinds := For_Use)
                      return CLIC.Config.Config_Key;
   --  Return the config key corresponding to a tool, for which milestone is in
   --  use or whether it is external.

   function Tool_Is_External (Crate : Crate_Name) return Boolean;
   --  Use the stored config to check if the tool is external without having to
   --  detect it. Defaults to True if unset or tool is not configured.

   function Tool_Milestone (Crate : Crate_Name) return Milestones.Milestone;

   function Tool_Release (Crate : Crate_Name) return Releases.Release;
   --  Will raise Checked_Error for unconfigured, or configured but without the
   --  release being deployed (e.g. the user messed with files and deleted it
   --  manually).

   procedure Unconfigure (Crate         : Crate_Name;
                          Level         : Config.Level;
                          Fail_If_Unset : Boolean := True);
   --  Set the crate as not configured. If not set and Fail_If_Unset, raise

   procedure Detect_Externals;
   --  Detect all tools that may have external definitions, so they're
   --  available for selection/installation.

   Description : constant AAA.Strings.Vector
     := AAA.Strings.Empty_Vector
       .Append ("Alire indexes binary releases of GNAT and gprbuild. The "
                & "compilers are indexed with their target name, e.g., "
                & Utils.TTY.Name ("gnat_native") & " or "
                & Utils.TTY.Name ("gnat_riscv_elf") & ". ")
     .Append ("")
     .Append ("Use " & TTY.Terminal ("alr toolchain --help") & " to obtain "
              & "information about toolchain management. Alire can be "
              & "configured to rely on a toolchain installed by the user in "
              & "the environment, or to use one of the indexed toolchains "
              & "whenever possible.")
     .Append ("")
     .Append ("Some crates may override the default toolchain by specifying "
              & "dependencies on particular compiler crates, for example to "
              & "use a cross-compiler. In this situation, a compiler already "
              & "available (selected as default or already installed) will "
              & "take precedence over a compiler available in the index. ")
     .Append ("")
     .Append ("See also "
              & TTY.URL ("https://alire.ada.dev/docs/#toolchains") & " for "
              & "additional details about compiler dependencies and toolchain "
              & "interactions.");

   --  From here on, these are former Alire.Shared subprograms, so they were
   --  more generally oriented.

   function Available (Detect_Externals : Boolean := True)
                       return Releases.Containers.Release_Set;
   --  Returns tools installed at the toolchain location

   function Release (Target : Milestones.Milestone;
                     Detect_Externals : Boolean := True)
                     return Releases.Release;
   --  Retrieve the release corresponding to Target, if it exists. Will raise
   --  Constraint_Error if not among Available.

   function Path return Any_Path;
   --  Returns the base folder in which all shared releases live, defaults to
   --  <cache>/toolchains

   procedure Deploy (Release  : Releases.Release;
                     Location : Any_Path := Path);
   --  Deploy a release in the specified location

   procedure Remove
     (Release : Releases.Release;
      Confirm : Boolean := not CLIC.User_Input.Not_Interactive)
     with Pre => Available.Contains (Release)
     or else raise Checked_Error with
       Errors.Set ("Requested release is not installed: "
                   & Release.Milestone.TTY_Image);
   --  Remove a release from the shared location for the configuration

   procedure Remove
     (Target : Milestones.Milestone;
      Confirm : Boolean := not CLIC.User_Input.Not_Interactive);
   --  Behaves as the previous Remove

private

   -----------------------
   -- Assistant_Enabled --
   -----------------------

   function Assistant_Enabled return Boolean
   is (Config.DB.Get (Config.Keys.Toolchain_Assistant, Default => True));

   ----------------------
   -- Tool_Is_External --
   ----------------------

   function Tool_Is_External (Crate : Crate_Name) return Boolean
   is (Boolean'Value
       (Config.DB.Get_As_String -- because it could be stored as bool or string
          (Tool_Key (Crate, For_Is_External), "True")));

   --------------
   -- Tool_Key --
   --------------
   --  Construct the "toolchain.use.crate" keys
   function Tool_Key (Crate : Crate_Name;
             Kind  : Info_Kinds := For_Use)
             return CLIC.Config.Config_Key
   is (if AAA.Strings.Has_Prefix (Crate.As_String, "gnat_")
       then Tool_Key (GNAT_Crate, Kind)
       else CLIC.Config.Config_Key
         ((case Kind is
             when For_Use => Config.Keys.Toolchain_Use,
             when For_Is_External => Config.Keys.Toolchain_External)
          & "." & Crate.As_String));

   --------------------
   -- Tool_Milestone --
   --------------------
   --  Return the milestone stored by the user for this tool
   function Tool_Milestone (Crate : Crate_Name) return Milestones.Milestone
   is (Milestones.New_Milestone (Config.DB.Get (Tool_Key (Crate), "")));

end Alire.Toolchains;
