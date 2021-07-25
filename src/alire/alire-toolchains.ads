with Ada.Containers.Indefinite_Ordered_Sets;

private with Alire.Config;
with Alire.Dependencies;
private with Alire.Milestones;
with Alire.Platforms;
with Alire.TTY;
with Alire.Utils;

package Alire.Toolchains is

   GNAT_Crate     : constant Crate_Name := To_Name ("gnat");
   GPRbuild_Crate : constant Crate_Name := To_Name ("gprbuild");

   GNAT_External_Crate : constant Crate_Name := To_Name ("gnat_external");

   package Name_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Crate_Name);

   Tools : constant Name_Sets.Set :=
             Name_Sets.Empty_Set
               .Union (Name_Sets.To_Set (GNAT_Crate))
               .Union (Name_Sets.To_Set (GPRbuild_Crate));
   --  All crates that are part of the provided binary toolchain

   --  The following functions will transform any `gnat_XXX` dependency on
   --  plain `gnat`.

   function Any_Tool (Crate : Crate_Name) return Dependencies.Dependency;
   --  Returns a dependency on crate*

   procedure Assistant (Current_OS : Platforms.Operating_Systems);
   --  Runs the interactive assistant to select the default toolchain. By
   --  default, the native Alire-provided compiler for Current_OS is proposed.

   function Tool_Is_Configured (Crate : Crate_Name) return Boolean;
   --  Say if a tool is actually configured by the user

   function Tool_Dependency (Crate : Crate_Name) return Dependencies.Dependency
     with Pre => Tool_Is_Configured (Crate);
   --  Return the configured compiler as an exact compiler=version dependency

   procedure Unconfigure (Crate : Crate_Name);
   --  Set the crate as not configured.

   Description : constant Utils.String_Vector
     := Utils.Empty_Vector
       .Append ("Alire indexes binary releases of GNAT and gprbuild. The "
                & "compilers are indexed with their target name, e.g., "
                & TTY.Name ("gnat_native") & " or "
                & TTY.Name ("gnat_riscv_elf") & ". ")
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
              & "take precedence over a compiler available in the catalog. ")
     .Append ("")
     .Append ("See also "
              & TTY.URL ("https://alire.ada.dev/docs/#toolchains") & " for "
              & "additional details about compiler dependencies and toolchain "
              & "interactions.");

private

   --------------
   -- Tool_Key --
   --------------
   --  Construct the "toolchain.use.crate" keys
   function Tool_Key (Crate : Crate_Name) return Config.Config_Key
   is (if Utils.Starts_With (Crate.As_String, "gnat_")
       then Tool_Key (GNAT_Crate)
       else Config.Config_Key
              (String (Config.Keys.Toolchain_Use) & "." & Crate.As_String));

   --------------------
   -- Tool_Milestone --
   --------------------
   --  Return the milestone stored by the user for this tool
   function Tool_Milestone (Crate : Crate_Name) return Milestones.Milestone
   is (Milestones.New_Milestone (Config.Get (Tool_Key (Crate), "")));

end Alire.Toolchains;
