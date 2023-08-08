
with GNAT.Strings; use GNAT.Strings;

with AAA.Table_IO;

with Alire.Config.Edit;
with Alire.Containers;
with Alire.Dependencies;
with Alire.Errors;
with Alire.Milestones;
with Alire.Origins.Deployers;
with Alire.Releases.Containers;
with Alire.Solver;
with Alire.Toolchains;
with Alire.Utils; use Alire.Utils;
with Alire.Utils.TTY;
with Alire.Warnings;

with Semantic_Versioning.Extended;

package body Alr.Commands.Toolchain is

   package Name_Sets renames Alire.Containers.Crate_Name_Sets;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
        (Config,
         Cmd.Disable'Access,
         Long_Switch => "--disable-assistant",
         Help        => "Disable autorun of selection assistant");

      Define_Switch
        (Config,
         Cmd.Install'Access,
         Switch      => "-i",
         Long_Switch => "--install",
         Help        => "Install one or more toolchain component");

      Define_Switch
        (Config,
         Cmd.Install_Dir'Access,
         Long_Switch => "--install-dir=",
         Help        => "Toolchain component(s) installation directory");

      Define_Switch
        (Config,
         Cmd.Local'Access,
         Switch      => "",
         Long_Switch => "--local",
         Help        => "Store toolchain configuration in local workspace");

      Define_Switch
        (Config,
         Cmd.S_Select'Access,
         Switch      => "",
         Long_Switch => "--select",
         Help        => "Run the toolchain selection assistant");

      Define_Switch
        (Config,
         Cmd.Uninstall'Access,
         Switch      => "-u",
         Long_Switch => "--uninstall",
         Help        => "Uninstall one or more toolchain component");
   end Setup_Switches;

   -------------
   -- Install --
   -------------

   procedure Install (Cmd            : in out Command;
                      Request        : String;
                      Pending        : Name_Sets.Set;
                      Set_As_Default : Boolean)
   is
      use Alire;
      use all type Origins.Kinds;

      Dep : constant Dependencies.Dependency :=
              Dependencies.From_String (Request);

      type Origin_States is (Unset, Frozen, Mixed);
      --  To detect what tool origins are already in use: Unset none, Frozen
      --  one kind, Mixed more than one kind.
      Origin_Status : Origin_States := Unset;
      Origin_Kind   : Origins.Kinds; -- The Frozen kind in use

      ----------------------
      -- Identify_Origins --
      ----------------------

      procedure Identify_Origins is

         ----------------------
         -- Equivalent_Crate --
         ----------------------

         function Equivalent_Crate (L, R : Crate_Name) return Boolean
         is (L = R
             or else
               (AAA.Strings.Has_Prefix (L.As_String, "gnat_")
                and then R = GNAT_Crate)
             or else
               (AAA.Strings.Has_Prefix (R.As_String, "gnat_")
                and then L = GNAT_Crate)
             or else
               (AAA.Strings.Has_Prefix (L.As_String, "gnat_")
                and then AAA.Strings.Has_Prefix (R.As_String, "gnat_")));

      begin
         for Tool of Toolchains.Tools loop

            --  A tool that is already configured, and not pending in the
            --  command-line, will impose an origin compatibility constraint

            if Toolchains.Tool_Is_Configured (Tool)
              and then not
                (for some P of Pending =>
                   Toolchains.Tool_Release (Tool).Provides (P) or else
                   Equivalent_Crate (P, Tool))
              and then not Toolchains.Tool_Release (Tool).Provides (Dep.Crate)
            then
               declare
                  --  The one already selected we want to be compatible with
                  Other_Tool : constant Releases.Release :=
                                 Toolchains.Tool_Release (Tool);
               begin
                  Trace.Debug ("Configured tool " & Utils.TTY.Name (Tool)
                               & " has origin kind "
                               & Other_Tool.Origin.Kind'Image);
                  case Origin_Status is
                     when Unset =>
                        Origin_Status := Frozen;
                        Origin_Kind   := Other_Tool.Origin.Kind;
                     when Frozen =>
                        if Other_Tool.Origin.Kind /= Origin_Kind then
                           Origin_Status := Mixed;
                        end if;
                     when Mixed =>
                        null; -- We are beyond salvation at this point
                  end case;
               end;
            end if;
         end loop;

         if Origin_Status = Mixed then
            Warnings.Warn_Once
              ("Default toolchain contains mixed-origin tools");
         else
            Trace.Debug ("Tool compatibility identified as "
                         & Origin_Status'Image);
         end if;
      end Identify_Origins;

   begin

      --  We want to ensure that we are installing compatible tools. The user
      --  can force through this, so we consider that a bad situation may
      --  already exist. The following call checks what origins are already in
      --  use by configured tools. This is only relevant when setting defaults,
      --  though.

      if Set_As_Default then
         Identify_Origins;
         if Origin_Status = Frozen then
            Put_Info ("Already selected tool imposes on remaining tools to be "
                      & "of origin " & Origin_Kind'Image,
                      Trace.Detail);
         end if;
      end if;

      Installation :
      declare

         -------------------
         -- Origin_Filter --
         -------------------

         function Origin_Filter return Origins.Kinds_Set
         is
            Filter : Origins.Kinds_Set := (others => False);
         begin
            Filter (Origin_Kind) := True;
            return Filter;
         end Origin_Filter;

         Any_Origin : constant Origins.Kinds_Set := (others => True);

         Rel : constant Releases.Release :=
                 Solver.Find (Name    => Dep.Crate,
                              Allowed => Dep.Versions,
                              Policy  => Query_Policy,
                              Origins =>
                                 (if not Force and then Origin_Status = Frozen
                                  then Origin_Filter
                                  else Any_Origin));

         function The_Other (Tool : Crate_Name) return Crate_Name
         is (if Tool = GPRbuild_Crate then GNAT_Crate else GPRbuild_Crate);
         --  This will break the moment we have another tool in the toolchain,
         --  so leave a canary here:
         pragma Assert (Natural (Alire.Toolchains.Tools.Length) = 2);

      begin

         --  Only allow sharing toolchain elements in this command:

         if not (for some Crate of Alire.Toolchains.Tools =>
                   Rel.Provides (Crate))
         then
            Reportaise_Wrong_Arguments
              ("The requested crate is not a toolchain component");
         end if;

         --  Inform of how the requested crate has been narrowed down

         if not AAA.Strings.Has_Prefix (Dep.Versions.Image, "=") then
            Put_Info ("Requested crate resolved as "
                      & Rel.Milestone.TTY_Image);
         end if;

         --  Check for mixed-origin clashes

         if Origin_Status = Frozen and then Rel.Origin.Kind /= Origin_Kind then
            Recoverable_Error
              ("Currently configured " & Utils.TTY.Name (The_Other (Dep.Crate))
               & " has origin " & TTY.Emph (Origin_Kind'Image)
               & " but newly selected " & Utils.TTY.Name (Dep.Crate)
               & " has origin " & TTY.Emph (Rel.Origin.Kind'Image) & ASCII.LF
               & "Mixing tool origins may result in a broken toolchain");
         end if;

         --  And perform the actual installation

         if Cmd.Install_Dir.all /= "" then
            if Rel.Origin.Is_Index_Provided then
               Toolchains.Deploy (Rel, Cmd.Install_Dir.all);
            else
               Reportaise_Command_Failed
                 ("Releases with external origins cannot be installed at "
                  & "specific locations; origin for "
                  & Rel.Milestone.TTY_Image & " is: " & Rel.Origin.Kind'Image);
            end if;
         else
            if Rel.Origin.Is_Index_Provided then
               Toolchains.Deploy (Rel);
            elsif Rel.Origin.Is_System then
               Origins.Deployers.Deploy (Rel).Assert;
            elsif Rel.Origin.Kind = External then
               Put_Info ("External tool needs no installation: "
                         & Rel.Milestone.TTY_Image);
            else
               Raise_Checked_Error ("Unexpected release origin: "
                                    & Rel.Origin.Kind'Image);
            end if;
         end if;

         if Set_As_Default then
            Alire.Toolchains.Set_As_Default
              (Rel,
               Level => (if Cmd.Local
                         then Alire.Config.Local
                         else Alire.Config.Global));
            Alire.Put_Info
              (Rel.Milestone.TTY_Image & " set as default in "
               & TTY.Emph (if Cmd.Local then "local" else "global")
               & " configuration.");
         end if;

      end Installation;

   exception
      when E : Alire.Query_Unsuccessful =>
         Alire.Log_Exception (E);
         if Set_As_Default then
            Trace.Error (Alire.Errors.Get (E));
            Reportaise_Command_Failed
              ("Use --force to override compatibility checks between "
               & "installed toolchain components");
         else
            Reportaise_Command_Failed (Alire.Errors.Get (E));
         end if;
   end Install;

   ----------
   -- List --
   ----------

   procedure List (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
      use Alire;
      use type Dependencies.Dependency;
      Table : AAA.Table_IO.Table;
   begin
      Alire.Toolchains.Detect_Externals;
      --  Even if we have selected a non-external toolchain, in this case we
      --  want to force detection of external toolchains to be aware of them.

      if Alire.Toolchains.Available.Is_Empty then
         Trace.Info ("Nothing installed in configuration prefix "
                     & TTY.URL (Alire.Config.Edit.Path));
         return;
      end if;

      Table
        .Append (TTY.Emph ("CRATE"))
        .Append (TTY.Emph ("VERSION"))
        .Append (TTY.Emph ("STATUS"))
        .Append (TTY.Emph ("NOTES"))
        .New_Row;

      for Dep of Alire.Toolchains.Available loop
         if (for some Crate of Toolchains.Tools =>
               Dep.Provides (Crate))
         then
            declare
               Tool : constant Crate_Name :=
                        (if Dep.Provides (GNAT_Crate)
                         then GNAT_Crate
                         else Dep.Name);
            begin
               Table
                 .Append (Alire.Utils.TTY.Name (Dep.Name))
                 .Append (TTY.Version (Dep.Version.Image))
                 .Append (if Toolchains.Tool_Is_Configured (Tool)
                             and then Dep.To_Dependency.Value =
                                      Toolchains.Tool_Dependency (Tool)
                          then TTY.Description ("Default")
                          else "Available")
                 .Append (TTY.Dim (Dep.Notes))
                 .New_Row;
            end;
         end if;
      end loop;

      Table.Print;
   end List;

   ---------------
   -- Uninstall --
   ---------------

   procedure Uninstall (Cmd : in out Command; Target : String) is

      ------------------
      -- Find_Version --
      ------------------

      function Find_Version return String is
         --  Obtain all installed releases for the crate; we will proceed if
         --  only one exists.
         Available : constant Alire.Releases.Containers.Release_Set :=
                       Alire.Toolchains.Available.Satisfying
                         (Alire.Dependencies.New_Dependency
                            (Crate    => Alire.To_Name (Target),
                             Versions => Semantic_Versioning.Extended.Any));
      begin
         if Available.Is_Empty then
            Reportaise_Command_Failed
              ("Requested crate has no installed releases: "
               & Alire.Utils.TTY.Name (Alire.To_Name (Target)));
         elsif Available.Length not in 1 then
            Reportaise_Command_Failed
              ("Requested crate has several installed releases, "
               & "please provide an exact target version");
         end if;

         return Available.First_Element.Milestone.Version.Image;
      end Find_Version;

   begin

      --  If no version was given, find if only one is installed

      if not AAA.Strings.Contains (Target, "=") then
         Uninstall (Cmd, Target & "=" & Find_Version);
         return;
      end if;

      --  Otherwise we proceed with a complete milestone

      Alire.Toolchains.Remove (Alire.Milestones.New_Milestone (Target));

   end Uninstall;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args : AAA.Strings.Vector)
   is
      Pending : Name_Sets.Set;
      --  We do not want tools that are later in the command-line to be taken
      --  into account prematurely for compatibility of origins. We store here
      --  crates still to be dealt with.

   begin

      --  Validation

      if Alire.Utils.Count_True
        ((Cmd.Install, Cmd.S_Select, Cmd.Uninstall)) > 1
      then
         Reportaise_Wrong_Arguments
           ("The provided switches cannot be used simultaneously");
      end if;

      if (Cmd.Install or Cmd.Uninstall) and then Args.Is_Empty then
         Reportaise_Wrong_Arguments ("No release specified");
      end if;

      if not Args.Is_Empty and then
        not (Cmd.Install or Cmd.Uninstall or Cmd.S_Select)
      then
         Reportaise_Wrong_Arguments
           ("Specify the action to perform with the crate");
      end if;

      if Cmd.Local and then not (Cmd.S_Select or else Cmd.Disable) then
         Reportaise_Wrong_Arguments
           ("--local requires --select or --disable-assistant");
      end if;

      if Cmd.Install_Dir.all /= "" and then not Cmd.Install then
         Reportaise_Wrong_Arguments
           ("--install-dir is only compatible with --install action");
      end if;

      --  Dispatch to subcommands

      if Cmd.Disable then
         Alire.Toolchains.Set_Automatic_Assistant (False,
                                                   (if Cmd.Local
                                                    then Alire.Config.Local
                                                    else Alire.Config.Global));
         Alire.Put_Info
           ("Assistant disabled in "
            & TTY.Emph (if Cmd.Local then "local" else "global")
            & " configuration.");

      end if;

      if Cmd.S_Select then

         Alire.Toolchains.Detect_Externals;

         if Cmd.Local then
            Cmd.Requires_Workspace;
         end if;

         if Args.Count = 0 then
            Alire.Toolchains.Assistant ((if Cmd.Local
                                         then Alire.Config.Local
                                         else Alire.Config.Global),
                                        Allow_Incompatible => Alire.Force);
         else

            for Elt of Args loop
               Pending.Insert (Alire.Dependencies.From_String (Elt).Crate);
            end loop;

            for Elt of Args loop
               Install (Cmd, Elt, Pending, Set_As_Default => True);
               Pending.Exclude (Alire.Dependencies.From_String (Elt).Crate);
            end loop;

            Alire.Toolchains.Set_Automatic_Assistant
              (False,
               (if Cmd.Local
                then Alire.Config.Local
                else Alire.Config.Global));
            Trace.Detail
              ("Assistant disabled in "
               & TTY.Emph (if Cmd.Local then "local" else "global")
               & " configuration because of toolchain selection via "
               & "command line.");
         end if;

      elsif Cmd.Uninstall then
         for Elt of Args loop
            Uninstall (Cmd, Elt);
         end loop;

      elsif Cmd.Install then

         Alire.Toolchains.Detect_Externals;

         for Elt of Args loop
            Install (Cmd, Elt, Name_Sets.Empty_Set, Set_As_Default => False);
         end loop;

      elsif not Cmd.Disable then
         --  When no command is specified, print the list
         Cmd.List;
      end if;

   exception
      when E : Semantic_Versioning.Malformed_Input =>
         Alire.Log_Exception (E);
         Reportaise_Wrong_Arguments ("Improper version specification");
   end Execute;

end Alr.Commands.Toolchain;
