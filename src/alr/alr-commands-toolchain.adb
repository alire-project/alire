with Ada.Containers;
with AAA.Table_IO;

with Alire.Config.Edit;
with Alire.Dependencies;
with Alire.Errors;
with Alire.Milestones;
with Alire.Releases.Containers;
with Alire.Shared;
with Alire.Solver;
with Alire.Toolchains;
with Alire.Utils; use Alire.Utils;

with Semantic_Versioning.Extended;

package body Alr.Commands.Toolchain is

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
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
         Help        => "Install a toolchain component");

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
         Help        => "Uninstall a toolchain component");
   end Setup_Switches;

   -------------
   -- Install --
   -------------

   procedure Install (Cmd            : in out Command;
                      Request        : String;
                      Set_As_Default : Boolean)
   is
      use Alire;
   begin

      Cmd.Requires_Full_Index;

      Installation :
      declare
         Dep : constant Dependencies.Dependency :=
                 Dependencies.From_String (Request);
         Rel : constant Releases.Release :=
                 Solver.Find (Name    => Dep.Crate,
                              Allowed => Dep.Versions,
                              Policy  => Query_Policy);
      begin

         --  Only allow sharing toolchain elements in this command:

         if not (for some Crate of Alire.Toolchains.Tools =>
                   Rel.Provides (Crate))
         then
            Reportaise_Wrong_Arguments
              ("The requested crate is not a toolchain component");
         end if;

         --  Inform of how the requested crate has been narrowed down

         if not Alire.Utils.Starts_With (Dep.Versions.Image, "=") then
            Put_Info ("Requested crate resolved as "
                      & Rel.Milestone.TTY_Image);
         end if;

         --  And perform the actual installation

         Shared.Share (Rel);

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
         Trace.Error (Alire.Errors.Get (E));
   end Install;

   ----------
   -- List --
   ----------

   procedure List (Cmd : in out Command) is
      use Alire;
      use type Dependencies.Dependency;
      Table : AAA.Table_IO.Table;
   begin
      Cmd.Requires_Full_Index;

      if Alire.Shared.Available.Is_Empty then
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

      for Dep of Alire.Shared.Available loop
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
                 .Append (TTY.Name (Dep.Name))
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
                       Alire.Shared.Available.Satisfying
                         (Alire.Dependencies.New_Dependency
                            (Crate    => Alire.To_Name (Target),
                             Versions => Semantic_Versioning.Extended.Any));
      begin
         if Available.Is_Empty then
            Reportaise_Command_Failed
              ("Requested crate has no installed releases: "
               & TTY.Name (Target));
         elsif Available.Length not in 1 then
            Reportaise_Command_Failed
              ("Requested crate has several installed releases, "
               & "please provide an exact target version");
         end if;

         return Available.First_Element.Milestone.Version.Image;
      end Find_Version;

   begin
      Cmd.Requires_Full_Index;

      --  If no version was given, find if only one is installed

      if not Contains (Target, "=") then
         Uninstall (Cmd, Target & "=" & Find_Version);
         return;
      end if;

      --  Otherwise we proceed with a complete milestone

      Alire.Shared.Remove (Alire.Milestones.New_Milestone (Target));

   end Uninstall;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args : AAA.Strings.Vector)
   is
      use Ada.Containers;
   begin

      --  Validation

      if Alire.Utils.Count_True
        ((Cmd.Disable, Cmd.Install, Cmd.S_Select, Cmd.Uninstall)) > 1
      then
         Reportaise_Wrong_Arguments
           ("The provided switches cannot be used simultaneously");
      end if;

      if Args.Length > 1 then
         Reportaise_Wrong_Arguments
           ("One crate with optional version expected: crate[version set]");
      end if;

      if (Cmd.Install or Cmd.Uninstall) and then Args.Length /= 1 then
         Reportaise_Wrong_Arguments ("No release specified");
      end if;

      if Args.Count = 1 and then
        not (Cmd.Install or Cmd.Uninstall or Cmd.S_Select)
      then
         Reportaise_Wrong_Arguments
           ("Specify the action to perform with the crate");
      end if;

      if Cmd.S_Select and then Args.Count > 1 then
         Reportaise_Wrong_Arguments
           ("Toolchain installation accepts at most one argument");
      end if;

      if Cmd.Local and then not (Cmd.S_Select or else Cmd.Disable) then
         Reportaise_Wrong_Arguments
           ("--local requires --select or --disable-assistant");
      end if;

      if Cmd.Disable and then Args.Length /= 0 then
         Reportaise_Wrong_Arguments
           ("Disabling the assistant does not admit any extra arguments");
      end if;

      --  Dispatch to subcommands

      if Cmd.S_Select then

         Cmd.Requires_Full_Index;

         if Cmd.Local then
            Cmd.Requires_Valid_Session;
         end if;

         if Num_Arguments = 0 then
            Alire.Toolchains.Assistant (if Cmd.Local
                                        then Alire.Config.Local
                                        else Alire.Config.Global);
         else
            Install (Cmd, Argument (1), Set_As_Default => True);
         end if;

      elsif Cmd.Uninstall then
         Uninstall (Cmd, Args.First_Element);

      elsif Cmd.Install then
         Install (Cmd, Args (1), Set_As_Default => False);

      elsif Cmd.Disable then
         Alire.Toolchains.Set_Automatic_Assistant (False,
                                                   (if Cmd.Local
                                                    then Alire.Config.Local
                                                    else Alire.Config.Global));
         Alire.Put_Info
           ("Assistant disabled in "
            & TTY.Emph (if Cmd.Local then "local" else "global")
            & " configuration.");

      else
         Cmd.List;

      end if;

   exception
      when E : Semantic_Versioning.Malformed_Input =>
         Alire.Log_Exception (E);
         Reportaise_Wrong_Arguments ("Improper version specification");
   end Execute;

end Alr.Commands.Toolchain;
