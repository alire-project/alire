with Alire.Releases;
with Alire.Solutions.Diffs;
with Alire.Pinning;
with Alire.Utils.TTY;
with Alire.Utils.User_Input;

with Alr.Commands.User_Input;
with Alr.Platform;

with Semantic_Versioning;

package body Alr.Commands.Pin is

   package Semver renames Semantic_Versioning;
   package TTY renames Alire.Utils.TTY;

   --------------------
   -- Change_One_Pin --
   --------------------

   procedure Change_One_Pin (Cmd      : in out Command;
                             Solution : in out Alire.Solutions.Solution;
                             Target   :        String)
   is
      Version : Semver.Version;
      Name    : constant Alire.Crate_Name := +Utils.Head (Target, '=');

      ---------
      -- Pin --
      ---------

      procedure Pin is
      begin

         --  We let to re-pin without checks because the requested version may
         --  be different.

         Cmd.Requires_Full_Index;

         Solution := Alire.Pinning.Pin
           (Crate        => Name,
            Version      => Version,
            Dependencies => Cmd.Root.Release.Dependencies,
            Environment  => Platform.Properties,
            Solution     => Solution);

      end Pin;

      -----------
      -- Unpin --
      -----------

      procedure Unpin is
      begin
         if not (Solution.State (Name).Is_Linked or else
                 Solution.State (Name).Is_Pinned)
         then
            Reportaise_Command_Failed ("Requested crate is already unpinned");
         end if;

         Cmd.Requires_Full_Index;

         Solution := Alire.Pinning.Unpin
           (Crate        => Name,
            Dependencies => Cmd.Root.Release.Dependencies,
            Environment  => Platform.Properties,
            Solution     => Solution);
      end Unpin;

   begin

      --  Sanity checks

      if not Solution.Depends_On (Name) then
         Reportaise_Command_Failed ("Cannot pin dependency not in solution: "
                                    & (+Name));
      end if;

      --  Check if we are given a particular version

      if Utils.Contains (Target, "=") then

         if Cmd.Unpin then
            Reportaise_Wrong_Arguments ("Unpinning does not require version");
         end if;

         Version := Semver.Parse (Utils.Tail (Target, '='),
                                  Relaxed => False);

         Trace.Debug ("Pin requested for exact version: " & Version.Image);
      elsif Solution.State (Name).Is_Solved then
         Version := Solution.State (Name).Release.Version;
      elsif not Cmd.Unpin then
         Trace.Warning ("An explicit version is required to pin a crate with"
                        & " no release in the current solution: "
                        & TTY.Name (Name));
         return;
      end if;

      --  Proceed to pin/unpin

      if Cmd.Unpin then
         Unpin;
      else
         Pin;
      end if;
   end Change_One_Pin;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command)
   is

      -------------
      -- Confirm --
      -------------

      procedure Confirm (Old_Sol, New_Sol : Alire.Solutions.Solution) is
         Diff : constant Alire.Solutions.Diffs.Diff :=
                  Old_Sol.Changes (New_Sol);
      begin
         if Diff.Contains_Changes then
            if Alire.Utils.User_Input.Confirm_Solution_Changes (Diff) then
               Cmd.Root.Set (Solution => New_Sol);
               Cmd.Root.Deploy_Dependencies;
            end if;
         else
            Trace.Info ("No changes to apply.");
         end if;
      end Confirm;

   begin

      --  Argument validation

      if Cmd.Pin_All and then Num_Arguments /= 0 then
         Reportaise_Wrong_Arguments ("--all must appear alone");
      elsif Cmd.URL.all /= "" and then
        (Num_Arguments /= 1 or else Cmd.Pin_All or else Cmd.Unpin)
      then
         Reportaise_Wrong_Arguments
           ("--use must be used alone with a crate name");
      end if;

      Cmd.Requires_Valid_Session;

      --  Listing of pins

      if not Cmd.Pin_All and then Num_Arguments = 0 then
         Cmd.Root.Solution.Print_Pins;
         return;
      elsif Num_Arguments > 1 then
         Reportaise_Wrong_Arguments
           ("Pin expects a single crate or crate=version argument");
      end if;

      --  Apply changes;

      declare
         New_Sol : Alire.Solutions.Solution := Cmd.Root.Solution;
         Old_Sol : constant Alire.Solutions.Solution := New_Sol;
      begin

         if Cmd.Pin_All then

            --  Change all pins

            for Crate of New_Sol.Crates loop
               if New_Sol.State (Crate).Is_Pinned = Cmd.Unpin then
                  Change_One_Pin (Cmd, New_Sol, +Crate);
               end if;
            end loop;

         elsif Cmd.URL.all /= "" then

            --  Pin to dir

            Cmd.Requires_Full_Index; -- Next statement recomputes a solution

            New_Sol := Alire.Pinning.Pin_To
              (+Argument (1),
               Cmd.URL.all,
               Cmd.Root.Release.Dependencies,
               Platform.Properties,
               Old_Sol);

            --  Report crate detection at target destination

            User_Input.Report_Pinned_Crate_Detection (+Argument (1),
                                                      New_Sol);

         else

            --  Change a single pin

            Change_One_Pin (Cmd, New_Sol, Argument (1));
         end if;

         --  Consolidate changes

         Confirm (Old_Sol, New_Sol);
      end;

   exception
      when Semver.Malformed_Input =>
         Reportaise_Wrong_Arguments ("Improper version string");
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Pin releases to a particular version."
               & " By default, the current solution version is used."
               & " A pinned release is not affected by automatic updates.")
      .New_Line
      .Append ("Without arguments, show existing pins.")
      .New_Line
      .Append ("Use --all to pin the whole current solution.")
      .New_Line
      .Append ("Specify a single crate to modify its pin.")
      .New_Line
      .Append ("Use the --use <PATH> switch to "
               & " force alr to use the PATH target"
               & " to fulfill a dependency locally"
               & " instead of looking for indexed releases.")
     );

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
      Define_Switch (Config,
                     Cmd.Pin_All'Access,
                     Long_Switch => "--all",
                     Help        => "Pin the complete solution");

      Define_Switch (Config,
                     Cmd.Unpin'Access,
                     Long_Switch => "--unpin",
                     Help        => "Unpin a release");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.URL'Access,
         Long_Switch => "--use=",
         Argument    => "PATH",
         Help        => "Use a directory to fulfill a dependency");
   end Setup_Switches;

end Alr.Commands.Pin;
