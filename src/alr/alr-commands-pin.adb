with Alire.Conditional;
with Alire.Dependencies;
with Alire.Lockfiles;
with Alire.Releases;
with Alire.Solutions.Diffs;
with Alire.Solver;

with Alr.Commands.Update;
with Alr.Commands.User_Input;
with Alr.Platform;
with Alr.Root;

with Semantic_Versioning;

package body Alr.Commands.Pin is

   package Semver renames Semantic_Versioning;

   --------------------
   -- Change_One_Pin --
   --------------------

   procedure Change_One_Pin (Cmd      :        Command;
                             Solution : in out Alire.Solutions.Solution;
                             Target   :        String)
   is
      Version : Semver.Version;
      Name    : constant Alire.Crate_Name := +Utils.Head (Target, '=');

      ---------
      -- Pin --
      ---------

      procedure Pin is
         use type Alire.Conditional.Dependencies;
      begin

         --  We let to re-pin because the requested version may be different.
         --  Since two simultaneously pinned versions will always result in no
         --  solution, and the clear intent is to update the pinned version,
         --  we remove the existing pin for the solver call.

         --  Verify that the requested version is solvable (it might not be if
         --  the pin version does not exist or allows a complete solution).

         Requires_Full_Index;

         declare
            use Alire;
            New_Solution : constant Solutions.Solution :=
                             Solver.Resolve
                               (Conditional.New_Dependency (Name, Version) and
                                  Root.Current.Release.Dependencies,
                                Platform.Properties,
                                Solution.Changing_Pin (Name, Pinned => False));
         begin
            if New_Solution.Valid then
               Solution := New_Solution.Changing_Pin (Name, Pinned => True);
            else
               Reportaise_Command_Failed
                 ("Cannot find a solution with the requested pin version");
            end if;
         end;
      end Pin;

      -----------
      -- Unpin --
      -----------

      procedure Unpin is
      begin
         if not Solution.Releases.Element (Name).Is_Pinned then
            Reportaise_Command_Failed ("Requested crate is already unpinned");
         end if;

         Solution := Solution.Changing_Pin (Name, Pinned => False);
      end Unpin;

   begin

      --  Sanity checks

      if not Solution.Releases.Contains (Name) then
         Reportaise_Command_Failed ("Cannot pin release not in solution: "
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
      else
         Version := Solution.Releases.Element (Name).Version;
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
            if Commands.User_Input.Confirm_Solution_Changes
              (Diff, Changed_Only => not Alire.Detailed)
            then
               Alire.Lockfiles.Write (Solution    => New_Sol,
                                      Environment => Platform.Properties,
                                      Filename    => Root.Current.Lock_File);

               --  We force the update because we have just stored the new
               --  solution, so Update won't detect any changes.

               Update.Execute (Interactive => False,
                               Force       => True);
            end if;
         else
            Trace.Info ("No changes to apply.");
         end if;
      end Confirm;

   begin

      --  Argument validation

      if Cmd.Pin_All and then Num_Arguments /= 0 then
         Reportaise_Wrong_Arguments ("--all must appear alone");
      end if;

      Requires_Valid_Session;

      --  Listing of pins

      if not Cmd.Pin_All and then Num_Arguments = 0 then
         Root.Current.Solution.Print_Pins;
         return;
      elsif Num_Arguments > 1 then
         Reportaise_Wrong_Arguments ("Pin expects a single crate name");
      end if;

      --  Apply changes;

      declare
         New_Sol : Alire.Solutions.Solution := Root.Current.Solution;
         Old_Sol : constant Alire.Solutions.Solution := New_Sol;
      begin

         if Cmd.Pin_All then

            if not New_Sol.Valid then
               Reportaise_Command_Failed ("Cannot pin an invalid solution");
            end if;

            for Release of New_Sol.Releases loop
               if Release.Is_Pinned = Cmd.Unpin then
                  Change_One_Pin (Cmd, New_Sol, Release.Name_Str);
               end if;
            end loop;

         else
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
      .Append ("Pin releases to their current solution version."
               & " A pinned release is not affected by automatic updates.")
      .New_Line
      .Append ("Without arguments, show existing pins.")
      .New_Line
      .Append ("Use --all to pin the whole current solution.")
      .New_Line
      .Append ("Specify a single crate to modify its pin.")
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
   end Setup_Switches;

end Alr.Commands.Pin;
