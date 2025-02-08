with AAA.Enum_Tools;

with Alire.Dependencies.States;
with Alire.Directories;
with Alire.Properties.Actions.Executor;
with Alire.Releases;
with Alire.Solutions;
with Alire.TOML_Adapters;

package body Alr.Commands.Action is

   -------------------------
   -- Build_Custom_String --
   -------------------------

   function Build_Custom_String return String is

      use Alire.Properties.Actions;

      -----------
      -- Build --
      -----------

      function Build (Moment : Alire.Properties.Actions.Moments) return String
      is
         use AAA.Strings;
      begin
         return (if Moment /= Moments'First then "|" else "")
                & To_Lower_Case (Replace (Moment'Image, "_", "-"))
                & (if Moment = Moments'Pred (On_Demand)
                  then ""
                  else Build (Moments'Succ (Moment)));
      end Build;

   begin
      return Build (Alire.Properties.Actions.Moments'First);
   end Build_Custom_String;

   ----------
   -- List --
   ----------

   procedure List (Cmd : in out Command) is

      Some_Output : Boolean := False;

      --------------
      -- List_One --
      --------------

      procedure List_One
        (This     : in out Alire.Roots.Root;
         Solution : Alire.Solutions.Solution;
         State    : Alire.Dependencies.States.State)
      is
         pragma Unreferenced (Solution, This);

         ------------------
         -- List_Release --
         ------------------

         procedure List_Release (Rel : Alire.Releases.Release) is
         begin
            if not Rel.On_Platform_Actions (Cmd.Root.Environment).Is_Empty then
               Some_Output := True;
               Put_Line (Rel.Milestone.TTY_Image & ":");

               for Action of Rel.On_Platform_Actions (Cmd.Root.Environment)
               loop
                  Put_Line ("   " & Action.Image);
               end loop;

            end if;
         end List_Release;

      begin
         if Cmd.Recursive or else State.Crate = Cmd.Root.Release.Name then
            if State.Has_Release then
               List_Release (State.Release);
            end if;
         end if;
      end List_One;

   begin
      Cmd.Root.Traverse (Doing => List_One'Access);
      if not Some_Output then
         Put_Line ("No actions.");
      end if;
   end List;

   ---------
   -- Run --
   ---------

   procedure Run (Cmd : in out Command; Arg : String) is

      use Alire.Properties.Actions;

      Moment : Moments;

      Some_Output : Boolean := False;

      -------------
      -- Run_One --
      -------------

      procedure Run_One
        (This     : in out Alire.Roots.Root;
         Solution : Alire.Solutions.Solution;
         State    : Alire.Dependencies.States.State)
      is
         pragma Unreferenced (Solution, This);

         -----------------
         -- Run_Release --
         -----------------

         procedure Run_Release (Rel : Alire.Releases.Release) is
            Selected_Moment : Alire.Releases.Moment_Array := (others => False);
            use Alire.Directories;
         begin
            Selected_Moment (Moment) := True;

            if not Rel.On_Platform_Actions (Cmd.Root.Environment,
                                            Selected_Moment).Is_Empty
            then
               Some_Output := True;
               declare
                  use all type Alire.Roots.Usages;
                  CWD : Guard (Enter (Cmd.Root.Release_Base (Rel.Name,
                                                             For_Build)))
                    with Unreferenced;
               begin
                  Alire.Properties.Actions.Executor.Execute_Actions
                    (Rel,
                     Env     => Cmd.Root.Environment,
                     Moment  => Moment);
               exception
                  when Alire.Properties.Actions.Action_Failed =>
                     Reportaise_Command_Failed
                       ("An action exited with error");
               end;
            end if;
         end Run_Release;

      begin
         if Cmd.Recursive or else State.Crate = Cmd.Root.Release.Name then
            if State.Has_Release then
               Run_Release (State.Release);
            end if;
         end if;
      end Run_One;

      subtype Valid_Moments is
        Moments range Moments'First .. Moments'Pred (On_Demand);

      function Is_Valid is new AAA.Enum_Tools.Is_Valid (Valid_Moments);

   begin

      if Is_Valid (Alire.TOML_Adapters.Adafy (Arg)) then
         Moment := Moments'Value (Alire.TOML_Adapters.Adafy (Arg));
      else
         Reportaise_Wrong_Arguments ("Invalid action: " & Arg);
      end if;

      --  Ensure that all directories are ready
      Cmd.Root.Build_Prepare (Saved_Profiles => False,
                              Force_Regen    => False);

      Cmd.Root.Traverse (Doing => Run_One'Access);
      if not Some_Output then
         Put_Line ("No actions to run.");
      end if;
   end Run;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector) is
   begin
      if Args.Is_Empty then
         Cmd.List;
      else
         for Arg of Args loop
            Cmd.Run (Arg);
         end loop;
      end if;
   end Execute;

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
        (Config      => Config,
         Output      => Cmd.Recursive'Access,
         Switch      => "-r",
         Long_Switch => "--recursive",
         Help        => "List or trigger actions also in dependencies");
   end Setup_Switches;

end Alr.Commands.Action;
