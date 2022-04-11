with Alire.Dependencies.States;
with Alire.Properties.Actions;
with Alire.Releases;
with Alire.Solutions;

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
      is ((if Moment /= Moments'First then "|" else "")
          & AAA.Strings.To_Lower_Case (Moment'Image)
          & (if Moment = Moments'Pred (On_Demand)
            then ""
            else Build (Moments'Succ (Moment))));

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

   procedure Run (Cmd : in out Command; Arg : String) is null;

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
