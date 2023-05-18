with Alire.Crates.Containers;
with Alire.Externals;
with Alire.Index.Search;
with Alire.Platforms.Current;
with Alire.Releases.Containers;
with Alire.Solutions;
with Alire.Solver;
with Alire.Utils;
with Alire.Utils.Tables;
with Alire.Utils.TTY;

with Semantic_Versioning;

package body Alr.Commands.Search is

   package Platform renames Alire.Platforms.Current;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is

      Found : Natural := 0;
      Tab   : Alire.Utils.Tables.Table;

      Flag_System   : constant String := TTY.OK ("S");
      Flag_Unav     : constant String := TTY.Error ("U");
      Flag_Unsolv   : constant String := TTY.Error ("X");
      Flag_External : constant String := TTY.Warn ("E");

      ------------------
      -- List_Release --
      ------------------

      procedure List_Release (R : Alire.Releases.Release) is
         package Solver renames Alire.Solver;
      begin
         Trace.Debug ("Listing release: " & R.Milestone.TTY_Image);
         if (Cmd.Prop.all = ""
             or else
             R.Property_Contains (Cmd.Prop.all)
             or else
             AAA.Strings.Contains (R.Notes, Cmd.Prop.all)
             or else
             AAA.Strings.Contains (R.Description,
                             Cmd.Prop.all))
           and then
             (Cmd.External or else not R.Origin.Is_System)
         then
            Found := Found + 1;
            Tab.New_Row;
            Tab.Append (Alire.Utils.TTY.Name (+R.Name));
            Tab.Append
              ((if R.Origin.Is_System then Flag_System else " ") &
               (if R.Is_Available (Platform.Properties)
                  then " " else Flag_Unav) &
               (if R.Origin.Is_System then " " else
                      (if Solver.Is_Resolvable
                         (R.Dependencies (Platform.Properties),
                          Platform.Properties,
                          Alire.Solutions.Empty_Valid_Solution,
                          Options => (Age        => Query_Policy,
                                      On_Timeout => Solver.Stop,
                                      others     => <>))
                       then " "
                       else Flag_Unsolv)));
            Tab.Append (TTY.Version (Semantic_Versioning.Image (R.Version)));
            Tab.Append (TTY.Description (R.Description));
            Tab.Append (R.Notes);
         end if;
      end List_Release;

      ---------------------
      -- List_Undetected --
      ---------------------

      procedure List_Undetected (Name : Alire.Crate_Name;
                                 Ext  : Alire.Externals.External'Class) is
      begin
         Found := Found + 1;
         Tab.New_Row;
         Tab.Append (+Name);
         Tab.Append (Flag_External &
                     (if Cmd.Detect then Flag_Unav else " ") &
                       " ");
         Tab.Append ("external");
         Tab.Append (Alire.Index.Crate (Name).TTY_Description);
         Tab.Append (Ext.Image);
      end List_Undetected;

   begin

      --  First, simpler case of search into crates

      if Cmd.Crates then

         --  Search into crates

         if Alire.Utils.Count_True
           ((Cmd.Detect, Cmd.External, Cmd.Full, Cmd.Prop.all /= "")) > 0
         then
            Reportaise_Wrong_Arguments
              ("Extra switches are incompatible with --crates");
         end if;

         if Cmd.List and then Args.Count /= 0 then
            Reportaise_Wrong_Arguments
              ("Search substring and --list are incompatible");
         end if;

         Alire.Index.Search.Print_Crates
           (Substring => (case Args.Count is
                             when 0      => "",
                             when 1      => Args (1),
                             when others =>
                                raise Wrong_Command_Arguments with
                                  "Only one search substring supported"));
         return;
      end if;

      --  Remaining processing is for releases

      if Cmd.Detect then
         Cmd.External := True;
      end if;

      if Args.Count = 0
        and then
         not Cmd.List
        and then
         Cmd.Prop.all = ""
      then
         --  no search term, nor --list, nor --prop
         Reportaise_Wrong_Arguments
           ("Please provide a search term, --property, or use" &
              " --list to show all available releases");
      end if;

      if Args.Count = 0 and then Cmd.Prop.all /= "" then
         Cmd.List := True;
      end if;

      if Cmd.List and then Args.Count /= 0 then
         Reportaise_Wrong_Arguments ("Listing is incompatible with searching");
      end if;

      --  End of option verification, start of search. First load the index,
      --  required to look at its entries.

      Tab.Append (TTY.Bold ("NAME"));
      Tab.Append (TTY.Bold ("STATUS"));
      Tab.Append (TTY.Bold ("VERSION"));
      Tab.Append (TTY.Bold ("DESCRIPTION"));
      Tab.Append (TTY.Bold ("NOTES"));

      declare
         Busy : Simple_Logging.Ongoing :=
                  Simple_Logging.Activity ("Searching");

         ------------------------
         -- List_All_Or_Latest --
         ------------------------

         procedure List_All_Or_Latest
           (Crate : Alire.Crates.Crate)
         is
            Progress : Trace.Ongoing :=
                         Trace.Activity (Crate.Name.Index_Prefix)
                         with Unreferenced;
         begin
            if Cmd.Full then
               for Release of reverse Crate.Releases loop
                  List_Release (Release);
                  Busy.Step;
               end loop;
            elsif not Crate.Releases.Is_Empty then
               List_Release (Crate.Releases.Last_Element);
               Busy.Step;
            end if;
         end List_All_Or_Latest;

         --------------------
         -- List_Externals --
         --------------------

         procedure List_Externals (Crate : Alire.Crates.Crate)
         is
            Progress : Trace.Ongoing :=
                         Trace.Activity (Crate.Name.Index_Prefix)
                         with Unreferenced;
         begin
            if Cmd.External then
               --  We must show only externals that have failed detection
               --  (otherwise they'll appear as normal releases with --detect).
               for External of Crate.Externals loop
                  if not Cmd.Detect or else
                    External.Detect (Crate.Name).Is_Empty
                  then
                     List_Undetected (Crate.Name, External);
                  end if;
               end loop;
            end if;
         end List_Externals;

         ----------------
         -- List_Crate --
         ----------------

         procedure List_Crate (Crate : Alire.Crates.Crate) is
         begin
            if Cmd.Detect then
               Alire.Index.Detect_Externals (Crate.Name, Platform.Properties);
            end if;

            List_All_Or_Latest (Crate);
            List_Externals (Crate);
            Busy.Step;
         end List_Crate;

         I : Alire.Crates.Containers.Maps.Cursor :=
               Alire.Index.All_Crates.First;
         --  Cursor-based iteration because external detection during listing
         --  may cause addition of new crates, and that triggers tampering
         --  checks in some compiler versions.

         use Alire.Crates.Containers.Maps;
      begin
         if Cmd.List then
            Trace.Detail ("Searching...");
         else
            Trace.Detail ("Searching " & Alire.Utils.Quote (Args (1)) & "...");
         end if;

         while Has_Element (I) loop

            declare
               use AAA.Strings;

               Crate   : Alire.Crates.Crate renames Element (I);
               Pattern : constant String := (if Cmd.List
                                             then ""
                                             else To_Lower_Case (Args (1)));
            begin
               if Cmd.List then

                  --  List all releases
                  List_Crate (Crate);

               else

                  --  Search into release names and descriptions
                  if Contains (To_Lower_Case (+Crate.Name), Pattern)
                    or else
                     Contains (To_Lower_Case (Crate.Description), Pattern)
                  then
                     List_Crate (Crate);
                  end if;

               end if;
            end;

            Next (I);
         end loop;
      end;

      if Found = 0 then
         Log ("No hits              ");
      else
         Tab.Print (Always, Separator => "  ");
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is
     (AAA.Strings.Empty_Vector
      .Append ("Searches the given substring in crate names (or properties"
               & " with --property), and shows the most recent release"
               & " of matching crates (unless --full is specified).")
      .New_Line
      .Append ("Use --crates to get a simple list of only crate names and "
               & " descriptions. Otherwise,"
               & " besides version, description and release notes, a status"
               & " column with the following status flags is provided:")
      .New_Line
      .Append ("E: the release is externally provided.")
      .Append ("S: the release is available through a system package.")
      .Append ("U: the release is not available in the current platform.")
      .Append ("X: the release has dependencies that cannot be resolved.")
      .New_Line
      .Append ("The reasons for unavailability (U) can be ascertained with"
               & " 'alr show <crate>=<version>'.")
      .New_Line
      .Append ("Unresolvable releases (X) should not happen in platforms"
               & " with assigned maintainers. Common reasons are missing"
               & " system dependencies that have been phased out by the"
               & " platform without being updated yet in the community"
               & " index.")
     );

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
         Cmd.Crates'Access,
         "", "--crates",
         "Restrict search and output to crate names and descriptions");

      Define_Switch
        (Config,
         Cmd.Detect'Access,
         "", "--external-detect",
         "Detect externally-provided releases (implies --external)");

      Define_Switch (Config,
                     Cmd.Full'Access,
                     "", "--full",
                     "Show all versions of a crate (newest only otherwise)");

      Define_Switch (Config,
                     Cmd.List'Access,
                     "", "--list",
                     "List all available releases");

      Define_Switch (Config,
                     Cmd.External'Access,
                     "", "--external",
                     "Include externally-provided releases in search");

      Define_Switch (Config,
                     Cmd.Prop'Access,
                     "", "--property=",
                     "Search TEXT in property values",
                     Argument => "TEXT");
   end Setup_Switches;

end Alr.Commands.Search;
