with AAA.Table_IO;

with Alire.Containers;
with Alire.Externals;
with Alire.Index;
with Alire.Origins.Deployers;
with Alire.Crates.With_Releases;
with Alire.Platform;
with Alire.Releases;

with Alr.Platform;
with Alr.Query;
with Alr.Utils;

with Semantic_Versioning;

package body Alr.Commands.Search is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is

      Found : Natural := 0;
      Tab   : AAA.Table_IO.Table;

      ------------------
      -- List_Release --
      ------------------

      procedure List_Release (R : Alire.Releases.Release) is
      begin
         if (Cmd.Prop.all = ""
             or else
             R.Property_Contains (Cmd.Prop.all)
             or else
             Utils.Contains (R.Notes, Cmd.Prop.all)
             or else
             Utils.Contains (R.Description,
                             Cmd.Prop.all))
           and then
             (Cmd.External or else not R.Origin.Is_Native)
         then
            Found := Found + 1;
            Tab.New_Row;
            Tab.Append (+R.Name);
            Tab.Append ((if R.Origin.Is_Native then "N" else " ") &
                        (if Query.Is_Available (R) then " " else "U") &
                        (if Query.Is_Resolvable
                             (R.Depends (Platform.Properties))
                           then " "
                           else "X"));
            Tab.Append (Semantic_Versioning.Image
                        (R.Version) &
                        (if R.Origin.Is_Native and then
                           Alire.Platform.Distribution_Is_Known
                         then "+" & Alire.Origins.Deployers.New_Deployer
                             (R.Origin).Native_Version
                         else ""));
            Tab.Append (R.Description);
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
         Tab.Append ("E" &
                     (if Cmd.Detect then "U" else " ") &
                       " ");
         Tab.Append ("external");
         Tab.Append (Alire.Index.Crate (Name).Description);
         Tab.Append (Ext.Image);
      end List_Undetected;

      use Alire.Containers.Release_Sets;
   begin
      if Cmd.Detect then
         Cmd.External := True;
      end if;

      if Num_Arguments = 0
        and then
         not Cmd.List
        and then
         Cmd.Prop.all = ""
      then
         --  no search term, nor --list, nor --prop
         Trace.Error ("Please provide a search term, --property, or use" &
                        " --list to show all available releases");
         raise Wrong_Command_Arguments;
      end if;

      if Num_Arguments = 0 and then Cmd.Prop.all /= "" then
         Cmd.List := True;
      end if;

      if Cmd.List and then Num_Arguments /= 0 then
         Trace.Error ("Listing is incompatible with searching");
         raise Wrong_Command_Arguments;
      end if;

      --  End of option verification, start of search. First load the index,
      --  required to look at its entries.

      Requires_Full_Index;

      Tab.Append ("NAME");
      Tab.Append ("STATUS");
      Tab.Append ("VERSION");
      Tab.Append ("DESCRIPTION");
      Tab.Append ("NOTES");

      declare
         Busy : Utils.Busy_Prompt := Utils.Busy_Activity ("Searching...");

         ------------------------
         -- List_All_Or_Latest --
         ------------------------

         procedure List_All_Or_Latest
           (Crate : Alire.Crates.With_Releases.Crate) is
         begin
            if Cmd.Full then
               for Release of Crate.Releases loop
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

         procedure List_Externals (Crate : Alire.Crates.With_Releases.Crate)
         is
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

         procedure List_Crate (Crate : Alire.Crates.With_Releases.Crate) is
         begin
            if Cmd.Detect then
               Alire.Index.Add_Externals (Crate.Name);
            end if;

            List_All_Or_Latest (Crate);
            List_Externals (Crate);
            Busy.Step;
         end List_Crate;

      begin
         if Cmd.List then
            Trace.Detail ("Searching...");
            for Crate of Alire.Index.All_Crates.all loop
               List_Crate (Crate);
            end loop;
         else
            declare
               Pattern : constant String := Argument (1);
            begin
               Trace.Detail ("Searching " & Utils.Quote (Pattern) & "...");

               for Crate of Alire.Index.All_Crates.all loop
                  if Utils.Contains (+Crate.Name, Pattern) then
                     List_Crate (Crate);
                  end if;
               end loop;
            end;
         end if;
      end;

      if Found = 0 then
         Log ("No hits              ");
      else
         Tab.Print (Separator => "  ");
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Searches the given substring in crate names (or properties"
               & " with --property), and shows the most recent release"
               & " of matching crates (unless --full is specified).")
      .New_Line
      .Append ("Besides version, description and release notes, a status"
               & " column with the following status flags is provided:")
      .New_Line
      .Append ("E: the release is externally provided.")
      .Append ("N: the release is available through a system package.")
      .Append ("U: the release is not available in the current platform.")
      .Append ("X: the release has dependencies that cannot be resolved.")
      .New_Line
      .Append ("The reasons for unavalaibility (U) can be ascertained with"
               & " 'alr show <crate>=<version>'.")
      .New_Line
      .Append ("Unresolvable releases (X) should not happen in platforms"
               & " with assigned maintainers. Common reasons are missing"
               & " native dependencies that have been phased out by the"
               & " platform without being updated yet in the community"
               & " index.")
     );

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
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
