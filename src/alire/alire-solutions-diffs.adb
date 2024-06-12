with Alire.Origins;
with Alire.Toolchains;
with Alire.Utils.Tables;
with Alire.User_Pins;
with Alire.Utils.TTY;
with AAA.Strings; use AAA.Strings;

package body Alire.Solutions.Diffs is

   --  TODO: with the new solution tracking of all dependencies status, this
   --  type could be made much simpler, even not needing to preprocess the
   --  solutions. To keep in mind for any future large refactoring needed here.

   --  Define all changes we can detect, to simplify retrieving their
   --  icon/text. These are not exclusive to each other.
   type Changes is
     (Added,      -- A new release
      Removed,    -- A removed dependency of any kind
      Upgraded,   -- An upgraded release
      Downgraded, -- A downgraded release
      Pinned,     -- A release being pinned
      Unpinned,   -- A release being unpinned
      Unchanged,  -- An unchanged dependency/release
      Missing,    -- A missing dependency
      Binary,     -- A binary, system or external release
      Info        -- General info icon
     );

   Missing_Flag : constant String := "missing";

   ----------
   -- Icon --
   ----------

   function Icon (Change : Changes) return String
   is (if TTY.Color_Enabled then
         (case Change is
             when Added      => TTY.OK    (U ("+")),
             when Removed    => TTY.Emph  (U ("✗")),
             when Upgraded   => TTY.OK    (U ("⭧")),
             when Downgraded => TTY.Warn  (U ("⭨")),
             when Pinned     => TTY.OK    (U ("📌")), -- alts: ⊙📍📌
             when Unpinned   => TTY.Emph  (U ("🎈")), -- alts: 𐩒🎈
             when Unchanged  => TTY.OK    (U ("=")),
             when Missing    => TTY.Error (U ("❗")), -- alts: ⚠️❗‼️
             when Binary     => TTY.Warn  (U ("📦")),
             when Info       => TTY.Emph  (U ("🛈")))
       else "" &
         (case Change is
             when Added      => '+',
             when Removed    => '-',
             when Upgraded   => '^',
             when Downgraded => 'v',
             when Pinned     => '.',
             when Unpinned   => 'o',
             when Unchanged  => '=',
             when Missing    => '!',
             when Binary     => 'b',
             when Info       => 'i'
         ));

   --  This type is used to summarize every detected change
   type Crate_Changes is record
      Icon,
      Best_Version : UString;
      Detail       : AAA.Strings.Vector;
   end record;

   ----------------
   -- Add_Change --
   ----------------

   procedure Add_Change (Change : in out Crate_Changes; Icon, Detail : String)
   is
      use UStrings;
   begin
      if Icon /= "" and then not Contains (+Change.Icon, Icon) then
         Append (Change.Icon, Icon);
      end if;

      if Detail /= "" then
         Change.Detail.Append (Detail);
      end if;
   end Add_Change;

   -------------
   -- Between --
   -------------

   function Between (Former, Latter : Solution) return Diff
   is (Former => Former, Latter => Latter);

   ----------------------
   -- Contains_Changes --
   ----------------------

   function Contains_Changes (This : Diff) return Boolean
   is (This.Former /= This.Latter);

   ------------------
   -- Find_Changes --
   ------------------

   function Find_Changes (This : Diff; Crate : Crate_Name) return Crate_Changes
   is
      Chg : Crate_Changes;

      use UStrings;
      use all type Dependencies.States.Fulfillments;
      use all type Dependencies.States.Transitivities;
      use all type Semantic_Versioning.Version;

      Has_Former : constant Boolean := This.Former.Depends_On (Crate);
      Has_Latter : constant Boolean := This.Latter.Depends_On (Crate);

      function Former return Dependencies.States.State
      is (This.Former.State (Crate));

      function Latter return Dependencies.States.State
      is (This.Latter.State (Crate));

      -------------------
      -- Add_Or_Remove --
      -------------------

      procedure Add_Or_Remove is
      begin
         if not Has_Former and then Has_Latter then
            Add_Change (Chg, Icon (Added), "new");
         elsif Has_Former and then not Has_Latter then
            Add_Change (Chg, Icon (Removed), "removed");
         end if;
      end Add_Or_Remove;

      -------------------
      -- Fulfil_Change --
      -------------------

      procedure Fulfil_Change is

         -----------------
         -- Gains_State --
         -----------------

         function Gains_State (Fulfilment : Dependencies.States.Fulfillments)
                            return Boolean
         is ((not Has_Former or else Former.Fulfilment not in Fulfilment)
             and then Has_Latter and then Latter.Fulfilment in Fulfilment);

         use type Alire.User_Pins.Pin;

      begin
         --  Changed linked dir
         if Has_Latter and then Latter.Is_Linked and then
           (not Has_Former or else not Former.Is_Linked or else
            Former.Link /= Latter.Link)
         then
            Add_Change (Chg, Icon (Pinned),
                        Latter.Link.Image (User => True));

         --  New unsolvable
         elsif Gains_State (Missed) then
            Add_Change (Chg, Icon (Missing),
                        TTY.Error (Missing_Flag) & ":"
                        & TTY.Warn (To_Lower_Case (Latter.Reason'Image)));

         --  From hint to proper release
         elsif Has_Former and then Former.Is_Hinted and then
           Gains_State (Solved)
         then
            --  The crate was formerly in the solution, but not as regular
            --  release. Pinning is reported separately, so warn only when
            --  it was formerly an external.
            Add_Change (Chg, Icon (Added), TTY.OK ("solved"));
         end if;
      end Fulfil_Change;

      --------------------------
      -- transitivity_changed --
      --------------------------

      procedure Transitivity_Changed is
      begin

         --  Inform only when changing from direct to indirect or viceversa.
         --  A new direct dependency merits no highlight since it is trivially
         --  expected.

         if (not Has_Former or else Former.Transitivity = Direct) and then
           Has_Latter and then Latter.Transitivity = Indirect
         then
            Add_Change (Chg, "", "indirect");
         elsif Has_Former and then Has_Latter and then
           Former.Transitivity /= Latter.Transitivity
         then
            Add_Change (Chg, "",
                        AAA.Strings.To_Lower_Case (Latter.Transitivity'Img));
         end if;
      end Transitivity_Changed;

      ------------------------
      -- Pinned_Or_Unpinned --
      ------------------------

      procedure Pinned_Or_Unpinned is
      begin
         if (not Has_Former or else not Former.Is_User_Pinned) and then
           Has_Latter and then Latter.Is_User_Pinned
         then
            --  The actual pin change will be shown via version/target
            Add_Change (Chg, Icon (Pinned), "");
         elsif Has_Former and then Former.Is_User_Pinned and then
           Has_Latter and then not Latter.Is_User_Pinned
         then
            Add_Change (Chg, Icon (Unpinned), "unpinned");
         end if;

         --  Report pin version when new/changed. Link target already reported
         --  in Fulfil_Change.
         if Has_Latter and then Latter.Is_Pinned and then
           (not Has_Former or else not Former.Is_Pinned or else
            Former.Pin_Version /= Latter.Pin_Version)
         then
            Add_Change (Chg, Icon (Pinned),
                        "pin=" & TTY.Version (Latter.Pin_Version.Image));
         end if;
      end Pinned_Or_Unpinned;

      ---------------------
      -- Provider_Change --
      ---------------------

      procedure Provider_Change is
      begin
         if Has_Latter and then Latter.Is_Provided
           and then
             (not Has_Former or else
                (Former.Has_Release and then
                 Former.Release.Name_Str /= Latter.Release.Name_Str))
         then
            Add_Change (Chg, "", TTY.Italic (Latter.Release.Name.As_String));
         end if;
      end Provider_Change;

      ---------------------
      -- Up_Or_Downgrade --
      ---------------------

      procedure Up_Or_Downgrade is
      begin
         if Has_Former and then Former.Has_Release and then
           Has_Latter and then Latter.Has_Release
         then
            if Former.Release.Version < Latter.Release.Version then
               Add_Change
                 (Chg,
                  Icon (Upgraded),
                  "upgraded from "
                  & TTY.Version (Former.Release.Version.Image));
            elsif Latter.Release.Version < Former.Release.Version then
               Add_Change
                 (Chg,
                  Icon (Downgraded),
                  "downgraded from "
                  & TTY.Version (Former.Release.Version.Image));
            end if;

         end if;
      end Up_Or_Downgrade;

      --------------------------------
      -- Determine_Relevant_Version --
      --------------------------------

      procedure Determine_Relevant_Version is

         ------------------
         -- Best_Version --
         ------------------

         function Best_Version (State : Dependencies.States.State)
                                return String
         is (if State.Has_Release then
                TTY.Version (State.Release.Version.Image)
             elsif State.Is_Linked then -- linked dir without alire metadata
                TTY.Warn ("unknown")
             elsif State.Is_Hinted then -- undetected external, show dep
                TTY.Version (State.Versions.Image)
             else -- Not used, but just in case, the crate is in missing state:
                TTY.Error (State.Versions.Image));

      begin
         --  Default to unknown

         Chg.Best_Version := +TTY.Error ("unknown");

         --  Find something better

         if Has_Latter then
            Chg.Best_Version := +Best_Version (Latter);
         elsif Has_Former then
            --  Crate is gone, so it has no current version, show the one
            --  disappearing from the solutions.
            Chg.Best_Version := +Best_Version (Former);
         else
            Recoverable_Program_Error
              ("crate is neither in former or latter");
         end if;

      end Determine_Relevant_Version;

      ------------------------------
      -- Releases_Without_Sources --
      ------------------------------

      procedure Releases_Without_Sources is
         use all type Origins.Kinds;
         subtype Report_Kinds is Origins.Kinds with Static_Predicate =>
           Report_Kinds in Binary_Archive | External | System;
      begin
         --  For "special" releases, show extra info: binaries that can be very
         --  large, or releases that are not from sources (so harder to audit,
         --  intrinsically shared, ...), but only if they were going to be
         --  shown already.

         if not Has_Latter or else not Latter.Has_Release or else
           Chg.Detail.Is_Empty
         then
            return;
         end if;

         declare
            Rel : constant Alire.Releases.Release :=
                    This.Latter.Releases.Element (Crate);
         begin
            if Rel.Origin.Kind in Report_Kinds then
               Add_Change (Chg, Icon (Binary),
                           TTY.Warn
                             (case Report_Kinds (Rel.Origin.Kind) is
                                 when Binary_Archive => "binary",
                                 when External       => "executable in path",
                                 when System         => "system package"));
            end if;
         end;
      end Releases_Without_Sources;

      ----------------------
      -- Missing_Releases --
      ----------------------

      procedure Missing_Releases is
      begin
         if not Contains (Chg.Detail.Flatten, Missing_Flag) and then
           Has_Latter and then
           Latter.Is_Unfulfilled
         then
            Add_Change (Chg, Icon (Missing),
                        TTY.Error (Missing_Flag)
                        & (if Latter.Is_Missing -- Has a reason
                          then ":"
                          & TTY.Warn (To_Lower_Case (Latter.Reason'Image))
                          else "" -- hinted don't have a reason
                         ));
         end if;
      end Missing_Releases;

   begin

      --  Go through possible changes and add each marker

      Add_Or_Remove;

      Pinned_Or_Unpinned;

      Fulfil_Change;

      Provider_Change;

      Transitivity_Changed;

      Up_Or_Downgrade;

      Determine_Relevant_Version;

      Releases_Without_Sources;
      --  This one must go after the rest, as it only appends info if the
      --  release was going to be shown anyway.

      --  Final fill-in for no changes and missing (always reported)

      if Length (Chg.Icon) = 0 then
         Add_Change (Chg, Icon (Unchanged), "");
      end if;

      if Chg.Detail.Is_Empty then
         Add_Change (Chg, "", "unchanged");
      end if;

      Missing_Releases;

      return Chg;

   end Find_Changes;

   ------------------------
   -- Latter_Is_Complete --
   ------------------------

   function Latter_Is_Complete (This : Diff) return Boolean
   is (This.Latter.Is_Complete);

   -----------
   -- Print --
   -----------

   procedure Print (This         : Diff;
                    Changed_Only : Boolean      := not Alire.Detailed;
                    Prefix       : String       := "   ";
                    Level        : Trace.Levels := Trace.Info)
   is
      Table : Utils.Tables.Table;
      Changed    : Boolean := False;

      -----------------------------
      -- Warn_Toolchain_Download --
      -----------------------------
      --  If the solution requires downloading a new toolchain, warn about it
      procedure Warn_Toolchain_Download is
      begin
         for Rel of This.Latter.Releases loop
            if Toolchains.Is_Tool (Rel)
              and then not Toolchains.Available.Contains (Rel)
            then
               Trace.Log (Prefix, Level);
               Trace.Log
                 (Prefix & Icon (Info)
                  & "  The solution requires a toolchain that is");
               Trace.Log
                 (Prefix
                  & "   not yet installed. Accepting the solution");
               Trace.Log
                 (Prefix
                  & "   will download and install this toolchain.");
               return;
            end if;
         end loop;
      end Warn_Toolchain_Download;

      --------------------------------------
      -- Warn_Unsatisfiable_GNAT_External --
      --------------------------------------

      procedure Warn_Unsatisfiable_GNAT_External is
      begin
         for Dep of This.Latter.All_Dependencies loop
            if Dep.Crate = GNAT_Crate
              and then not Dep.Is_Solved
              and then Toolchains.Tool_Is_Configured (GNAT_Crate)
              and then Toolchains.Tool_Milestone (GNAT_Crate).Crate
                       = GNAT_External_Crate
              and then
                (Toolchains.Tool_Is_Missing (GNAT_Crate)
                 or else
                   not Toolchains.Tool_Release (GNAT_Crate).Satisfies (Dep))
            then
               Trace.Log (Prefix, Level);
               Trace.Log (Prefix & Icon (Missing)
                          & " The explicitly configured external compiler "
                          & Toolchains.Tool_Milestone (GNAT_Crate).TTY_Image);
               Trace.Log (Prefix
                          & "  cannot satisfy dependency in solution "
                          & Dep.As_Dependency.TTY_Image,
                          Log_Level);
               Trace.Log (Prefix
                          & "  You can select a different compiler for the "
                          & "workspace with");
               Trace.Log (Prefix & "  "
                          & TTY.Terminal ("alr toolchain --local --select"),
                          Log_Level);
            end if;
         end loop;
      end Warn_Unsatisfiable_GNAT_External;

      type Passes is (Fulfilled, Missing);
      Missing_Header_Added : Boolean := False;

   begin

      --  Start with an empty line to separate from previous output

      Trace.Log ("", Level);

      if not This.Latter.Is_Complete then
         Trace.Log (Prefix & "New solution is " & TTY.Warn ("incomplete."),
                    Level);
      elsif This.Latter.Is_Complete and then not This.Former.Is_Complete then
         Trace.Log (Prefix & "New solution is " & TTY.OK ("complete."),
                    Level);
      end if;

      --  Detailed changes, showing separately missing dependencies

      for Pass in Passes'Range loop
         for Crate of This.Former.Crates.Union (This.Latter.Crates) loop
            declare
               Changes : constant Crate_Changes := Find_Changes (This, Crate);
            begin

               if not Changed_Only or else
                 Changes.Detail.Flatten /= "unchanged" or else
                 Pass = Missing -- Always show missing ones to raise awareness
               then

                  if (Pass = Fulfilled and then
                        not Contains (Changes.Detail.Flatten, Missing_Flag))
                    or else
                      (Pass = Missing and then
                       Contains (Changes.Detail.Flatten, Missing_Flag))
                  then
                     Changed := Changed or True;

                     --  Header for missing section on first missing

                     if Pass = Missing and then not Missing_Header_Added then
                        Missing_Header_Added := True;
                        Table.Append (Prefix & TTY.Warn ("Missing:")).New_Row;
                     end if;

                     --  Show icon of change

                     Table.Append (Prefix & (+Changes.Icon));

                     --  Always show crate name

                     Table.Append (Utils.TTY.Name (Crate));

                     --  Show most precise version available

                     Table.Append (+Changes.Best_Version);

                     --  Show an explanation of the change depending on
                     --  status changes.

                     Table.Append ("(" & Changes.Detail.Flatten (",") & ")");

                     Table.New_Row;
                  end if;
               end if;
            end;
         end loop;
      end loop;

      if Changed then
         Table.Print (Level);

         Warn_Toolchain_Download;
         Warn_Unsatisfiable_GNAT_External;
         --  Only one of those two can happen and emit their warning, so the
         --  order doesn't matter.
      else
         Trace.Log (Prefix & "No changes between former and new solution.",
                    Level);
      end if;
   end Print;

end Alire.Solutions.Diffs;
