with Alire.Containers;
with Alire.Utils.Tables;
with Alire.Utils.TTY;

package body Alire.Solutions.Diffs is

   --  TODO: with the new solution tracking of all dependencies status, this
   --  type could be made much simpler, even not needing to preprocess the
   --  solutions. To keep in mind for any future large refactoring needed here.

   package TTY renames Utils.TTY;

   use type Semantic_Versioning.Version;

   ------------------
   -- Best_Version --
   ------------------

   function Best_Version (Status : Crate_Status) return String is
     (case Status.Status is
         when Needed   => Semantic_Versioning.Image (Status.Version),
         when Linked   => "file:" & (+Status.Path),
         when Hinted   => Status.Versions.Image,
         when Unneeded => "unneeded",
         when Unsolved => "unsolved");

   -------------
   -- Between --
   -------------

   function Between (Former, Latter : Solution) return Diff is

      use type Containers.Crate_Name_Sets.Set;

      -----------------
      -- Make_Status --
      -----------------

      function Make_Status (Crate : Crate_Name;
                            Sol   : Solution) return Crate_Status is
      begin
         if Sol.Releases.Contains (Crate) then
            return (Status  => Needed,
                    Pinned  => Sol.State (Crate).Is_Pinned,
                    Version => Sol.State (Crate).Release.Version);
         elsif Sol.Links.Contains (Crate) then
            return (Status => Linked,
                    Path   => +Sol.State (Crate).Link.Path);
         elsif Sol.Hints.Contains (Crate) then
            return (Status   => Hinted,
                    Versions => Sol.Dependency (Crate).Versions);

         elsif Sol.Depends_On (Crate) then
            return (Status => Unsolved);

         else
            return (Status => Unneeded);

         end if;
      end Make_Status;

      --  Get all involved crates, before and after

      Crates : constant Containers.Crate_Name_Sets.Set :=
                 Former.Crates or Latter.Crates;
   begin
      return This : Diff do

         --  Solution validities

         This.Former_Complete := Former.Is_Complete;
         This.Latter_Complete := Latter.Is_Complete;

         --  Store changes for each crate

         for Crate of Crates loop
            This.Changes.Insert (Crate,
                                 Crate_Changes'
                                   (Former => Make_Status (Crate, Former),
                                    Latter => Make_Status (Crate, Latter)));
         end loop;

      end return;
   end Between;

   ------------
   -- Change --
   ------------

   function Change (This : Diff; Crate : Crate_Name) return Changes is
      Former : Crate_Status renames This.Changes (Crate).Former;
      Latter : Crate_Status renames This.Changes (Crate).Latter;
   begin

      --  Changes in pinning take precedence

      if Latter.Status = Needed and then Latter.Pinned and then
        (Former.Status /= Needed or else not Former.Pinned)
      then
         return Pinned;
      end if;

      if Former.Status = Needed and then Former.Pinned and then
        (Latter.Status /= Needed or else not Latter.Pinned)
      then
         return Unpinned;
      end if;

      --  Other changes that don't involve pinning

      return
        (case Latter.Status is
            when Needed =>
              (if Former.Status = Needed then
                 (if Former.Version < Latter.Version then Upgraded
                  elsif Former.Version = Latter.Version then Unchanged
                  else Downgraded)
               else Added),
            when Linked   => Pinned,
            when Hinted   => External,
            when Unneeded => Removed,
            when Unsolved => Unsolved);
   end Change;

   ----------------------
   -- Contains_Changes --
   ----------------------

   function Contains_Changes (This : Diff) return Boolean is
     (This.Former_Complete /= This.Latter_Complete or else
      (for some Change of This.Changes => Change.Former /= Change.Latter));

   ------------------------
   -- Pin_Change_Summary --
   ------------------------

   function Pin_Change_Summary (Former, Latter : Crate_Status) return String
   is
   begin
      --  Show what's going on with versions

      if Former.Status = Needed and then Latter.Status = Needed then
         if Former.Version < Latter.Version then
            return ", upgraded from " & TTY.Version (Former.Version.Image);
         elsif Former.Version = Latter.Version then
            return ", version unchanged";
         else
            return ", downgraded from " & TTY.Version (Former.Version.Image);
         end if;
      elsif Former.Status = Needed and then Latter.Status /= Needed then
         return " from " & TTY.Version (Former.Version.Image);
      else
         --  Pinned, nothing else to say
         return "";
      end if;
   end Pin_Change_Summary;

   -----------
   -- Print --
   -----------

   procedure Print (This         : Diff;
                    Changed_Only : Boolean;
                    Prefix       : String       := "   ";
                    Level        : Trace.Levels := Trace.Info)
   is
      use Change_Maps;

      package Semver renames Semantic_Versioning;

      Table : Utils.Tables.Table;
   begin

      --  Start with an empty line to separate from previous output

      Trace.Log ("", Level);

      if not This.Latter_Complete then
         Trace.Log (Prefix & "New solution is " & TTY.Warn ("invalid."),
                    Level);
      elsif This.Latter_Complete and then not This.Former_Complete then
         Trace.Log (Prefix & "New solution is " & TTY.OK ("valid."),
                    Level);
      end if;

      --  Early exit if no changes

      if Changed_Only and then not This.Contains_Changes then
         Trace.Log (Prefix & "No changes between former an new solution.",
                    Level);
         return;
      end if;

      --  Detailed changes otherwise

      for I in This.Changes.Iterate loop
         declare
            Former : Crate_Status renames This.Changes (I).Former;
            Latter : Crate_Status renames This.Changes (I).Latter;
         begin
            if not Changed_Only or else Former /= Latter then

               --  Show icon of change

               if TTY.Color_Enabled then
                  Table.Append
                    (Prefix
                     & (case This.Change (Key (I)) is
                          when Added      => TTY.OK    ("✓"),
                          when Removed    => TTY.Emph  ("✗"),
                          when External   => TTY.Warn  ("↪"),
                          when Upgraded   => TTY.OK    ("⭧"),
                          when Downgraded => TTY.Warn  ("⭨"),
                          when Pinned     => TTY.OK    ("⊙"),
                          when Unpinned   => TTY.Emph  ("𐩒"),
                          when Unchanged  => TTY.OK    ("="),
                          when Unsolved   => TTY.Error ("⚠")));
               else
                  Table.Append
                    (Prefix
                     & (case This.Change (Key (I)) is
                          when Added      => "+",
                          when Removed    => "-",
                          when External   => "~",
                          when Upgraded   => "^",
                          when Downgraded => "v",
                          when Pinned     => ".",
                          when Unpinned   => "o",
                          when Unchanged  => "=",
                          when Unsolved   => "!"));
               end if;

               --  Always show crate name

               Table.Append (TTY.Name (+Key (I)));

               --  Show most precise version available

               if Latter.Status in Hinted | Linked | Needed then
                  Table.Append (TTY.Version (Best_Version (Latter)));
               else
                  Table.Append (TTY.Version (Best_Version (Former)));
               end if;

               --  Finally show an explanation of the change depending on
               --  status changes.

               Table.Append
                 ("("
                  & (case This.Change (Key (I)) is
                       when Added      => "new",
                       when Removed    => "removed",
                       when External   => "external",
                       when Upgraded   => "upgraded from "
                                 & TTY.Version (Semver.Image (Former.Version)),
                       when Downgraded => "downgraded from "
                                 & TTY.Version (Semver.Image (Former.Version)),
                       when Pinned     => "pinned"
                                         & Pin_Change_Summary (Former, Latter),
                       when Unpinned   => "unpinned"
                                         & Pin_Change_Summary (Former, Latter),
                       when Unchanged  => "unchanged",
                       when Unsolved   => "missing")
                  & ")");

               Table.New_Row;
            end if;
         end;
      end loop;

      Table.Print (Level);
   end Print;

end Alire.Solutions.Diffs;
