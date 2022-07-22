with AAA.Text_IO;

with Ada.Containers.Indefinite_Vectors;

with Alire.Config.Edit;
with Alire.Index;
with Alire.Origins;
with Alire.Platforms.Current;
with Alire.Properties;
with Alire.Releases.Containers;
with Alire.Root;
with Alire.Shared;

with CLIC.User_Input;
with CLIC.Config.Edit;

with Semantic_Versioning.Extended;

package body Alire.Toolchains is

   --------------
   -- Any_Tool --
   --------------
   --  crate=* dependency builder
   function Any_Tool (Crate : Crate_Name) return Dependencies.Dependency
   is (Dependencies.New_Dependency
        (Crate, Semantic_Versioning.Extended.Any));

   ---------------
   -- Assistant --
   ---------------

   procedure Assistant (Level              : Config.Level;
                        Allow_Incompatible : Boolean := False) is
      package Release_Vectors is new
        Ada.Containers.Indefinite_Vectors
          (Positive, Releases.Release, Releases."=");

      type Selections is record
         Choices : AAA.Strings.Vector;
         Targets : Release_Vectors.Vector;
         --  These two variables are in sync; so the picked choice says the
         --  release to use at the same position in the respective vector.
      end record;

      Selected : Releases.Containers.Release_Set;
      --  We store here all selected releases, so they are deployed in batch
      --  after all the user interactions.

      use all type Origins.Kinds;
      Origin_Frozen : Boolean := False;
      Chosen_Origin : Origins.Kinds;
      --  GNAT and gprbuild should go in tandem; either from system packages,
      --  from some external user-provided location, or from indexed releases.
      --  Otherwise they don't see each other. When the user picks the first
      --  tool with a certain origin, only matching origins are allowed for
      --  the remaining tool.

      None : constant String := "None";

      ---------------------
      -- Is_Valid_Choice --
      ---------------------

      function Is_Valid_Choice (R : Releases.Release) return Boolean
      is (Allow_Incompatible
          or else not Origin_Frozen
          or else Chosen_Origin = R.Origin.Kind);

      --------------------------
      -- Fill_Version_Choices --
      --------------------------

      function Fill_Version_Choices (Crate : Crate_Name)
                                     return Selections
      is

         Result : Selections;

         ----------------
         -- Add_Choice --
         ----------------

         procedure Add_Choice (Text    : String;
                               Release : Alire.Releases.Release;
                               Prepend : Boolean := False)
         is
         begin
            if Prepend then
               Result.Choices.Prepend (Text);
               Result.Targets.Prepend (Release);
            else
               Result.Choices.Append (Text);
               Result.Targets.Append (Release);
            end if;
         end Add_Choice;

         Env : constant Properties.Vector := Root.Platform_Properties;
      begin
         Index.Detect_Externals (Crate, Root.Platform_Properties);

         --  Always offer to configure nothing
         Result.Choices.Append (None);
         Result.Targets.Append (Releases.New_Empty_Release (Crate));
         --  Just a placeholder that won't be used anywhere, but keeps both
         --  collections in sync.

         --  Identify possible externals first (but after the newest Alire one)
         for Release of reverse Index.Releases_Satisfying (Any_Tool (Crate),
                                                           Env)
         loop
            if Release.Origin.Kind in System | External and then
              Is_Valid_Choice (Release)
            then
               Add_Choice (Release.Milestone.TTY_Image
                           & TTY.Dim (" [" & Release.Notes & "]"),
                           Release);
            end if;
         end loop;

         --  Regular choices go afterwards, except for the most current one
         --  which goes before anything else.
         Add_Binary_Versions :
         declare
            First : Boolean := True;
         begin
            for Release of reverse
              Releases.Containers.From_Set -- This sorts by version
                (Index.Releases_Satisfying (Any_Tool (Crate),
                 Env))
            loop
               if Release.Origin.Is_Regular and then Is_Valid_Choice (Release)
               then

                  --  We want the newest native compiler packaged by Alire to
                  --  be the default. Sorting of the GNAT crate in Releases
                  --  already guarantees that the last compiler in the
                  --  collection will be a native one (if there is one).

                  if First and then Release.Name.As_String = "gnat_native" then
                     First := False;
                     Add_Choice (Release.Milestone.TTY_Image, Release,
                                 Prepend => True);
                  else
                     Add_Choice (Release.Milestone.TTY_Image, Release);
                  end if;
               end if;
            end loop;
         end Add_Binary_Versions;

         --  If it turns out that the first choice is None, this means that
         --  the user has selected a external/system toolchain, and no native
         --  options are offered. In this case, we move None to the second
         --  position.

         if Result.Choices.First_Element = None and then
           Natural (Result.Choices.Length) > 1
         then
            Result.Choices.Delete_First;
            Result.Targets.Delete_First;
            Result.Choices.Insert (Before => 2, New_Item => None, Count => 1);
            Result.Targets.Insert
              (Before   => 2,
               New_Item => Releases.New_Empty_Release (Crate),
               Count    => 1);
         end if;

         return Result;
      end Fill_Version_Choices;

      -------------
      -- Install --
      -------------

      procedure Install (Release : Releases.Release) is
      begin

         --  If the selected tool is one of our regular indexed ones, install
         --  the tool. Also, store the version in our configuration for future
         --  reference. On the contrary, if the selection is from system
         --  packages or the environment, we need not to install anything.
         --  (We are not offering system packages, as only one gnat can
         --  be installed e.g. in Debian, and changing it would affect the
         --  whole system. We only offer external compilers detected in the
         --  environment.)

         --  Deploy as a shared install unless external

         if Release.Origin.Is_Regular then
            Shared.Share (Release);
         else
            Trace.Debug
              ("The user selected a external version as default for "
               & Release.Milestone.TTY_Image);
         end if;

         --  Store tool milestone after successful deployment

         Set_As_Default (Release, Level);

      end Install;

      ------------------
      -- Pick_Up_Tool --
      ------------------

      procedure Pick_Up_Tool (Crate : Crate_Name; Selection : Selections) is
         Choice : constant Positive :=
           CLIC.User_Input.Query_Multi
             (Question  =>
                "Please select the " & Crate.TTY_Image
              & " version for use with this configuration",
              Choices   => Selection.Choices);
      begin
         if Selection.Choices (Choice) = None then

            Put_Info ("Selected to rely on a user-provided binary.");

            --  Clean up stored version

            Unconfigure (Crate, Level);

         else

            Put_Info
              ("Selected tool version "
               & TTY.Bold (Selection.Targets (Choice).Milestone.TTY_Image));

            --  Store for later installation

            declare
               Selected_Release : constant Releases.Release :=
                                    Selection.Targets (Choice);
            begin
               Selected.Insert (Selected_Release);

               --  And verify we are not mixing external/indexed tools

               if not Origin_Frozen then
                  Origin_Frozen := True;
                  Chosen_Origin := Selected_Release.Origin.Kind;
               elsif Chosen_Origin /= Selected_Release.Origin.Kind then
                  raise Program_Error with
                    "Mixed selection should not be offered";
               end if;
            end;
         end if;
      end Pick_Up_Tool;

      ------------
      -- Set_Up --
      ------------

      procedure Set_Up (Crate : Crate_Name) is
      begin

         Trace.Info ("");
         if Tool_Is_Configured (Crate) then
            Put_Info ("Currently configured: "
                      & Tool_Dependency (Crate).TTY_Image);
         else
            Put_Info (Crate.TTY_Image & " is currently not configured. ("
                      & Utils.TTY.Alr
                      & " will use the version found in the environment.)");
         end if;
         Trace.Info ("");

         --  Find the newest regular release in our index:
         if not Index.Releases_Satisfying (Any_Tool (Crate),
                                           Root.Platform_Properties,
                                           Opts => Index.Query_Fully).Is_Empty
         then
            Pick_Up_Tool (Crate, Fill_Version_Choices (Crate));
         else
            Put_Warning
              ("No indexed versions in the catalog for crate "
               & Crate.TTY_Image);
         end if;

      end Set_Up;

   begin

      AAA.Text_IO.Put_Paragraphs
        (AAA.Strings.Empty_Vector
         .Append ("Welcome to the toolchain selection assistant")
         .Append ("")
         .Append
           ("In this assistant you can set up the default toolchain to be "
            & "used with any crate that does not specify its own top-level "
            & "dependency on a version of " & Utils.TTY.Name ("gnat") & " or "
            & Utils.TTY.Name ("gprbuild."))
         .Append ("")
         .Append
           ("If you choose " & TTY.Italic ("""None""") & ", Alire will use "
            & "whatever version is found in the environment.")
        );

      if Allow_Incompatible then
         Put_Warning ("Selection of incompatible tools is "
                      & TTY.Emph ("enabled"), Trace.Warning);
      end if;

      for Tool of Tools loop
         if not Allow_Incompatible
           and then Tool /= Tools.First_Element
           and then not Selected.Is_Empty
         then
            Trace.Info ("");
            Put_Info ("Choices for the following tool are narrowed down to "
                      & "releases compatible with just selected "
                      & Selected.First_Element.Milestone.TTY_Image);
            Trace.Detail ("Origin allowed for compatible tools is currently: "
                          & Chosen_Origin'Image);
         end if;
         Set_Up (Tool);
      end loop;

      --  The user has already chosen, so disable the assistant

      Set_Automatic_Assistant (False, Level);

      --  Finally deploy selections

      for Release of Selected loop
         Install (Release);
      end loop;

   end Assistant;

   procedure Detect_Externals is
   begin
      for Tool of Tools loop
         Index.Detect_Externals (Tool, Platforms.Current.Properties);
      end loop;
   end Detect_Externals;

   --------------------
   -- Set_As_Default --
   --------------------

   procedure Set_As_Default (Release : Releases.Release; Level : Config.Level)
   is
   begin
      Alire.Config.Edit.Set
        (Level,
         Key   => Tool_Key (Release.Name),
         Value => Release.Milestone.Image);
      Alire.Config.Edit.Set
        (Level,
         Key   => Tool_Key (Release.Name, For_Is_External),
         Value => Boolean'(not Release.Origin.Is_Regular)'Image);
   end Set_As_Default;

   -----------------------------
   -- Set_Automatic_Assistant --
   -----------------------------

   procedure Set_Automatic_Assistant (Enabled : Boolean; Level : Config.Level)
   is
   begin
      Config.Edit.Set (Level,
                       Config.Keys.Toolchain_Assistant,
                       (if Enabled then "true" else "false"));
   end Set_Automatic_Assistant;

   ------------------------
   -- Tool_Is_Configured --
   ------------------------

   function Tool_Is_Configured (Crate : Crate_Name) return Boolean
   is (Config.DB.Defined (Tool_Key (Crate)));

   ---------------------
   -- Tool_Dependency --
   ---------------------

   function Tool_Dependency (Crate : Crate_Name) return Dependencies.Dependency
   is (Dependencies.New_Dependency (Tool_Milestone (Crate)));

   ------------------
   -- Tool_Release --
   ------------------

   function Tool_Release (Crate : Crate_Name) return Releases.Release
   is
   begin
      if not Tool_Is_Configured (Crate) then
         Raise_Checked_Error ("Requested tool is not configured: "
                              & Utils.TTY.Name (Crate));
      else
         return Shared.Release (Tool_Milestone (Crate));
      end if;
   exception
      when E : Constraint_Error =>
         Log_Exception (E);
         Raise_Checked_Error ("Requested tool configured but not installed: "
                              & Utils.TTY.Name (Crate));
   end Tool_Release;

   -----------------
   -- Unconfigure --
   -----------------

   procedure Unconfigure (Crate         : Crate_Name;
                          Level         : Config.Level;
                          Fail_If_Unset : Boolean := True) is
   begin
      if CLIC.Config.Defined (Config.DB, Tool_Key (Crate)) and then
        not CLIC.Config.Edit.Unset
          (Config.Edit.Filepath (Level),
           Tool_Key (Crate))
      then
         declare
            Msg : constant String :=
                    "Cannot unset config key " & Tool_Key (Crate)
                  & " at config level " & Level'Image;
         begin
            if Fail_If_Unset then
               Raise_Checked_Error (Msg);
            else
               Trace.Debug (Msg);
            end if;
         end;
      end if;
   end Unconfigure;

end Alire.Toolchains;
