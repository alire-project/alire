with AAA.Strings;
with AAA.Text_IO;

with Ada.Containers.Indefinite_Vectors;

with Alire.Config.Edit;
with Alire.Containers;
with Alire.Index;
with Alire.Origins;
with Alire.Properties;
with Alire.Releases;
with Alire.Root;
with Alire.Shared;
with Alire.Utils.User_Input;

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

   procedure Assistant (Current_OS : Platforms.Operating_Systems) is
      pragma Unreferenced (Current_OS);
      package Release_Vectors is new
        Ada.Containers.Indefinite_Vectors
          (Positive, Releases.Release, Releases."=");

      type Selections is record
         Choices : Utils.String_Vector;
         Targets : Release_Vectors.Vector;
         --  These two variables are in sync; so the picked choice says the
         --  release to use at the same position in the respective vector.
      end record;

      Selected : Containers.Release_Set;
      --  We store here all selected releases, so they are deployed in batch
      --  after all the user interactions.

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

         use all type Origins.Kinds;
         Env : constant Properties.Vector := Root.Platform_Properties;
      begin
         if Crate = GNAT_Crate then
            --  We need a bit of magic as the externals for GNAT are now in a
            --  different crate
            Index.Detect_Externals
              (GNAT_External_Crate, Root.Platform_Properties);
         else
            Index.Detect_Externals (Crate, Root.Platform_Properties);
         end if;

         --  Always offer to configure nothing
         Result.Choices.Append ("None");
         Result.Targets.Append (Releases.New_Empty_Release (Crate));
         --  Just a placeholder that won't be used anywere, but keeps boot
         --  collections in sync.

         --  Identify possible externals first (but after the newest Alire one)
         for Release of reverse Index.Releases_Satisfying (Any_Tool (Crate),
                                                           Env)
         loop
            if Release.Origin.Kind in System | External then
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
            for Release of reverse Index.Releases_Satisfying (Any_Tool (Crate),
                                                              Env)
            loop
               if Release.Origin.Is_Regular then

                  --  We want the newest native compiler packaged by Alire to
                  --  be the default. Sorting of the GNAT crate in Releases
                  --  already guarantees that the last compiler in the
                  --  collection will be a native one (if there is one).

                  if First then
                     First := False;
                     Add_Choice (Release.Milestone.TTY_Image, Release,
                                 Prepend => True);
                  else
                     Add_Choice (Release.Milestone.TTY_Image, Release);
                  end if;
               end if;
            end loop;
         end Add_Binary_Versions;

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

         Config.Edit.Set (Path  => Config.Edit.Filepath (Config.Global),
                          Key   => Tool_Key (Release.Name),
                          Value => Release.Milestone.Image);

      end Install;

      ------------------
      -- Pick_Up_Tool --
      ------------------

      procedure Pick_Up_Tool (Crate : Crate_Name; Selection : Selections) is
         Choice : constant Positive :=
                    Utils.User_Input.Query_Multi
                      (Question  =>
                                  "Please select the " & Crate.TTY_Image
                                  & " version for use with this configuration",
                       Choices   => Selection.Choices);
      begin
         if Selection.Choices (Choice) = "None" then

            Put_Info ("Selected to rely on a user-provided binary.");

            --  Clean up stored version

            Config.Edit.Unset (Path  => Config.Edit.Filepath (Config.Global),
                               Key   => Tool_Key (Crate));

         else

            Put_Info
              ("Selected tool version "
               & TTY.Bold (Selection.Targets (Choice).Milestone.TTY_Image));

            --  Store for later installation

            Selected.Insert (Selection.Targets (Choice));

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
                      & TTY.Alr
                      & " will use the version found in the environment.)");
         end if;
         Trace.Info ("");

         --  Find the newest regular release in our index:
         if not Index.Releases_Satisfying (Any_Tool (Crate),
                                           Root.Platform_Properties).Is_Empty
         then
            Pick_Up_Tool (Crate, Fill_Version_Choices (Crate));
         else
            Put_Warning
              ("No indexed versions in the catalog for crate "
               & Crate.TTY_Image);
         end if;

         Config.Edit.Set (Config.Edit.Filepath (Config.Global),
                          Config.Keys.Toolchain_Assistant,
                          "false");
      end Set_Up;

   begin

      AAA.Text_IO.Put_Paragraphs
        (AAA.Strings.Empty_Vector
         .Append ("Welcome to the toolchain selection assistant")
         .Append ("")
         .Append
           ("In this assistant you can set up the default toolchain to be "
            & "used with any crate that does not specify its own top-level "
            & "dependency on a version of " & TTY.Name ("gnat") & " or "
            & TTY.Name ("gprbuild."))
         .Append ("")
         .Append
           ("If you choose " & TTY.Italic ("""None""") & ", Alire will use "
            & "whatever version is found in the environment.")
        );

      for Tool of Tools loop
         Set_Up (Tool);
      end loop;

      for Release of Selected loop
         Install (Release);
      end loop;

   end Assistant;

   ------------------------
   -- Tool_Is_Configured --
   ------------------------

   function Tool_Is_Configured (Crate : Crate_Name) return Boolean
   is (Config.Defined (Tool_Key (Crate)));

   ---------------------
   -- Tool_Dependency --
   ---------------------

   function Tool_Dependency (Crate : Crate_Name) return Dependencies.Dependency
   is (Dependencies.New_Dependency
       (Milestones.New_Milestone (Config.Get (Tool_Key (Crate), ""))));

   -----------------
   -- Unconfigure --
   -----------------

   procedure Unconfigure (Crate : Crate_Name) is
   begin
      Config.Edit.Unset (Config.Edit.Filepath (Config.Global),
                         Tool_Key (Crate));
   end Unconfigure;

end Alire.Toolchains;
