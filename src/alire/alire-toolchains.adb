with AAA.Strings;
with AAA.Text_IO;

with Ada.Containers.Indefinite_Vectors;

with Alire.Config.Edit;
with Alire.Index;
with Alire.Origins;
with Alire.Properties;
with Alire.Releases;
with Alire.Root;
with Alire.Shared;
with Alire.TTY;
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
         Index.Detect_Externals (Crate, Root.Platform_Properties);

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

      -------------------
      -- Set_Toolchain --
      -------------------

      procedure Set_Toolchain (Crate : Crate_Name; Selection : Selections) is
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

            --  If the selected compiler is one of our regular indexed ones,
            --  install the compiler and a matching gprbuild, if existing.
            --  Also, store the version in our configuration for future
            --  reference. On the contrary, if the selection is from system
            --  packages or the environment, we need not to install anything.
            --  (We are not offering system packages, as only one gnat can
            --  be installed e.g. in Debian, and changing it would affect the
            --  whole system. We only offer external compilers detected in the
            --  environment.)

            Set_Up_Choice :
            declare
               Release : constant Releases.Release :=
                           Selection.Targets (Choice);
            begin

               --  Store tool version

               Config.Edit.Set (Path  => Config.Edit.Filepath (Config.Global),
                                Key   => Tool_Key (Crate),
                                Value => Release.Milestone.Image);

               --  Optionally, deploy as a shared install

               if Release.Origin.Is_Regular then
                  Shared.Share (Release);
               else
                  Trace.Debug
                    ("The user selected a external version as default");
               end if;
            end Set_Up_Choice;
         end if;
      end Set_Toolchain;

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
            Put_Info (Crate.TTY_Image & " is currently not configured. (alr "
                      & "will use the version found in the environment.)");
         end if;
         Trace.Info ("");

         --  Find the newest regular release in our index:
         if not Index.Releases_Satisfying (Any_Tool (Crate),
                                           Root.Platform_Properties).Is_Empty
         then
            Set_Toolchain (Crate, Fill_Version_Choices (Crate));
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
      Put_Info ("Welcome to the toolchain selection assistant");

      AAA.Text_IO.Put_Paragraphs
        (AAA.Strings.Empty_Vector
         .Append ("")
         .Append
           ("In this assistant you can set up the default toolchain to be "
            & "used in any crate that does not specify its own top-level "
            & "dependency on a version of GNAT or gprbuild.")
         .Append ("")
         .Append
           ("If you choose " & TTY.Italic ("""None""") & ", Alire will use "
            & "whatever version is found in the environment.")
         .Append ("")
         .Append
           ("You can re-run this assistant at any time using "
            & """alr install --toolchain"".")
        );

      for Tool of Tools loop
         Set_Up (Tool);
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

end Alire.Toolchains;
