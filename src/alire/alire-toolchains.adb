with AAA.Strings;
with AAA.Text_IO;

with Ada.Containers.Indefinite_Vectors;

with Alire.Config.Edit;
with Alire.Index;
with Alire.Milestones;
with Alire.Origins;
with Alire.Properties;
with Alire.Releases;
with Alire.Root;
with Alire.Shared;
with Alire.TTY;
with Alire.Utils.User_Input;

with Semantic_Versioning.Extended;

package body Alire.Toolchains is

   ------------------
   -- Add_Compiler --
   ------------------

   function Add_Compiler (Deps : Conditional.Dependencies)
                          return Conditional.Dependencies
   is
      use type Conditional.Dependencies;
      use Utils;
   begin

      --  We do not add a dependency if no compiler is configured (old alr
      --  working mode) or if there is a top-level specific dependency.

      if not Compiler_Is_Configured
        or else
        (for some Dep of Conditional.Enumerate (Deps) =>
             Starts_With (Dep.Crate.As_String, "gnat_"))
      then
         Trace.Debug ("Not adding compiler dependency (compiler configured: "
                      & Compiler_Is_Configured'Image & ")");
         return Deps;
      else
         Trace.Debug ("Adding compiler dependency on "
                      & Compiler_Dependency.TTY_Image);
         return Conditional.New_Dependency (Compiler_Dependency) and Deps;
      end if;
   end Add_Compiler;

   --------------
   -- Any_GNAT --
   --------------

   function Any_GNAT return Dependencies.Dependency
   is (Dependencies.New_Dependency
        (GNAT_Crate, Semantic_Versioning.Extended.Any));

   ---------------
   -- Assistant --
   ---------------

   procedure Assistant (Current_OS : Platforms.Operating_Systems) is
      package Release_Vectors is new
        Ada.Containers.Indefinite_Vectors
          (Positive, Releases.Release, Releases."=");

      Choices : Utils.String_Vector;
      Targets : Release_Vectors.Vector;
      --  These two variables are in sync; so the picked choice says the
      --  release to use at the same position in the respective vector.

      GPRbuild : constant Crate_Name := To_Name ("gprbuild");

      ----------------
      -- Add_Choice --
      ----------------

      procedure Add_Choice (Text    : String;
                            Release : Alire.Releases.Release;
                            Prepend : Boolean := False)
      is
      begin
         if Prepend then
            Choices.Prepend (Text);
            Targets.Prepend (Release);
         else
            Choices.Append (Text);
            Targets.Append (Release);
         end if;
      end Add_Choice;

      --------------------------
      -- Fill_Version_Choices --
      --------------------------

      procedure Fill_Version_Choices is
         use all type Origins.Kinds;
         Env : constant Properties.Vector := Root.Platform_Properties;
      begin
         Index.Detect_Externals (GNAT_Crate, Root.Platform_Properties);

         --  Always offer to configure nothing
         Choices.Append ("None");
         Targets.Append (Index.Crate (GNAT_Crate).Base); -- Just a placeholder

         --  Identify possible externals first (but after the newest Alire one)
         for Release of reverse Index.Releases_Satisfying (Any_GNAT, Env) loop
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
            use Utils;
            First : Boolean := True;
         begin
            for Release of reverse Index.Releases_Satisfying (Any_GNAT, Env)
            loop
               if Release.Origin.Is_Regular then

                  --  We want the newest native compiler to be the default.
                  --  This is a crudish check in which the compiler crate name
                  --  must contain our current platform name, since there's
                  --  nothing in the manifest saying if the compiler is native
                  --  or not.

                  if First
                    and then Contains
                      (Release.Name.As_String,
                       To_Lower_Case (Current_OS'Image))
                  then
                     First := False;
                     Add_Choice (Release.Milestone.TTY_Image, Release,
                                 Prepend => True);
                  else
                     Add_Choice (Release.Milestone.TTY_Image, Release);
                  end if;
               end if;
            end loop;
         end Add_Binary_Versions;
      end Fill_Version_Choices;

      -------------------
      -- Set_Toolchain --
      -------------------

      procedure Set_Toolchain is
         Choice : constant Positive :=
                    Utils.User_Input.Query_Multi
                      (Question  =>
          "Please select the compiler version for use with this configuration",
                       Choices   => Choices);
      begin
         if Choices (Choice) = "None" then

            Put_Info ("Selected to rely on a user-provided toolchain.");

            --  Clean up stored version

            Config.Edit.Unset (Path  => Config.Edit.Filepath (Config.Global),
                               Key   => Config.Keys.Toolchain_Default);

         else

            Put_Info ("Selected toolchain version "
                      & TTY.Bold (Targets (Choice).Milestone.TTY_Image));

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
               GNAT_Release : constant Releases.Release := Targets (Choice);
            begin

               --  Store toolchain

               Config.Edit.Set (Path  => Config.Edit.Filepath (Config.Global),
                                Key   => Config.Keys.Toolchain_Default,
                                Value => GNAT_Release.Milestone.Image);

               --  Optionally, deploy as a shared install

               if GNAT_Release.Origin.Is_Regular then
                  Shared.Share (GNAT_Release);

                  if Index.Exists (GPRbuild) then
                     if Index.Exists (GPRbuild, GNAT_Release.Version) then
                        Shared.Share
                          (Index.Find (GPRbuild, GNAT_Release.Version));
                     else
                        Put_Warning
                          ("No matching GPRbuild version in the catalog.");
                     end if;
                  else
                     Put_Warning
                       ("No indexed GPRbuild versions in the catalog.");
                  end if;

               else
                  Trace.Debug ("The user selected a external GNAT as default");
               end if;
            end Set_Up_Choice;
         end if;
      end Set_Toolchain;

   begin
      Put_Info ("Welcome to the toolchain selection assistant");

      AAA.Text_IO.Put_Paragraphs
        (AAA.Strings.Empty_Vector
         .Append ("")
         .Append
           ("In this assistant you can set up the default compiler to be "
            & "used in any crate that does not specify its own top-level "
            & "dependency on a version of GNAT.")
         .Append ("")
         .Append
           ("If you choose " & TTY.Italic ("""None""") & ", Alire will use "
            & "whatever GNAT version is found in the environment.")
         .Append ("")
         .Append
           ("You can re-run this assistant at any time using "
            & """alr install --toolchain"".")
         .Append (""));

      if Compiler_Is_Configured then
         Put_Info ("Compiler currently in use: "
                   & Compiler_Dependency.TTY_Image);
         Trace.Info ("");
      end if;

      --  Find the newest regular gnat in our index:
      if not Index.Releases_Satisfying (Any_GNAT,
                                        Root.Platform_Properties).Is_Empty
      then
         Fill_Version_Choices;
         Set_Toolchain;
      else
         Put_Warning ("No indexed GNAT versions in the catalog");
      end if;

      Config.Edit.Set (Config.Edit.Filepath (Config.Global),
                       Config.Keys.Toolchain_Assistant,
                       "false");
   end Assistant;

   ------------------------------
   -- Compiler_Milestone_Image --
   ------------------------------
   --  The string we use to store the compiler version
   function Compiler_Milestone_Image return String
   is (Config.Get (Config.Keys.Toolchain_Default, ""));

   ----------------------------
   -- Compiler_Is_Configured --
   ----------------------------

   function Compiler_Is_Configured return Boolean
   is (Compiler_Milestone_Image /= "");

   -------------------------
   -- Compiler_Dependency --
   -------------------------

   function Compiler_Dependency return Dependencies.Dependency
   is (Dependencies.New_Dependency
       (Milestones.New_Milestone (Compiler_Milestone_Image)));

end Alire.Toolchains;
