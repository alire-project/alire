with AAA.Strings;
with AAA.Text_IO;

with Ada.Containers.Indefinite_Vectors;

with Alire.Config.Edit;
with Alire.Index;
with Alire.Origins;
with Alire.Releases;
with Alire.Root;
with Alire.TTY;
with Alire.Utils.User_Input;

package body Alire.Toolchains is

   ---------------
   -- Assistant --
   ---------------

   procedure Assistant is
      package Release_Vectors is new
        Ada.Containers.Indefinite_Vectors
          (Positive, Releases.Release, Releases."=");

      Choices  : Utils.String_Vector;
      Releases : Release_Vectors.Vector;
      --  These two variables are in sync; so the picked choice says the
      --  release to use at the same position in the respective vector.

      GNAT    : constant Crate_Name := To_Name ("gnat");

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
            Releases.Prepend (Release);
         else
            Choices.Append (Text);
            Releases.Append (Release);
         end if;
      end Add_Choice;

      --------------------------
      -- Fill_Version_Choices --
      --------------------------

      procedure Fill_Version_Choices is
         use all type Origins.Kinds;
      begin
         Index.Detect_Externals (GNAT, Root.Platform_Properties);

         --  Always offer to configure nothing
         Choices.Append ("None");
         Releases.Append (Index.Crate (GNAT).Base); -- Just a placeholder

         --  Identify possible externals first (but after the newest Alire one)
         for Release of reverse Index.Crate (GNAT).Releases loop
            if Release.Origin.Kind in System | External then
               Add_Choice (Release.Version.Image
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
            for Release of reverse Index.Crate (GNAT).Releases loop
               if Release.Origin.Is_Regular then
                  if First then
                     First := False;
                     Add_Choice (Release.Version.Image, Release,
                                 Prepend => True);
                  else
                     Add_Choice (Release.Version.Image, Release);
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

         else
            Put_Info ("Selected toolchain version "
                      & TTY.Bold (Releases (Choice).Version.Image));
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

      --  Find the newest regular gnat in our index:
      if Index.Exists (GNAT) then
         Fill_Version_Choices;
         Set_Toolchain;
      else
         Put_Warning ("There are no indexed GNAT versions in the catalog");
      end if;

      Config.Edit.Set (Config.Edit.Filepath (Config.Global),
                       Config.Keys.Toolchain_Assistant,
                       "false");
   end Assistant;

end Alire.Toolchains;
