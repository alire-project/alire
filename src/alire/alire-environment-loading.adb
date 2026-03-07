with Ada.Exceptions;

with Alire_Early_Elaboration;
with Alire.Formatting;
with Alire.GPR;
with Alire.Platforms.Current;
with Alire.Properties.Scenarios;
with Alire.Releases;
with Alire.Solutions;
with Alire.Toolchains.Solutions;
with Alire.Utils.TTY;

with GNAT.IO;

package body Alire.Environment.Loading is

   ----------
   -- Load --
   ----------

   Already_Warned : Boolean := False;

   procedure Load (This        : in out Context;
                   Root        : in out Alire.Roots.Root;
                   For_Hashing : Boolean := False)
   is
      Solution  : constant Solutions.Solution :=
                    Toolchains.Solutions.Add_Toolchain (Root.Solution);
      Tool_Root : Roots.Editable.Root :=
                    Roots.Editable.New_Root (Root);
      --  We use a copy of the base root to add the toolchain elements that
      --  might be missing from its solution
   begin
      Tool_Root.Set (Solution);

      --  Load platform environment
      Alire.Platforms.Current.Load_Environment (This);

      --  Warnings when setting up an incomplete environment

      if not Solution.Is_Complete then
         Trace.Debug ("Generating possibly incomplete environment"
                      & " because of missing dependencies");

         --  Normally we would generate a warning, but since that will pollute
         --  the output making it unusable, for once we write directly to
         --  stderr (unless quiet is in effect):

         if not Alire_Early_Elaboration.Switch_Q and then not Already_Warned
         then
            Already_Warned := True;

            GNAT.IO.Put_Line
              (GNAT.IO.Standard_Error,
               TTY.Warn ("warn:")
               & " Generating possibly incomplete environment"
               & " because of missing dependencies");
         end if;
      end if;

      --  Project paths for all releases in the solution, implicitly defined by
      --  supplied project files.

      if not For_Hashing then
         declare
            Sorted_Paths : constant AAA.Strings.Set :=
                             Tool_Root.Current.Project_Paths;
         begin
            if not Sorted_Paths.Is_Empty then
               for Path of reverse Sorted_Paths loop
                  --  Reverse should not matter as our paths shouldn't overlap,
                  --  but at least is nicer for user inspection to respect
                  --  alphabetical order.

                  This.Prepend ("GPR_PROJECT_PATH", Path, "crates");
               end loop;
            end if;
         end;
      end if;

      --  Custom definitions provided by each release

      for Rel of Solution.Releases.Including (Root.Release) loop
         Load (This        => This,
               Root        => Tool_Root,
               Crate       => Rel.Name,
               For_Hashing => For_Hashing);
      end loop;

      This.Set ("ALIRE", "True", "Alire");
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load (This            : in out Context;
                   Root            : in out Roots.Editable.Root;
                   Crate           : Crate_Name;
                   For_Hashing     : Boolean := False)
   is
      Env    : constant Properties.Vector := Root.Current.Environment;
      Rel    : constant Releases.Release := Root.Current.Release (Crate);
      Origin : constant String := Rel.Name_Str;

      Release_Base : constant String
        := (if For_Hashing
            then Rel.Base_Folder
            else Root.Current.Release_Base (Rel.Name, Roots.For_Build));
      --  Before we can known the Release_Base, we supplant it with its
      --  simple name. This shouldn't be a problem for hashing, as this
      --  is only used for $CRATE_ROOT paths, and the important parts
      --  that might merit a hash change are the rest of the path.
   begin
      Trace.Debug ("Loading environment for crate "
                   & Alire.Utils.TTY.Name (Crate)
                   & " release: " & Rel.Milestone.TTY_Image);

      --  Environment variables defined in the crate manifest
      for Act of Rel.Environment (Env) loop
         Trace.Debug ("Processing env entry: " & Act.Name
                      & " of type " & Act.Action'Image
                      & " with value " & Act.Value);
         begin
            declare
               Value : constant String :=
                         Formatting.Format
                           (Act.Value,
                            Formatting.For_Manifest_Environment (Release_Base),
                            Convert_Path_Seps => True);
            begin
               case Act.Action is

               when Properties.Environment.Set =>

                  This.Set (Act.Name, Value, Origin & " (env)");

               when Properties.Environment.Append =>

                  This.Append (Act.Name, Value, Origin & " (env)");

               when Properties.Environment.Prepend =>

                  This.Prepend (Act.Name, Value, Origin & " (env)");

               end case;
            end;
         exception
            when E : Formatting.Unknown_Formatting_Key =>
               Raise_Checked_Error
                 ("Unknown predefined variable in environment variable '" &
                    Act.Name & "' of '" & Origin & "': " &
                    Ada.Exceptions.Exception_Message (E));
         end;
      end loop;

      --  Environment variables for GPR external scenario variables
      for Property of Rel.On_Platform_Properties (Env) loop
         if Property in Alire.Properties.Scenarios.Property'Class then
            declare
               use all type Alire.GPR.Variable_Kinds;
               Variable : constant Alire.GPR.Variable
                 := Alire.Properties.Scenarios.Property (Property).Value;
            begin
               if Variable.Kind = External then
                  This.Set (Variable.Name, Variable.External_Value,
                            Origin & " (gpr ext)");
               end if;
            end;
         end if;
      end loop;

      --  Set the crate PREFIX location for access to resources
      This.Set (AAA.Strings.To_Upper_Case (+Rel.Name) & "_ALIRE_PREFIX",
                Release_Base,
                "Crate prefix for resources location");
   end Load;

end Alire.Environment.Loading;
