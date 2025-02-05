with AAA.Strings;

with Alire.Index;
with Alire.Root;
with Alire.Solver;

package body Alire.Toolchains.Solutions is

   -------------------
   -- Add_Toolchain --
   -------------------

   function Add_Toolchain (Solution : Alire.Solutions.Solution;
                           Deploy   : Boolean := True)
                           return Alire.Solutions.Solution
   is

      ------------------------
      -- Redeploy_If_Needed --
      ------------------------

      procedure Redeploy_If_Needed (Mil : Milestones.Milestone) is
         use type Milestones.Milestone;
      begin
         --  Check that is not already there
         if (for some Rel of Toolchains.Available => Rel.Milestone = Mil) then
            return;
         end if;

         --  It must be redeployed
         Put_Warning ("Tool " & Mil.TTY_Image & " is missing, redeploying...");
         Toolchains.Deploy (Index.Find (Mil.Crate, Mil.Version));
      end Redeploy_If_Needed;

      Result : Alire.Solutions.Solution := Solution;
   begin

      --  Last-minute redeployment of any missing toolchain element. This may
      --  happen if the user has manually deleted the cache of toolchains, or
      --  uninstalled a system package for the external compiler.

      Toolchains.Deploy_Missing;

      --  For every tool in the toolchain that does not appear in the solution,
      --  we will insert the user-configured tool, if any.

      for Tool of Toolchains.Tools loop
         if Solution.Depends_On (Tool) then
            Trace.Debug
              ("Toolchain environment: solution already depends on "
               & Solution.State (Tool).TTY_Image);
         elsif Toolchains.Tool_Is_Configured (Tool) then

            --  This shouldn't happen normally, but it can happen if the user
            --  has just changed the cache location.
            if Deploy and then not Tool_Is_External (Tool) then
               Redeploy_If_Needed (Tool_Milestone (Tool));
            end if;

            --  Add the configured tool release to the solution
            Result := Result.Including
              (Release        => Toolchains.Release
                 (Target           => Tool_Milestone (Tool),
                  Detect_Externals => Tool_Is_External (Tool)),
               Env            => Root.Platform_Properties,
               Add_Dependency => True);
         else
            Trace.Debug ("Toolchain environment: tool not in solution nor "
                         & "defined by the user: " & Tool.TTY_Image);
         end if;
      end loop;

      return Result;
   end Add_Toolchain;

   --------------
   -- Compiler --
   --------------

   function Compiler (Solution : Alire.Solutions.Solution)
                      return Releases.Release
   is

      --------------------------
      -- Environment_Compiler --
      --------------------------

      function Environment_Compiler return Releases.Release is
      begin
         Index.Detect_Externals (GNAT_Crate, Root.Platform_Properties);
         return Solver.Find (GNAT_External_Crate,
                             Policy => Solver.Default_Options.Age);
      exception
         when Query_Unsuccessful =>
            Raise_Checked_Error
              (Errors.New_Wrapper
               .Wrap ("Unable to determine compiler version.")
               .Wrap ("Check that the workspace solution is complete "
                 & "and a compiler is available.")
               .Get);
      end Environment_Compiler;

   begin
      if not Solution.Depends_On (GNAT_Crate) then
         declare
            With_GNAT : constant Alire.Solutions.Solution :=
                          Add_Toolchain (Solution,
                                         Deploy => False);
         begin
            if not With_GNAT.Depends_On (GNAT_Crate) then
               --  This means that no compiler (or None) has been selected
               --  with `alr toolchain`, so we return whichever one is in
               --  the environment.
               return Environment_Compiler;
            else
               return Compiler (With_GNAT);
            end if;
         end;
      end if;

      --  At this point we have a GNAT in the solution

      Assert (Solution.Releases_Providing (GNAT_Crate).Length in 1,
              "Solution contains more than one compiler?");

      return Solution.Releases_Providing (GNAT_Crate).First_Element;
   end Compiler;

   ---------------------
   -- Is_In_Toolchain --
   ---------------------

   function Is_In_Toolchain (Release : Releases.Release) return Boolean
   is
      use type Dependencies.Dependency;
   begin
      return Tool_Is_Configured (Release.Name) and then
        Tool_Dependency (Release.Name) = Release.To_Dependency.Value;
   end Is_In_Toolchain;

end Alire.Toolchains.Solutions;
