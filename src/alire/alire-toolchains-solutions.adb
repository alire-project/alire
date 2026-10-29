with AAA.Strings;

with Alire.Index;
with Alire.Origins;
with Alire.Releases.Containers;
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

      --  Detect tool mismatch. This is a good place as this is the last moment
      --  before a tool is really going to be used.

      Detect_Hash_Mismatch;

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

   ----------------------
   -- Environment_Tool --
   ----------------------

   function Environment_Tool (Tool : Crate_Name)
                              return Releases.Containers.Optional
   --  Return the external release of Tool detected in the environment, if any
   is
   begin
      Index.Detect_Externals (Tool, Root.Platform_Properties);

      if Tool = GNAT_Crate then
         return Releases.Containers.Unit
           (Solver.Find (GNAT_External_Crate,
                         Policy => Solver.Default_Options.Age));
      else
         return Releases.Containers.Unit
           (Solver.Find (Tool,
                         Policy  => Solver.Default_Options.Age,
                         Origins => (Alire.Origins.External => True,
                                     others                 => False)));
      end if;
   exception
      when Query_Unsuccessful =>
         return Releases.Containers.Optional_Releases.Empty;
   end Environment_Tool;

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
         Found : constant Releases.Containers.Optional :=
                   Environment_Tool (GNAT_Crate);
      begin
         if Found.Is_Empty then
            Raise_Checked_Error
              (Errors.New_Wrapper
               .Wrap ("Unable to determine compiler version.")
               .Wrap ("Check that the workspace solution is complete "
                 & "and a compiler is available.")
               .Get);
         end if;

         return Found.Value;
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

   ---------------
   -- Effective --
   ---------------

   function Effective (Solution : Alire.Solutions.Solution;
                       Tool     : Crate_Name)
                       return Effective_Tool
   is
      No_Deps : constant Alire.Dependencies.Containers.List :=
                  (Alire.Dependencies.Containers.Lists.Empty_List
                   with null record);

      ----------------
      -- Found_Tool --
      ----------------

      function Found_Tool (Source   : Tool_Sources;
                           Crate    : Crate_Name;
                           Version  : Semantic_Versioning.Version;
                           Provides : Alire.Dependencies.Containers.List :=
                                        No_Deps)
                           return Effective_Tool
        with Pre => Source /= Missing
      is
      begin
         return Result : Effective_Tool (Source, Crate.Len) do
            Result.Crate    := Crate;
            Result.Provides := Provides;
            Result.Version  := Version;
         end return;
      end Found_Tool;

      ------------------
      -- Missing_Tool --
      ------------------

      function Missing_Tool (Provides : Alire.Dependencies.Containers.List :=
                                          No_Deps)
                             return Effective_Tool
      is (Source   => Missing,
          Name_Len => Tool.Len,
          Crate    => Tool,
          Provides => Provides);

   begin
      if Solution.Depends_On (Tool) then

         --  All dependencies fulfilled by the tool release, possibly on
         --  different crates (e.g., gnat and gnat_cross_1) that the solver
         --  keeps separately.

         declare
            Providers : constant Alire.Solutions.State_Map :=
                          Solution.Dependencies_Providing (Tool);
            Provides : Alire.Dependencies.Containers.List;
         begin
            if Providers.Is_Empty then
               --  Unsolved dependency on the tool itself
               Provides.Append (Solution.Dependency (Tool));
               return Missing_Tool (Provides);
            end if;

            for State of Providers loop
               Provides.Append (State.As_Dependency);
            end loop;

            return Found_Tool (Dependencies,
                               Providers.First_Element.Release.Name,
                               Providers.First_Element.Release.Version,
                               Provides);
         end;

      elsif Tool_Is_Configured (Tool) then
         return Found_Tool
           ((if Settings.DB.Origin (Tool_Key (Tool))
               = Settings.Level'Image (Settings.Local)
            then Local
            else Global),
            Tool_Milestone (Tool).Crate,
            Tool_Milestone (Tool).Version);

      else
         declare
            Env_Tool : constant Releases.Containers.Optional :=
                         Environment_Tool (Tool);
         begin
            if Env_Tool.Has_Element then
               return Found_Tool (Environment,
                                  Env_Tool.Value.Name,
                                  Env_Tool.Value.Version);
            else
               return Missing_Tool;
            end if;
         end;
      end if;
   end Effective;

   ---------------------
   -- Is_In_Toolchain --
   ---------------------

   function Is_In_Toolchain (Release : Releases.Release) return Boolean
   is
      use type Alire.Dependencies.Dependency;
   begin
      return Tool_Is_Configured (Release.Name) and then
        Tool_Dependency (Release.Name) = Release.To_Dependency.Value;
   end Is_In_Toolchain;

end Alire.Toolchains.Solutions;
