with AAA.Strings;

with Alire.Index;
with Alire.Root;

package body Alire.Toolchains.Solutions is

   -------------------
   -- Add_Toolchain --
   -------------------

   function Add_Toolchain (Solution : Alire.Solutions.Solution)
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
            if not Tool_Is_External (Tool) then
               Redeploy_If_Needed (Tool_Milestone (Tool));
            end if;

            --  Add the configured tool release to the solution
            Result := Result.Including
              (Release        => Toolchains.Release
                 (Target           => Tool_Milestone (Tool),
                  Detect_Externals => Tool_Is_External (Tool)),
               Env            => Root.Platform_Properties,
               Add_Dependency => True,
               Shared         => True);
         else
            Trace.Debug ("Toolchain environment: tool not in solution nor "
                         & "defined by the user: " & Tool.TTY_Image);
         end if;
      end loop;

      return Result;
   end Add_Toolchain;

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
