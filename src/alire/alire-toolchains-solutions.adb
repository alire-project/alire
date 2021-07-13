with Alire.Root;
with Alire.Shared;

package body Alire.Toolchains.Solutions is

   -------------------
   -- Add_Toolchain --
   -------------------

   function Add_Toolchain (Solution : Alire.Solutions.Solution)
                              return Alire.Solutions.Solution
   is
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
            Result := Result.Including
              (Release        => Shared.Release
                 (Tool_Milestone (Tool)),
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

end Alire.Toolchains.Solutions;
