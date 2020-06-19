with GNAT.IO;

with Alire_Early_Elaboration;

with Alire.GPR;
with Alire.Solutions;
with Alire.Solver;
with Alire.Utils.TTY;
with Alire.Utils;
with Alire.Environment;
with Alire.Properties;
with Alire.Releases;

with Alr.Platform;

package body Alr.Build_Env is

   package TTY renames Alire.Utils.TTY;

   ------------------
   -- Load_Context --
   ------------------

   procedure Load_Context (Ctx  : in out Alire.Environment.Context;
                           Root : Alire.Roots.Root)
   is
      Needed : constant Alire.Solver.Solution := Root.Solution;
   begin
      if not Needed.Is_Complete then
         Trace.Debug ("Generating incomplete environment"
                      & " because of missing dependencies");

         --  Normally we would generate a warning, but since that will pollute
         --  the output making it unusable, for once we write directly to
         --  stderr (unless quiet is in effect):

         if not Alire_Early_Elaboration.Switch_Q then
            GNAT.IO.Put_Line
              (GNAT.IO.Standard_Error,
               TTY.Warn ("warn:") & " Generating incomplete environment"
               & " because of missing dependencies");
         end if;
      end if;

      declare
         Sorted_Paths : constant Alire.Utils.String_Set := Root.Project_Paths;
      begin
         if not Sorted_Paths.Is_Empty then
            for Path of Sorted_Paths loop
               Ctx.Append ("GPR_PROJECT_PATH", Path, "crates");
            end loop;
         end if;
      end;

      for Rel of Needed.Releases.Including (Root.Release) loop
         Ctx.Load (Rel,
                   Platform.Properties,
                   Is_Root_Release => Rel.Name = Root.Release.Name);
      end loop;

      Ctx.Set ("ALIRE", "True", "Alire");
   end Load_Context;

   ------------
   -- Export --
   ------------

   procedure Export (Root : Alire.Roots.Root)
   is
      Ctx : Alire.Environment.Context;
   begin
      Load_Context (Ctx, Root);
      Ctx.Export;
   end Export;

   -------------------
   -- Print_Details --
   -------------------

   procedure Print_Details (Root : Alire.Roots.Root)
   is
      Ctx : Alire.Environment.Context;
   begin
      Load_Context (Ctx, Root);
      Ctx.Print_Details;
   end Print_Details;

   -----------------
   -- Print_Shell --
   -----------------

   procedure Print_Shell (Root : Alire.Roots.Root;
                          Kind : Alire.Platforms.Shells)
   is
      Ctx : Alire.Environment.Context;
   begin
      Load_Context (Ctx, Root);
      Ctx.Print_Shell (Kind);
   end Print_Shell;

end Alr.Build_Env;
