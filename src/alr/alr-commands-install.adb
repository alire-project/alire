with AAA.Table_IO;

with Alire.Config.Edit;
with Alire.Dependencies;
with Alire.Errors;
with Alire.Releases;
with Alire.Shared;
with Alire.Solver;
with Alire.Utils;

package body Alr.Commands.Install is

   -------------
   -- Install --
   -------------

   procedure Install (Cmd : in out Command; Request : String) is
      use Alire;
   begin
      Cmd.Requires_Full_Index;

      Installation :
      declare
         Dep : constant Dependencies.Dependency :=
                 Dependencies.From_String (Request);
         Rel : constant Releases.Release :=
                 Solver.Find (Name    => Dep.Crate,
                              Allowed => Dep.Versions,
                              Policy  => Query_Policy);
      begin

         --  Inform of how the requested crate has been narrowed down

         if not Alire.Utils.Starts_With (Dep.Versions.Image, "=") then
            Put_Info ("Requested crate resolved as "
                      & Rel.Milestone.TTY_Image);
         end if;

         --  And perform the actual installation

         Shared.Share (Rel);

      end Installation;

   exception
      when E : Alire.Query_Unsuccessful =>
         Alire.Log_Exception (E);
         Trace.Error (Alire.Errors.Get (E));
   end Install;

   ----------
   -- List --
   ----------

   procedure List (Unused_Cmd : in out Command) is
      Table : AAA.Table_IO.Table;
   begin
      if Alire.Shared.Available.Is_Empty then
         Trace.Info ("Nothing installed in configuration prefix "
                     & TTY.URL (Alire.Config.Edit.Path));
         return;
      end if;

      Table.Append (TTY.Emph ("CRATE")).Append (TTY.Emph ("VERSION")).New_Row;

      for Dep of Alire.Shared.Available loop
         Table
           .Append (TTY.Name (Dep.Name))
           .Append (TTY.Version (Dep.Version.Image))
           .New_Row;
      end loop;

      Table.Print;
   end List;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command) is
   begin

      --  Validation

      if Num_Arguments > 1 then
         Reportaise_Wrong_Arguments
           ("One crate with optional version expected: crate[version set]");
      end if;

      --  Dispatch to subcommands

      if Num_Arguments = 1 then
         Cmd.Install (Argument (1));
      else
         Cmd.List;
      end if;

   end Execute;

end Alr.Commands.Install;
