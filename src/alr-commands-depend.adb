with Alire.Conditional;
with Alire.Index;
with Alire.Releases;
with Alire.Roots;

with Alr.Files;
with Alr.Parsers;
with Alr.Platform;
with Alr.Root;
with Alr.Spawn;
with Alr.Templates;

package body Alr.Commands.Depend is

   ---------
   -- Add --
   ---------

   procedure Add is

      procedure Add (Request : String) is
         Requested : constant Parsers.Allowed_Milestones := Parsers.Project_Versions (Request);
      begin
         if not Query.Exists (Requested.Project) then
            Reportaise_Command_Failed ("The requested project was not found in the catalog: " & Request);
         end if;

         declare
            use all type Alire.Conditional.Dependencies;

            New_Root  : constant Alire.Roots.Root :=
                          Alire.Index.Set_Root
                            (Root.Project,
                             Root.Current.Dependencies and
                               Alire.Conditional.New_Dependency (Requested.Project, Requested.Versions))
              with Unreferenced;
         begin
            if not Query.Is_Resolvable (Root.Current.Dependencies.Evaluate (Platform.Properties)) then
               Reportaise_Command_Failed ("Adding " & Request & " has no dependency solution");
            else
               Trace.Detail ("Dependency " & Request & " successfully added");
            end if;
         end;
      end Add;

   begin
      for I in 2 .. Num_Arguments loop
         Add (Argument (I));
      end loop;

      --  Regenerate and update
      declare
         Success : Boolean;
         Needed  : constant Query.Instance :=
                     Query.Resolve (Root.Current.Dependencies.Evaluate (Platform.Properties),
                                    Success,
                                    Query_Policy);
      begin
         if not Success then
            Trace.Error ("depend add failed");
            raise Program_Error with "Reached unresolvable situation despite previous checks";
            --  This really should be impossible, given that we checked each new dependency adition
         end if;
         Templates.Generate_Prj_Alr (Needed, Root.Current, Templates.Unknown, Files.Locate_Metadata_File);
         Trace.Detail ("Regeneration finished, updating now");
      end;

      Spawn.Alr (Cmd_Update);
   end Add;

   ---------
   -- Del --
   ---------

   procedure Del is null;

   --------------------------
   -- Display_Help_Details --
   --------------------------

   overriding procedure Display_Help_Details (Cmd : Command) is
      pragma Unreferenced (Cmd);
   begin
      New_Line;
      Print_Project_Version_Sets;
   end Display_Help_Details;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);

      type Subcommands is (Add, Del);
   begin
      Requires_Project;

      if Num_Arguments < 1 then
         Reportaise_Wrong_Arguments ("Specify add/del subcommand and dependency");
      elsif Num_Arguments < 2 then
         Reportaise_Wrong_Arguments ("No project dependency given");
      end if;

      case Subcommands'Value (Argument (1)) is
         when Add =>
            Requires_Full_Index (Even_In_Session => True);
            Add;
         when Del =>
            Del;
      end case;
   end Execute;

end Alr.Commands.Depend;
