with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Alire.Conditional;
with Alire.Index;
with Alire.Releases;
with Alire.Roots;

with Alr.Hardcoded;
with Alr.Parsers;
with Alr.Platform;
with Alr.Root;
with Alr.Spawn;
with Alr.Templates;

with GNAT.Command_Line;

with Semantic_Versioning;

package body Alr.Commands.Depend is

   ---------
   -- Add --
   ---------

   function Add (Deps : Alire.Conditional.Dependencies; New_Dep : String) return Alire.Conditional.Dependencies
   is
      use all type Alire.Conditional.Dependencies;
      Requested : constant Parsers.Allowed_Milestones := Parsers.Project_Versions (New_Dep);
   begin
      if not Query.Exists (Requested.Project) then
         Reportaise_Command_Failed ("The requested project was not found in the catalog: " & (+Requested.Project));
      end if;

      return Result : constant Alire.Conditional.Dependencies :=
        Deps and Alire.Conditional.New_Dependency (Requested.Project, Requested.Versions)
      do
         if not Query.Is_Resolvable (Result.Evaluate (Platform.Properties)) then
            Reportaise_Command_Failed ("Adding " & New_Dep & " has no dependency solution");
         else
            Trace.Detail ("Dependency " & New_Dep & " successfully added");
         end if;
      end return;
   end Add;

   ---------
   -- Del --
   ---------

   function Del (Deps : Alire.Conditional.Dependencies; Old_Dep : String) return Alire.Conditional.Dependencies
   is
      use all type Alire.Conditional.Dependencies;
      use all type Semantic_Versioning.Version_Set;
      Requested : constant Parsers.Allowed_Milestones := Parsers.Project_Versions (Old_Dep);
   begin
      if Requested.Versions /= Semantic_Versioning.Any then
         Trace.Warning ("Version is not used when removing dependencies: " & Old_Dep);
      end if;

      --  Iterate over actual dependencies and remove any matching the given
      return Filtered : Alire.Conditional.Dependencies do
         declare
            procedure Check (Dep : Alire.Conditional.Dependencies) is
               use all type Alire.Conditional.For_Dependencies.Kinds;
            begin
               case Dep.Kind is
                  when Condition =>
                     Trace.Warning ("Skipping unsupported conditional dependency");
                  when Value => -- A value is a vector of dependencies!
                     for E of Dep.Value loop
                        if E.Project /= Requested.Project then
                           Filtered := Filtered and Alire.Conditional.New_Dependency (E.Project, E.Versions);
                        end if;
                     end loop;
                  when Vector =>
                     raise Program_Error with "Shouldn't happen";
               end case;
            end Check;
         begin
            Deps.Iterate_Children (Check'Access);
         end;
      end return;
   end Del;

   ---------------------
   -- Replace_Current --
   ---------------------

   procedure Replace_Current (Deps : Alire.Conditional.Dependencies) is
   begin
      --  Set, regenerate and update
      declare
         New_Root  : constant Alire.Roots.Root := Alire.Index.Set_Root (Root.Project, Deps) with Unreferenced;
         Success   : Boolean;
         Needed    : constant Query.Instance :=
                       Query.Resolve (Deps.Evaluate (Platform.Properties),
                                      Success,
                                      Query_Policy);
      begin
         if not Success then
            Trace.Error ("depend failed");
            raise Program_Error with "Reached unresolvable situation despite previous checks";
            --  This really should be impossible, given that we checked each new dependency adition
         end if;
         Templates.Generate_Prj_Alr (Needed, Root.Current, Templates.Unknown, Hardcoded.Working_Deps_File);
         Trace.Detail ("Regeneration finished, updating now");
      end;

      Spawn.Alr (Cmd_Update);
   end Replace_Current;

   ---------
   -- Add --
   ---------

   procedure Add is
      Deps : Alire.Conditional.Dependencies := Root.Current.Dependencies;
   begin
      for I in 1 .. Num_Arguments loop
         Deps := Add (Deps, Argument (I));
      end loop;

      Replace_Current (Deps);
   end Add;

   ---------
   -- Del --
   ---------

   procedure Del is
      Deps : Alire.Conditional.Dependencies := Root.Current.Dependencies;
   begin
      for I in 1 .. Num_Arguments loop
         Deps := Del (Deps, Argument (I));
      end loop;

      Replace_Current (Deps);
   end Del;

   ----------
   -- From --
   ----------

   procedure From is
      use Ada.Text_IO;
      use Utils;

      Deps : Alire.Conditional.Dependencies;

      -------------
      -- Extract --
      -------------

      procedure Extract (Line : String) is
         use Ada.Strings;
         use Ada.Strings.Fixed;
         use Ada.Strings.Maps;
         --  Line contains "alr depend", is crunched and lowercased
         First, Last : Natural := Line'First - 1;

         Found : Boolean := False;
      begin
         loop
            Find_Token (Line, To_Set (' '),
                        From => Last + 1,
                        Test => Outside,
                        First => First,
                        Last  => Last);

            exit when First > Line'Last;

            if Found then
               Deps := Add (Deps, Line (First .. Last));
            elsif Line (First .. Last) = "--add" then
               Found := True;
            end if;

            exit when Last = Line'Last;
         end loop;
      end Extract;

      ----------------
      -- Check_File --
      ----------------

      procedure Check_File (Name : String) is
         File : File_Type;
      begin
         if not OS_Lib.Is_Regular_File (Name) then
            Reportaise_Command_Failed ("Given file not found: " & Name);
         end if;

         Open (File, In_File, Name);

         while not End_Of_File (File) loop
            declare
               Line : constant String := Crunch (To_Lower_Case (Get_Line (File)));
            begin
               exit when Contains (Line, "project");

               if Contains (Line, "alr depend") then
                  Extract (Line);
               end if;
            end;
         end loop;

         Close (File);
      end Check_File;
   begin
      for I in 1 .. Num_Arguments loop
         Check_File (Argument (I));
      end loop;

      Replace_Current (Deps);
   end From;

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
   begin
      Requires_Project;

      if not (Cmd.Add or else Cmd.Del or else Cmd.From) then
         Reportaise_Wrong_Arguments ("Exactly one of --add, --del, --from is required");
      end if;

      if (Cmd.Add and Cmd.Del) or (Cmd.Add and Cmd.From) or (Cmd.Del and Cmd.From) then
         Reportaise_Wrong_Arguments ("Exactly one of --add, --del, --from is required");
      end if;

      if Num_Arguments < 1 then
         if Cmd.Add or Cmd.Del then
            Reportaise_Wrong_Arguments ("At least one dependency required");
         else
            Reportaise_Wrong_Arguments ("At least one GPR file to process required");
         end if;
      end if;

      if Cmd.Add then
         Requires_Full_Index (Even_In_Session => True);
         Add;
      elsif Cmd.Del then
         Del;
      else
         From;
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Add'Access,
                     "", "--add",
                     "Add given dependencies");

      Define_Switch (Config,
                     Cmd.Del'Access,
                     "", "--del",
                     "Remove given dependencies");

      Define_Switch (Config,
                     Cmd.From'Access,
                     "", "--from",
                     "Use dependencies declared within GPR project file");
   end Setup_Switches;

end Alr.Commands.Depend;
