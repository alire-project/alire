with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Alire.Conditional;
with Alire.Roots;
with Alire.Utils;

with Alr.Commands.Update;
with Alr.Exceptions;
with Alr.OS_Lib;
with Alr.Parsers;
with Alr.Platform;
with Alr.Root;
with Alr.Templates;

with Semantic_Versioning;

package body Alr.Commands.Withing is

   ---------
   -- Add --
   ---------

   function Add (Deps    : Alire.Conditional.Dependencies;
                 New_Dep : String)
                 return Alire.Conditional.Dependencies
   is
      use all type Alire.Conditional.Dependencies;
      Requested : constant Parsers.Allowed_Milestones :=
        Parsers.Project_Versions (New_Dep);
   begin
      if not Query.Exists (Requested.Project) then
         Reportaise_Command_Failed
           ("The requested project was not found in the catalog: " &
            (+Requested.Project));
      end if;

      return Result : constant Alire.Conditional.Dependencies :=
        Deps and Alire.Conditional.New_Dependency (Requested.Project,
                                                   Requested.Versions)
      do
         if not Query.Is_Resolvable (Result.Evaluate (Platform.Properties))
         then
            Reportaise_Command_Failed ("Adding " & New_Dep &
                                         " has no dependency solution");
         else
            Trace.Detail ("Dependency " & New_Dep & " successfully added");
         end if;
      end return;
   end Add;

   ---------
   -- Del --
   ---------

   function Del (Deps    : Alire.Conditional.Dependencies;
                 Old_Dep : String)
                 return Alire.Conditional.Dependencies
   is
      use all type Alire.Conditional.Dependencies;
      use all type Semantic_Versioning.Version_Set;
      Requested : constant Parsers.Allowed_Milestones :=
        Parsers.Project_Versions (Old_Dep);
   begin
      if Requested.Versions /= Semantic_Versioning.Any then
         Trace.Warning
           ("Version is not used when removing dependencies: " & Old_Dep);
      end if;

      --  Iterate over actual dependencies and remove any matching the given
      return Filtered : Alire.Conditional.Dependencies do
         declare
            procedure Check (Dep : Alire.Conditional.Dependencies) is
               use all type Alire.Conditional.For_Dependencies.Kinds;
            begin
               case Dep.Kind is
                  when Condition =>
                     Trace.Warning
                       ("Skipping unsupported conditional dependency");
                  when Value => -- A value is a vector of dependencies!
                     if Dep.Value.Project /= Requested.Project then
                        Filtered := Filtered and
                          Alire.Conditional.New_Dependency
                            (Dep.Value.Project, Dep.Value.Versions);
                     end if;
                  when Vector =>
                     for I in Dep.Iterate loop
                        Check (Dep (I));
                     end loop;
               end case;
            end Check;

            use all type Alire.Conditional.For_Dependencies.Kinds;
         begin
            case Deps.Kind is
               when Vector =>
                  Deps.Iterate_Children (Check'Access);
               when Value =>
                  Check (Deps);
               when Condition =>
                  raise Program_Error with "Should not happen";
            end case;
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
         New_Root  : constant Alire.Roots.Root :=
           Alire.Roots.New_Root
             (Root.Current.Release.Replacing (Dependencies => Deps),
              Root.Current.Path);
      begin
         Templates.Generate_Prj_Alr (New_Root.Release,
                                     New_Root.Crate_File);
         Trace.Detail ("Regeneration finished, updating now");
      end;

      Commands.Update.Execute;
   end Replace_Current;

   ---------
   -- Add --
   ---------

   procedure Add is
      Deps : Alire.Conditional.Dependencies :=
        Root.Current.Release.Dependencies;
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
      Deps : Alire.Conditional.Dependencies :=
        Root.Current.Release.Dependencies;
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
         --  Line contains "alr with", is crunched and lowercased
         First, Last : Natural := Line'First - 1;

         type Found_Steps is (Nothing, Dashes, Alr, Withh);
         Found : Found_Steps := Nothing;
      begin
         loop
            Find_Token (Line, To_Set (' '),
                        From  => Last + 1,
                        Test  => Outside,
                        First => First,
                        Last  => Last);

            exit when First > Line'Last;

            case Found is
               when Nothing =>
                  if Line (First .. Last) = "--" then
                     Found := Dashes;
                  end if;
               when Dashes =>
                  if Line (First .. Last) = "alr" then
                     Found := Alr;
                  end if;
               when Alr =>
                  if Line (First .. Last) = "with" then
                     Found := Withh;
                  end if;
               when Withh =>
                  Deps := Add (Deps, Line (First .. Last));
            end case;

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
               Line : constant String :=
                 Crunch (To_Lower_Case (Get_Line (File)));
            begin
               exit when Contains (Line, "project");

               if Contains (Line, "alr with") then
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

   ----------
   -- List --
   ----------

   procedure List is
   begin
      Root.Current.Release.Dependencies (Platform.Properties).Print;
   end List;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      Requires_Project;

      --  No parameters: give current platform dependencies and BAIL OUT
      if not (Cmd.Del or else Cmd.From) and then Num_Arguments = 0 then
         List;
         return;
      end if;

      if Cmd.Del and Cmd.From then
         Reportaise_Wrong_Arguments
           ("Simultaneous --del, --from are incompatible");
      end if;

      if Num_Arguments < 1 then
         if Cmd.Del then
            Reportaise_Wrong_Arguments ("At least one dependency required");
         elsif Cmd.From then
            Reportaise_Wrong_Arguments
              ("At least one GPR file to process required");
         end if;
      end if;

      if not (Cmd.Del or else Cmd.From) and then Num_Arguments > 0 then
         Requires_Full_Index;
         Add;
      elsif Cmd.Del then
         Del;
      elsif Cmd.From then
         Requires_Full_Index;
         From;
      else
         raise Program_Error with "List should have already happended";
      end if;
   exception
      when E : Constraint_Error =>
         Exceptions.Report ("In Withing.Execute:", E);
         Reportaise_Command_Failed
           ("Could not locate package containing releases of " & Argument (1));
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector
   is (Alire.Utils.Empty_Vector
       .Append ("Manages dependencies of the current crate or sandbox.")
       .New_Line
       .Append ("* From the command line:")
       .Append ("Dependencies are added by giving their name, and removed"
                & " by using the --del flag. Dependencies cannot be"
                & " simultaneously added and removed in a single invocation.")
       .New_Line
       .Append ("* From a GPR file:")
       .Append ("The project file given with --from will be scanned looking"
                & " for comments that contain the sequence 'alr with'. "
                & " These will be processed individually as if they had been"
                & " given in the command line, starting with no dependencies."
                & " That is, only dependencies given in the GPR file will be"
                & " preserved.")
       .New_Line
       .Append ("Example of GPR file contents:")
       .New_Line
       .Append ("with ""libhello""; -- alr with libhello")
       .New_Line
       .Append ("* Caveat:")
       .Append ("Since alr does not modify user files, any dependencies"
                & " managed through this command only directly affect"
                & " the metadata files of alr itself. In order to use the"
                & " dependencies in Ada code, the user *must* add the needed"
                & " 'with'ed project files in their own GPR files.")
       .New_Line
       .Append (Project_Version_Sets));

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
                     Cmd.Del'Access,
                     "", "--del",
                     "Remove given dependencies");

      Define_Switch (Config,
                     Cmd.From'Access,
                     "", "--from",
                     "Use dependencies declared within GPR project file");
   end Setup_Switches;

end Alr.Commands.Withing;
