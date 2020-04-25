with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

with Alire.Directories;
with Alire.GPR;
with Alire.Properties.Scenarios;
with Alire.Solver;
with Alire.Utils;

with Alr.Commands;
with Alr.OS_Lib;
with Alr.Platform;
with Alr.Paths;

package body Alr.Build_Env is

   package Query renames Alire.Solver;

   type Env_Var_Action_Callback is access procedure (Key, Val : String);

   Path_Separator : constant Character := GNAT.OS_Lib.Path_Separator;

   ------------------
   -- Project_Path --
   ------------------

   function Project_Path (Instance : Query.Instance;
                          Root     : Alire.Roots.Root)
                          return String
   is
      use Alr.OS_Lib;
      use Ada.Strings.Unbounded;

      Result       : Unbounded_String;
      All_Paths    : Alire.Utils.String_Vector;
      Sorted_Paths : Alire.Utils.String_Set;

      Working_Folder : constant Alire.Absolute_Path :=
        Alire.Directories.Current / Paths.Alr_Working_Folder;

      First : Boolean := True;
   begin
      --  First obtain all paths and then output them, if any needed
      for Rel of Instance.Including (Root.Release) loop
         if Rel.Name = Root.Release.Name then
            --  All_Paths.Append (".");
            null; -- That's the first path in aggregate projects anyway
         else
            All_Paths.Append (Working_Folder /
                                Paths.Alr_Working_Deps_Path /
                                  Rel.Unique_Folder);
         end if;

         --  Add non-root extra project paths, always
         for Path of Rel.Project_Paths (Platform.Properties) loop
            All_Paths.Append
              (Working_Folder /
                 (if Rel.Name = Root.Release.Name
                  then ".."
                  else Paths.Alr_Working_Deps_Path / Rel.Unique_Folder) /
                 Path);
         end loop;
      end loop;

      --  Sort and remove duplicates in paths (may come from extension
      --  projects).
      for Path of All_Paths loop
         Sorted_Paths.Include (Path);
      end loop;

      if not Sorted_Paths.Is_Empty then
         for Path of Sorted_Paths loop

            if First then
               Result := Result & Path;
               First := False;
            else
               Result := Result & Path_Separator & Path;
            end if;

         end loop;
      end if;

      return To_String (Result);
   end Project_Path;

   -------------
   -- Gen_Env --
   -------------

   procedure Gen_Env (Root     : Alire.Roots.Root;
                      Action   : not null Env_Var_Action_Callback)
   is
      Needed  : constant Query.Solution :=
                  Query.Resolve
                    (Root.Release.Dependencies.Evaluate (Platform.Properties),
                     Platform.Properties,
                     Options => (Age       => Commands.Query_Policy,
                                 Detecting => <>,
                                 Hinting   => <>));

      Existing_Project_Path : GNAT.OS_Lib.String_Access;

      Full_Instance : Query.Instance;
   begin
      if not Needed.Valid then
         raise Command_Failed;
      end if;

      --  GPR_PROJECT_PATH
      Existing_Project_Path := GNAT.OS_Lib.Getenv ("GPR_PROJECT_PATH");

      if Existing_Project_Path.all'Length = 0 then

         --  The variable is not already defined
         Action ("GPR_PROJECT_PATH", Project_Path (Needed.Releases, Root));
      else

         --  Append to the existing variable
         Action ("GPR_PROJECT_PATH",
                 Existing_Project_Path.all & Path_Separator &
                   Project_Path (Needed.Releases, Root));
      end if;

      GNAT.OS_Lib.Free (Existing_Project_Path);

      Full_Instance := Needed.Releases.Including (Root.Release);

      --  Externals
      --
      --  FIXME: what to do with duplicates? at a minimum research what
      --  gprbuild does (err, ignore...).
      for Release of Full_Instance loop
         for Prop of Release.On_Platform_Properties (Platform.Properties) loop
            if Prop in Alire.Properties.Scenarios.Property'Class then
               declare
                  use all type Alire.GPR.Variable_Kinds;
                  Variable : constant Alire.GPR.Variable :=
                    Alire.Properties.Scenarios.Property (Prop).Value;
               begin
                  if Variable.Kind = External then
                     Action (Variable.Name, Variable.External_Value);
                  end if;
               end;
            end if;
         end loop;
      end loop;

      Action ("ALIRE", "True");
   end Gen_Env;

   ---------------
   -- Print_Var --
   ---------------

   procedure Print_Var (Key, Value : String) is
   begin
      Ada.Text_IO.Put_Line ("export " & Key & "=""" & Value & """");
   end Print_Var;

   -------------
   -- Set_Var --
   -------------

   procedure Set_Var (Key, Value : String) is
   begin
      Alire.Trace.Detail ("Set environment variable: " &
                            Key & "=""" & Value & """");
      OS_Lib.Setenv (Key, Value);
   end Set_Var;

   ---------
   -- Set --
   ---------

   procedure Set (Root : Alire.Roots.Root)
   is
   begin
      Gen_Env (Root, Set_Var'Access);
   end Set;

   -----------
   -- Print --
   -----------

   procedure Print (Root : Alire.Roots.Root)
   is
   begin
      Gen_Env (Root, Print_Var'Access);
   end Print;

end Alr.Build_Env;
