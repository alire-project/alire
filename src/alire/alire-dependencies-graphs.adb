with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Utils.Tables;
with Alire.Utils.Vectors;

package body Alire.Dependencies.Graphs is

   -----------------
   -- Empty_Graph --
   -----------------

   function Empty_Graph return Graph is
      (Dep_Sets.Empty_Set with null record);

   -------------------
   -- From_Instance --
   -------------------

   function From_Solution (Sol : Solutions.Solution;
                           Env : Properties.Vector) return Graph is
   begin
      return Result : Graph do
         for Rel of Sol.Releases loop
            Result := Result.Including (Rel, Env);
         end loop;

         Result := Result.Filtering_Unused (Sol.Crates);
      end return;
   end From_Solution;

   ---------------
   -- Including --
   ---------------

   function Including (This : Graph;
                       R    : Releases.Release;
                       Env  : Properties.Vector)
                       return Graph
   is
   begin
      return Result : Graph := This do
         for Dep of R.Flat_Dependencies (Env) loop
            Result.Include (New_Dependency (R.Name, Dep.Crate));
         end loop;
      end return;
   end Including;

   ----------------------
   -- Filtering_Unused --
   ----------------------

   function Filtering_Unused (This : Graph;
                              Used : Alire.Containers.Crate_Name_Sets.Set)
                              return Graph
   is
   begin
      return Result : Graph do
         for Dep of This loop
            if Used.Contains (+Dep.Dependee) then
               Result.Include (Dep);
            end if;
         end loop;
      end return;
   end Filtering_Unused;

   ----------------------
   -- Has_Dependencies --
   ----------------------

   function Has_Dependencies (This : Graph;
                              Crate : Alire.Crate_Name)
                              return Boolean
   is
      package Graphs
      is new
         Alire.Utils.Vectors.Indefinite (
            Index_Type   => Positive,
            Element_Type => Alire.Crate_Name,
            Container    => Graph);
   begin
      return
         Graphs.Contains (
            Container => This,
            Item      => Crate);
   --   for Dep of This loop
   --      if +Dep.Dependent = Crate then
   --         return True;
   --      end if;
   --   end loop;
   --
   --   return False;
   end Has_Dependencies;

   -----------
   -- Label --
   -----------

   function Label_Dependee (Dependent : Crate_Name;
                            Dependee  : Crate_Name;
                            Solution  : Solutions.Solution;
                            For_Plot  : Boolean)
                            return String
   --  Get the proper label in the graph for a crate: milestone for releases,
   --  dependency for hints.
   is
   begin

      --  For a solved dependency, return "crate=version (original dependency)"
      --  For an unsolved dependency, return "crate^dependency". In the case of
      --  For_Plot, omit the (original dependency).

      if Solution.State (Dependee).Has_Release then
         if For_Plot then
            return Solution.State (Dependee).Milestone_Image (Color => False);
         else
            return Solution.State (Dependee).Milestone_Image
              & " ("
              & TTY.Version
               (Solution.Dependency (Dependent, Dependee).Versions.Image)
              & ")";
         end if;
      else
         if For_Plot then
            return Solution.Dependency (Dependent, Dependee).Image;
         else
            return Solution.Dependency (Dependent, Dependee).TTY_Image;
         end if;
      end if;
   end Label_Dependee;

   ---------------------
   -- Label_Dependent --
   ---------------------

   function Label_Dependent (Crate    : Crate_Name;
                             Solution : Solutions.Solution;
                             TTY      : Boolean := False)
                             return String
   is (if TTY then
          Solution.State (Crate).Release.Milestone.TTY_Image
       else
          Solution.State (Crate).Release.Milestone.Image);

   ----------
   -- Plot --
   ----------

   procedure Plot (This     : Graph;
                   Solution : Solutions.Solution)
   is
      function B (Str : String) return String is ("[ " & Str & " ]");
      function Q (Str : String) return String is ("""" & Str & """");

      Source : AAA.Strings.Vector;
      Alt    : AAA.Strings.Vector;

      Filtered : constant Graph := This.Filtering_Unused (Solution.Crates);
   begin
      Alt.Append ("graph dependencies {");

      for Dep of Filtered loop
         Alt.Append (Q (Label_Dependent (+Dep.Dependent, Solution))
                     & " -> "
                     & Q (Label_Dependee (+Dep.Dependent,
                                          +Dep.Dependee,
                                          Solution,
                                          For_Plot => True)) & "; ");

         Source.Append (B (Label_Dependent (+Dep.Dependent, Solution))
                        & " -> "
                        & B (Label_Dependee (+Dep.Dependent,
                                             +Dep.Dependee,
                                             Solution,
                                             For_Plot => True)));
      end loop;
      Alt.Append (" }");

      declare
         Tmp : Directories.Temp_File;
      begin
         Source.Write (Tmp.Filename, Separator => " ");
         OS_Lib.Subprocess.Checked_Spawn
           (Paths.Scripts_Graph_Easy,
            AAA.Strings.Empty_Vector
            .Append ("--as=boxart")
            .Append (Tmp.Filename));
      end;
   end Plot;

   -----------
   -- Print --
   -----------

   procedure Print (This     : Graph;
                    Solution : Solutions.Solution;
                    Prefix   : String := "")
   is
      Table : Alire.Utils.Tables.Table;

      Filtered : constant Graph := This.Filtering_Unused (Solution.Crates);
   begin
      for Dep of Filtered loop
         Table.Append
           (Prefix & Label_Dependent (+Dep.Dependent, Solution, TTY => True));
         Table.Append ("-->");
         Table.Append
           (Label_Dependee
              (+Dep.Dependent, +Dep.Dependee, Solution, For_Plot => False));
         Table.New_Row;
      end loop;

      Table.Print (Always);
   end Print;

   -----------------------
   -- Removing_Dependee --
   -----------------------

   function Removing_Dependee (This    : Graph;
                               Crate : Alire.Crate_Name) return Graph is
   begin
      return Result : Graph do
         for Dep of This loop
            if +Dep.Dependee /= Crate then
               Result.Include (Dep);
            end if;
         end loop;
      end return;
   end Removing_Dependee;

end Alire.Dependencies.Graphs;
