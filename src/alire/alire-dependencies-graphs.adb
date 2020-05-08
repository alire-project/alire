with Alire.Conditional;
with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Utils.Tables;

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

         Result := Result.Filtering_Unused (Sol.Releases);
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

      function Enumerate is new Alire.Conditional.For_Dependencies.Enumerate
        (Alire.Containers.Dependency_Lists.List,
         Alire.Containers.Dependency_Lists.Append);

   begin
      return Result : Graph := This do
         for Dep of Enumerate (R.Dependencies.Evaluate (Env))
         loop
            Result.Include (New_Dependency (R.Name, Dep.Crate));
         end loop;
      end return;
   end Including;

   ----------------------
   -- Filtering_Unused --
   ----------------------

   function Filtering_Unused (This     : Graph;
                              Instance : Alire.Containers.Release_Map)
                              return Graph
   is
   begin
      return Result : Graph do
         for Dep of This loop
            if Instance.Contains (+Dep.Dependee) then
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
   begin
      for Dep of This loop
         if +Dep.Dependent = Crate then
            return True;
         end if;
      end loop;

      return False;
   end Has_Dependencies;

   ----------
   -- Plot --
   ----------

   procedure Plot (This     : Graph;
                   Instance : Alire.Containers.Release_Map)
   is
      function B (Str : String) return String is ("[ " & Str & " ]");
      function Q (Str : String) return String is ("""" & Str & """");

      Source : Utils.String_Vector;
      Alt    : Utils.String_Vector;

      Filtered : constant Graph := This.Filtering_Unused (Instance);
   begin
      Alt.Append ("graph dependencies {");

      for Dep of Filtered loop
         Alt.Append (Q (Instance (+Dep.Dependent).Milestone.Image) &
                          " -> " &
                          Q (Instance (+Dep.Dependee).Milestone.Image) & "; ");

         Source.Append (B (Instance (+Dep.Dependent).Milestone.Image) &
                          " -> " &
                          B (Instance (+Dep.Dependee).Milestone.Image));
      end loop;
      Alt.Append (" }");

      declare
         Tmp : Directories.Temp_File;
      begin
         Source.Write (Tmp.Filename, Separator => " ");
         OS_Lib.Subprocess.Checked_Spawn
           (Paths.Scripts_Graph_Easy,
            Utils.Empty_Vector.Append ("--as=boxart").Append (Tmp.Filename));
      end;
   end Plot;

   -----------
   -- Print --
   -----------

   procedure Print (This     : Graph;
                    Instance : Alire.Containers.Release_Map;
                    Prefix   : String := "")
   is
      Table : Alire.Utils.Tables.Table;

      Filtered : constant Graph := This.Filtering_Unused (Instance);
   begin
      for Dep of Filtered loop
         Table.Append (Prefix & Instance (+Dep.Dependent).Milestone.TTY_Image);
         Table.Append ("-->");
         Table.Append (Instance (+Dep.Dependee).Milestone.TTY_Image);
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
