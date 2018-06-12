with AAA.Table_IO;

with Alire.Conditional;

with Alr.OS_Lib;
with Alr.Platform;
with Alr.Utils;
with Alr.Utils.Temp_File;

package body Alr.Dependency_Graphs is

   -----------------
   -- Empty_Graph --
   -----------------

   function Empty_Graph return Graph is
      (Dep_Sets.Empty_Set with null record);

   -------------------
   -- From_Instance --
   -------------------

   function From_Instance (Instance : Alire.Containers.Release_Map) return Graph is
   begin
      return Result : Graph do
         for Rel of Instance loop
            Result := Result.Including (Rel);
         end loop;

         Result := Result.Filtering_Unused (Instance);
      end return;
   end From_Instance;

   ---------------
   -- Including --
   ---------------

   function Including (This : Graph;
                       R    : Types.Release) return Graph
   is

      function Enumerate is new Alire.Conditional.For_Dependencies.Enumerate
        (Alire.Containers.Dependency_Lists.List,
         Alire.Containers.Dependency_Lists.Append);

   begin
      return Result : Graph := This do
         for Dep of Enumerate (R.Dependencies.Evaluate (Platform.Properties)) loop
            Result.Include (New_Dependency (R.Project, Dep.Project));
         end loop;
      end return;
   end Including;

   ----------------------
   -- Filtering_Unused --
   ----------------------

   function Filtering_Unused (This     : Graph;
                              Instance : Alire.Containers.Release_Map) return Graph is
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
                              Project : Alire.Project) return Boolean is
   begin
      for Dep of This loop
         if +Dep.Dependent = Project then
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

      Source : Utils.String_Vector;
   begin
      for Dep of This loop
         Source.Append (B (Instance (+Dep.Dependent).Milestone.Image) &
                          " -> " &
                          B (Instance (+Dep.Dependee).Milestone.Image));
      end loop;

      declare
         Tmp : constant Utils.Temp_File.File := Utils.Temp_File.New_File;
      begin
         Source.Write (Tmp.Name, Separator => " ");
         OS_Lib.Spawn_Raw ("graph-easy", Tmp.Name);
      end;
   end Plot;

   -----------
   -- Print --
   -----------

   procedure Print (This     : Graph;
                    Instance : Alire.Containers.Release_Map;
                    Prefix   : String := "")
   is
      Table : AAA.Table_IO.Table;
   begin
      for Dep of This loop
         Table.Append (Prefix & Instance (+Dep.Dependent).Milestone.Image);
         Table.Append ("-->");
         Table.Append (Instance (+Dep.Dependee).Milestone.Image);
         Table.New_Row;
      end loop;

      Table.Print;
   end Print;

   -----------------------
   -- Removing_Dependee --
   -----------------------

   function Removing_Dependee (This    : Graph;
                               Project : Alire.Project) return Graph is
   begin
      return Result : Graph do
         for Dep of This loop
            if +Dep.Dependee /= Project then
               Result.Include (Dep);
            end if;
         end loop;
      end return;
   end Removing_Dependee;

end Alr.Dependency_Graphs;
