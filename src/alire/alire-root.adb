with Alire.Index;

package body Alire.Root is

   type Root_Access is access Roots.Root;
   Root : Root_Access;
   --  Root dependency (the working project). If Is_Empty we know we must recompile,
   --  unless the hash already matches. In this case, we know the project file is
   --  missing the Set call

   -------------
   -- Current --
   -------------

   function Current return Roots.Root is (Root.all);

   ------------
   -- Is_Set --
   ------------

   function Is_Set return Boolean is (Root /= null);

   ---------
   -- Set --
   ---------

   function Set (Project      : Alire.Project;
                 Version      : Semantic_Versioning.Version)
                 return Roots.Root
   is
   begin
      if Index.Exists (Project, Version) then
         Root := new Roots.Root'(Index.Find (Project, Version) with null record);
         Trace.Debug ("Storing indexed release as root: " & Root.Milestone.Image);
         return Root.all;
      else
         --  Session is outdated or outside
         Trace.Error ("Requesting released root not in index: " &
                      (+Project) & "=" & Semantic_Versioning.Image (Version));
         raise Constraint_Error;
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   function Set (Project      : Alire.Project;
                 Dependencies : Conditional.Dependencies)
                 return Roots.Root
   is
   begin
      Trace.Debug ("Storing unindexed project as root:" & (+Project));
      Root := new Roots.Root'
        (Releases.New_Working_Release (Project, Dependencies => Dependencies) with null record);
      return Root.all;
   end Set;

   ---------
   -- Set --
   ---------

   function Set (Release : Releases.Release) return Roots.Root is
   begin
      Trace.Debug ("Storing unindexed release as root:" & Release.Milestone.Image);
      Root := new Roots.Root'(Release with null record);
      return Root.all;
   end Set;

end Alire.Root;
