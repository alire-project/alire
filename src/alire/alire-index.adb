with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Projects;

package body Alire.Index is

   use all type Semantic_Versioning.Version;

   package Name_Entry_Maps
   is new Ada.Containers.Indefinite_Ordered_Maps (Alire.Project,
                                                  Catalog_Entry);

   Master_Entries : Name_Entry_Maps.Map;

   ---------------------------------
   -- Manually_Catalogued_Project --
   ---------------------------------

   function Manually_Catalogued_Project
     (Crate_Name, Description : String) return Catalog_Entry
   is
      use Alire.Utils, Name_Entry_Maps;
      Project  : constant Alire.Project := +To_Lower_Case (Crate_Name);
      Position : constant Cursor := Master_Entries.Find (Project);
   begin
      if Has_Element (Position) then

         --  If this package was already registered, just check that arguments
         --  haven't changed.

         return Result : constant Catalog_Entry := Element (Position) do
            if Result.Description /= Description then
               raise Constraint_Error;
            end if;
         end return;

      else
         --  Otherwise, create the entry and register it

         return C : constant Catalog_Entry :=
           (Name_Len  => Crate_Name'Length,
            Descr_Len => Description'Length,

            Project      => Project,
            Description  => Description)
         do
            Master_Entries.Insert (C.Project, C);
            Projects.Descriptions.Include (C.Project, Description);
            --  This description may be already present from the session
            --  project file.
         end return;
      end if;
   end Manually_Catalogued_Project;

   ------------
   -- Exists --
   ------------

   function Exists (Project : Alire.Project;
                    Version : Semantic_Versioning.Version)
                    return Boolean is
   begin
      for R of Catalog loop
         if R.Project = Project and then R.Version = Version then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Project : Alire.Project;
                  Version : Semantic_Versioning.Version) return Release is
   begin
      for R of Catalog loop
         if R.Project = Project and then R.Version = Version then
            return R;
         end if;
      end loop;

      raise Constraint_Error
        with "Not in index: " & (+Project) & "=" &
        Semantic_Versioning.Image (Version);
   end Find;

   -------------------
   -- Register_Real --
   -------------------

   function Register_Real (R : Release) return Release is
   begin
      if Catalog.Contains (R) then
         Trace.Debug ("Not registering release already indexed: " &
                        R.Milestone.Image);
      else
         Catalog.Insert (R);
      end if;

      return R;
   end Register_Real;

   --------------
   -- Register --
   --------------

   function Register
     ( --  Mandatory
                      This               : Catalog_Entry;
       Version            : Semantic_Versioning.Version;
       Origin             : Origins.Origin;
       -- we force naming beyond this point with this ugly guard:
       XXXXXXXXXXXXXX     : Utils.XXX_XXX         := Utils.XXX_XXX_XXX;
       --  Optional
       Notes              : Description_String    := "";
       Dependencies       : Release_Dependencies  := No_Dependencies;
       Properties         : Release_Properties    := No_Properties;
       Private_Properties : Release_Properties    := No_Properties;
       Available_When     : Release_Requisites    := No_Requisites)
      return Release
   is
      pragma Unreferenced (XXXXXXXXXXXXXX);
   begin
      return Register_Real
        (Alire.Releases.New_Release
           (Project            => This.Project,
            Version            => Version,
            Origin             => Origin,
            Notes              => Notes,
            Dependencies       => Dependencies,
            Properties         => Properties,
            Private_Properties => Private_Properties,
            Available          => Available_When));
   end Register;

end Alire.Index;
