with Ada.Containers.Indefinite_Ordered_Maps;

with GNAT.Source_Info;

package body Alire.Index is

   use all type Version;

   package Name_Entry_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Alire.Project,
                                                                          Catalog_Entry);

   Master_Entries : Name_Entry_Maps.Map;

   type Reflected_Info (Pack_Len, Id_Len : Positive) is record
      Package_Name : String (1 .. Pack_Len);
      Identifier   : String (1 .. Id_Len);
   end record;

   --------------
   -- Identify --
   --------------

   function Identify (Enclosing : String) return Reflected_Info is
      use Utils;
      Identifier : constant String := -- Portion after last dot
                     Split (Enclosing, '.', Side => Tail, From => Tail);
      Full_Name  : constant String := -- Portion after Alire.Index.
                     Split (Enclosing, '.', Side => Tail, From => Head, Count => 2);
      Pack_Name  : constant String := -- Portion between Alire.Index. and .Identifier
                     Split (Full_Name, '.', Side => Head, From => Tail);
   begin
      return (Pack_Name'Length, Identifier'Length, Pack_Name, Identifier);
   end Identify;

   ------------------------
   -- Catalogued_Project --
   ------------------------

   function Catalogued_Project return Catalog_Entry is
      Reflected : constant Reflected_Info :=
                    Identify (GNAT.Source_Info.Enclosing_Entity);
   begin
      return Manually_Catalogued_Project
        (Reflected.Package_Name, Reflected.Identifier, Description);
   end Catalogued_Project;

   ---------------------------------
   -- Manually_Catalogued_Project --
   ---------------------------------

   function Manually_Catalogued_Project
     (Package_Name, Self_Name, Description : String) return Catalog_Entry
   is
      use Alire.Utils, Name_Entry_Maps;
      Project  : constant Alire.Project := +To_Lower_Case (Package_Name);
      Position : constant Cursor := Master_Entries.Find (Project);
   begin
      if Has_Element (Position) then

         --  If this package was already registered, just check that arguments
         --  haven't changed.

         return Result : constant Catalog_Entry := Element (Position) do
            if Result.Description /= Description
               or else Result.Package_Name /= Package_Name
               or else Result.Self_Name /= Self_Name
            then
               raise Constraint_Error;
            end if;
         end return;

      else
         --  Otherwise, create the entry and register it

         return C : constant Catalog_Entry :=
           (Name_Len  => Package_Name'Length,
            Descr_Len => Description'Length,
            Pack_Len  => Package_Name'Length,
            Self_Len  => Self_Name'Length,

            Project      => Project,
            Description  => Description,
            Package_Name => Package_Name,
            Self_Name    => Self_Name)
         do
            Master_Entries.Insert (C.Project, C);
            Projects.Descriptions.Insert (C.Project, Description);
         end return;
      end if;
   end Manually_Catalogued_Project;

   -------------
   -- Current --
   -------------

   function Current (C : Catalog_Entry) return Release is
   begin
      for R of reverse Catalog loop
         if R.Project = C.Project then
            return R;
         end if;
      end loop;

      raise Program_Error with "Catalog entry without releases: " & (+C.Project);
   end Current;

   ---------
   -- Get --
   ---------

   function Get (Name : Alire.Project) return Catalog_Entry is
     (Master_Entries.Element (Name));

   --------------------------
   -- Is_Currently_Indexed --
   --------------------------

   function Is_Currently_Indexed (Name : Alire.Project) return Boolean is
      (Master_Entries.Contains (Name));

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

      raise Constraint_Error with "Not in index: " & (+Project) & "=" & Semantic_Versioning.Image (Version);
   end Find;

   -------------------
   -- Register_Real --
   -------------------

   function Register_Real (R : Release) return Release is
   begin
      if Catalog.Contains (R) then
         Trace.Debug ("Not registering release already indexed: " & R.Milestone.Image);
      else
         Catalog.Insert (R);
      end if;

      return R;
   end Register_Real;

   --------------
   -- Register --
   --------------

   function Register (--  Mandatory
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

   --------------
   -- Register --
   --------------

   function Register (Extension          : Catalog_Entry;
                      Extended_Release   : Release)
                      return Release
   is
   begin
      return Register_Real (Extended_Release.Replacing
                            (Project => Extension.Project));
   end Register;

   ----------------
   -- Unreleased --
   ----------------

   function Unreleased (This               : Catalog_Entry;
                        Version            : Semantic_Versioning.Version := No_Version;
                        Origin             : Origins.Origin        := No_Origin;
                        Notes              : Description_String    := "";
                        Dependencies       : Release_Dependencies  := No_Dependencies;
                        Properties         : Release_Properties    := No_Properties;
                        Private_Properties : Release_Properties    := No_Properties;
                        Available_When     : Release_Requisites    := No_Requisites)
                    return Release
   is
   begin
      return
        Alire.Releases.New_Release (Project            => This.Project,
                                    Version            => Version,
                                    Origin             => Origin,
                                    Notes              => Notes,
                                    Dependencies       => Dependencies,
                                    Properties         => Properties,
                                    Private_Properties => Private_Properties,
                                    Available          => Available_When);
   end Unreleased;

   -----------------
   -- New_Release --
   -----------------

   package body Project_Release is

      The_Release : constant Index.Release :=
                      Project.Register			--  Add to catalog
                        (Base.Retagging                 --  Overriding the version
                           (Versions.From_Identifier    --  with the one in the
                              (Identify			--  package name
                                 (GNAT.Source_Info.Enclosing_Entity).Identifier)));

      -------------
      -- Release --
      -------------

      function Release return Index.Release is
      begin
         return The_Release;
      end Release;

      function Version return Semantic_Versioning.Version is
        (The_Release.Version);

      function Version return Semantic_Versioning.Version_Set is
        (Exactly (The_Release.Version));

      function This_Version return Conditional.Dependencies is
         (The_Release.This_Version);

      function Within_Major return Conditional.Dependencies is
         (The_Release.Within_Major);

      function Within_Minor return Conditional.Dependencies is
         (The_Release.Within_Minor);

   end Project_Release;

end Alire.Index;
