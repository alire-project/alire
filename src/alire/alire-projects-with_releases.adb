with Ada.Strings.Fixed;

with Alire.Properties.Labeled;
with Alire.Releases;
with Alire.TOML_Keys;

with Semantic_Versioning;

with TOML;

package body Alire.Projects.With_Releases is

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out Crate;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is
      package Semver renames Semantic_Versioning;

      --  TODO: reestructure this proc into several smaller procs.
      --  general/child-inheritance/releases.

   begin
      --  Process the general key
      declare
         package APL renames Alire.Properties.Labeled;
         use all type TOML.Any_Value_Kind;

         Val : TOML.TOML_Value;
      begin
         if not From.Pop (TOML_Keys.General, Val) then
            return From.Failure ("missing general section in crate");
         elsif Val.Kind /= TOML_Table then
            return From.Failure ("general section must be a table");
         end if;

         --  Ensure mandatory properties are there:
         for Label in APL.Mandatory'Range loop
            if APL.Mandatory (Label) then
               if not Val.Has (APL.Key (Label)) then
                  return From.Failure
                    ("mandatory property missing: " & APL.Key (Label));
               end if;
            end if;
         end loop;

         --  Load the [general] crate part
         declare
            Result : constant Outcome :=
                       General (This)
                         .From_TOML (From.Descend (Val, Context => "general"));
         begin
            if not Result.Success then
               return Result;
            end if;
         end;
      end;

      --  Process remaining keys, that must be releases
      loop
         declare
            Val : TOML.TOML_Value;
            Key : constant String := From.Pop (Val);
            Ver : Semver.Version;
         begin
            exit when Key = "";

            Ver := Semver.Parse (Key, Relaxed => False);

            declare
               Release : Alire.Releases.Release :=
                           Alire.Releases
                             .New_Working_Release (Project => This.Name)
                             .Retagging (Ver)
                             .Replacing (Properties   => This.Properties)
                             .Replacing (Dependencies => This.Dependencies)
                             .Replacing (Available    => This.Available);
               Result  : constant Outcome :=
                           Release
                             .From_TOML (From.Descend (Val, Context => Key));
            begin
               if not Result.Success then
                  return Result;
               end if;

               This.Releases.Insert (Release);
            end;

         exception
            when Semver.Malformed_Input =>
               return Outcome_Failure ("Bad crate version: " & Key);
         end;
      end loop;

      --  There cannot be any remaining keys at this level, as any unknown key
      --  has been processed as a version or already reported as invalid.

      if This.Releases.Is_Empty then
         Trace.Debug ("Crate contains no releases: " & (+This.Name));
      end if;

      return Outcome_Success;
   end From_TOML;

   -----------------
   -- Description --
   -----------------

   function Description (This : Crate) return Description_String is
      Descr : constant Properties.Vector :=
                Properties.Labeled.Filter
                  (Conditional.Enumerate (This.Properties),
                   Properties.Labeled.Description);
   begin
      if Natural (Descr.Length) > 1 then
         raise Program_Error with "Multiple descriptions!";
         --  Shouldn't happen because the loader checks for multiplicity.
      else
         return Properties.Labeled.Label (Descr.First_Element).Value;
      end if;
   end Description;

   ----------
   -- Name --
   ----------

   function Name (This : Crate) return Alire.Project is (+(+This.Name));

   ---------------
   -- New_Crate --
   ---------------

   function New_Crate (Name : Alire.Project) return Crate is
     (Crate'(General with
             Len      => Name'Length,
             Name     => Name,
             Releases => <>));

   --------------
   -- Releases --
   --------------

   function Releases (This : Crate) return Containers.Release_Set is
      (This.Releases);

   --------------
   -- Is_Child --
   --------------

   function Is_Child (This : Crate) return Boolean is
     (for some Char of This.Name => Char = Child_Separator);

   -----------------
   -- Parent_Name --
   -----------------

   function Parent_Name (This : Crate) return Alire.Project is
      Last_Sep_Pos : constant Positive :=
                       Ada.Strings.Fixed.Index
                         (Source  => +This.Name,
                          Pattern => String'(1 => Child_Separator),
                          Going   => Ada.Strings.Backward);
   begin
      return This.Name (This.Name'First .. Last_Sep_Pos);
   end Parent_Name;

end Alire.Projects.With_Releases;
