with Alire.Conditional;
with Alire.Errors;
with Alire.Origins;
with Alire.Properties.Labeled;
with Alire.Requisites;
with Alire.TOML_Keys;

with TOML;

package body Alire.Crates.With_Releases is

   package Keys is new Containers.Release_Sets.Generic_Keys
     (Semantic_Versioning.Version,
      Alire.Releases.Version,
      Semantic_Versioning."<");

   ---------
   -- Add --
   ---------

   procedure Add (This    : in out Crate;
                  Release : Alire.Releases.Release) is
   begin
      This.Releases.Insert (Release);
   end Add;

   ----------
   -- Base --
   ----------

   function Base (This : Crate) return Alire.Releases.Release is
      use type Conditional.Properties;
      All_Props : constant Properties.Vector :=
                    Conditional.Enumerate (This.Properties);
      Props : Conditional.Properties;
   begin
      for Label in Properties.Labeled.Labels loop
         if Properties.Labeled.Mandatory (Label) then
            for Prop of Properties.Labeled.Filter (All_Props, Label) loop
               Props := Props and Conditional.For_Properties.New_Value (Prop);
            end loop;
         end if;
      end loop;

      return Alire.Releases.New_Release
        (Name         => This.Name,
         Version      => Semantic_Versioning.Parse ("0"),
         Origin       => Origins.New_Filesystem ("."),
         Notes        => "",
         Dependencies => Conditional.No_Dependencies,
         Properties   =>  (Props),
         Available    => Requisites.No_Requisites);
   end Base;

   --------------
   -- Contains --
   --------------

   function Contains (This    : Crate;
                      Version : Semantic_Versioning.Version) return Boolean
   is
   begin
      return Keys.Contains (This.Releases, Version);
   end Contains;

   ---------------
   -- Externals --
   ---------------

   function Externals (This : Crate) return Alire.Externals.List is
     (This.Externals);

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out Crate;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is
      package Semver renames Semantic_Versioning;

      --------------------
      -- Load_Externals --
      --------------------

      procedure Load_Externals is
         TOML_Externals : TOML.TOML_Value;
         Has_Externals  : constant Boolean :=
                            From.Pop (TOML_Keys.External, TOML_Externals);
      begin
         if Has_Externals then
            if TOML_Externals.Kind not in TOML.TOML_Array then
               From.Checked_Error ("external entries must be TOML arrays");
            else
               for I in 1 .. TOML_Externals.Length loop
                  This.Externals.Append
                    (Alire.Externals.From_TOML
                       (From.Descend (TOML_Externals.Item (I),
                                      "external index" & I'Img)));
               end loop;
            end if;
         end if;
      end Load_Externals;

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

      --  Process any external detectors

      Load_Externals;

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
                             .New_Working_Release (Name => This.Name)
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
         --  This does not make much sense currently, but it might serve as a
         --  placeholder, or if in the future we rescue child crates.
      end if;

      return Outcome_Success;

   exception
      when E : Alire.Checked_Error =>
         return Errors.Get (E);
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

   function Name (This : Crate) return Crate_Name is (+(+This.Name));

   ---------------
   -- New_Crate --
   ---------------

   function New_Crate (Name : Crate_Name) return Crate is
     (Crate'(General with
             Len       => Name'Length,
             Name      => Name,
             Externals => <>,
             Releases  => <>));

   --------------
   -- Releases --
   --------------

   function Releases (This : Crate) return Containers.Release_Set is
     (This.Releases);

   -------------
   -- Replace --
   -------------

   procedure Replace (This    : in out Crate;
                      Release : Alire.Releases.Release)
   is
   begin
      Keys.Replace (This.Releases, Release.Version, Release);
   end Replace;

end Alire.Crates.With_Releases;
