with Alire.Index;
with Alire.Origins;
with Alire.Properties.Labeled;
with Alire.Provides;
with Alire.TOML_Keys;
with Alire.TOML_Load;
with Alire.User_Pins.Maps;
with Alire.Utils.TTY;

with TOML;

package body Alire.Crates is

   -----------------------
   -- Naming_Convention --
   -----------------------

   function Naming_Convention return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
      .Append ("Identifiers for crates and indexes must use "
        & "lowercase alphanumeric characters from the latin "
        & "alphabet. Underscores can also be used except as "
        & "the first character.")
      .New_Line
      .Append ("Length must be of" & Alire.Min_Name_Length'Img
               & " to" & Alire.Max_Name_Length'Img
               & " characters."));

   ----------
   -- Keys --
   ----------

   package Keys is new Alire.Releases.Containers.Release_Sets.Generic_Keys
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
   begin
      return Alire.Releases.New_Release
        (Name         => This.Name,
         Version      => Semantic_Versioning.Parse ("0"),
         Origin       => Origins.New_Filesystem ("."),
         Notes        => "",
         Dependencies => Conditional.No_Dependencies,
         Properties   => This.Externals.Properties,
         Available    => Conditional.Empty);
   end Base;

   --------------
   -- Contains --
   --------------

   function Contains (This    : Crate;
                      Version : Semantic_Versioning.Version) return Boolean
   is
   begin
      return Keys.Contains
        (Alire.Releases.Containers.Release_Sets.Set (This.Releases),
         Version);
   end Contains;

   ---------------
   -- Externals --
   ---------------

   function Externals (This : Crate) return Alire.Externals.Lists.List is
     (This.Externals.Detectors);

   -----------------------------
   -- From_Externals_Manifest --
   -----------------------------

   function From_Externals_Manifest (From   : TOML_Adapters.Key_Queue;
                                     Strict : Boolean)
                                     return Crate
   is
   begin
      From.Assert_Key (TOML_Keys.Name, TOML.TOML_String);

      return This : Crate :=
        New_Crate (+From.Unwrap.Get (TOML_Keys.Name).As_String)
      do
         This.Load_Externals (From, Strict);
      end return;
   end From_Externals_Manifest;

   --------------------
   -- Load_Externals --
   --------------------

   procedure Load_Externals
     (This   : in out Crate;
      From   :        TOML_Adapters.Key_Queue;
      Strict :        Boolean;
      Policy :        Policies.For_Index_Merging :=
        Policies.Merge_Priorizing_Existing)
   is
      --------------------
      -- Load_Externals --
      --------------------

      procedure Load_Externals_Array is
         TOML_Externals : TOML.TOML_Value;
         Has_Externals  : constant Boolean :=
                            From.Pop (TOML_Keys.External, TOML_Externals);
      begin
         if Has_Externals then
            if TOML_Externals.Kind not in TOML.TOML_Array then
               From.Checked_Error ("external entries must be TOML arrays");
            else
               for I in 1 .. TOML_Externals.Length loop
                  This.Externals.Detectors.Append
                    (Alire.Externals.From_TOML
                       (From.Descend
                            (TOML_Externals.Item (I),
                             "external index" & I'Img),
                        Strict));
               end loop;
            end if;

            --  Register any aliased in the detectors for this crate, so we
            --  know when to detect.

            for Detector of This.Externals.Detectors loop
               for Alias of Detector.Equivalences loop
                  Index.Register_Alias (Provider  => This.Name,
                                        Providing => Alias);
               end loop;
            end loop;
         end if;
      end Load_Externals_Array;

   begin
      if From.Unwrap.Kind not in TOML.TOML_Table then
         From.Checked_Error ("top-level section must be a table");
      end if;

      --  Process any external detectors

      Load_Externals_Array;

      --  Load the shared section

      declare
         Unused_Avail : Conditional.Availability;
         Unused_Deps  : Conditional.Dependencies;
         Unused_Equiv : Provides.Equivalences;
         Unused_Pins  : User_Pins.Maps.Map;
         Properties   : Conditional.Properties;
      begin
         TOML_Load.Load_Crate_Section
           (Strict  => Strict,
            Section => External_Shared_Section,
            From    => From,
            Props   => Properties,
            Deps    => Unused_Deps,
            Equiv   => Unused_Equiv,
            Forbids => Unused_Deps,
            Pins    => Unused_Pins,
            Avail   => Unused_Avail);

         Assert (Unused_Deps.Is_Empty,
                 "Unexpected dependencies in external definition");
         Assert (Unused_Pins.Is_Empty,
                 "Unexpected pins in external definition");
         Assert (Unused_Avail.Is_Empty,
                 "Unexpected availability in external definition");

         case Policy is
            when Policies.Merge_Priorizing_Existing =>
               if This.Externals.Properties.Is_Empty then
                  This.Externals.Properties := Properties;
               else
                  Trace.Debug ("Discarding new properties for externals base");
               end if;
         end case;
      end;

      From.Report_Extra_Keys;
   end Load_Externals;

   ---------------------
   -- Merge_Externals --
   ---------------------

   procedure Merge_Externals
     (This   : in out Crate;
      From   :        Crate;
      Policy :        Policies.For_Index_Merging :=
        Policies.Merge_Priorizing_Existing)
   is
      use type Alire.Externals.External'Class;
   begin
      --  Merge new external detectors

      case Policy is
         when Policies.Merge_Priorizing_Existing =>
            if This.Externals.Properties.Is_Empty then
               This.Externals.Properties := From.Externals.Properties;
            end if;

            for Ext of From.Externals.Detectors loop
               if not (for some Existing of This.Externals.Detectors =>
                         Ext = Existing)
               then
                  This.Externals.Detectors.Append (Ext);
               end if;
            end loop;
      end case;
   end Merge_Externals;

   -----------------
   -- Description --
   -----------------

   function Description (This : Crate) return Description_String is
      Descr : constant Properties.Vector :=
                Properties.Labeled.Filter
                  (Conditional.Enumerate (This.Externals.Properties),
                   Properties.Labeled.Description);
   begin
      if not This.Releases.Is_Empty then
         return This.Releases.Last_Element.Description;
      elsif not Descr.Is_Empty then
         return Properties.Labeled.Label (Descr.First_Element).Value;
      else
         return "Crate is empty and a description cannot thus be provided";
      end if;
   end Description;

   ---------------------
   -- TTY_Description --
   ---------------------

   function TTY_Description (This : Crate) return String
   is (Utils.TTY.Description (This.Description));

   ----------
   -- Name --
   ----------

   function Name (This : Crate) return Crate_Name is (This.Name);

   --------------
   -- TTY_Name --
   --------------

   function TTY_Name (This : Crate) return String
   is (Utils.TTY.Name (+This.Name));

   ---------------
   -- New_Crate --
   ---------------

   function New_Crate (Name : Crate_Name) return Crate is
     (Crate'(Len       => Name.Length,
             Name      => Name,
             Externals => <>,
             Releases  => <>));

   --------------
   -- Releases --
   --------------

   function Releases (This : Crate)
                      return Alire.Releases.Containers.Release_Set
   is (This.Releases);

   -------------
   -- Replace --
   -------------

   procedure Replace (This    : in out Crate;
                      Release : Alire.Releases.Release)
   is
   begin
      Keys.Replace (Alire.Releases.Containers.Release_Sets.Set (This.Releases),
                    Release.Version,
                    Release);
   end Replace;

end Alire.Crates;
