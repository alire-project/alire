with Alire.Conditional;
with Alire.Containers;
with Alire.Externals.Lists;
with Alire.Policies;
with Alire.Properties;
with Alire.Releases;
with Alire.Requisites;
with Alire.TOML_Adapters;
with Alire.Utils;

with Semantic_Versioning;

package Alire.Crates is

   function Naming_Convention return Utils.String_Vector;
   --  Return a description of the naming restrictions on crates/indexes.

   type Sections is (Release_Section,
                     --  Top-level table, with all info about a release

                     External_Shared_Section,
                     --  Top-level table, with only info valid for externals

                     External_Private_Section
                     --  Info that can be provided by all external definitions,
                     --  but already in their private [[external]] entry.
                    );

   type Crate (<>) is tagged private;
   --  A complete crate with its releases and external definitions.

   function New_Crate (Name : Crate_Name) return Crate;

   function Name (This : Crate) return Crate_Name;

   function TTY_Name (This : Crate) return String;

   procedure Add (This    : in out Crate;
                  Release : Releases.Release) with Pre =>
     not This.Contains (Release.Version) or else
     raise Checked_Error with
       "Crate already contains given release: "
       & Semantic_Versioning.Image (Release.Version);

   function Base (This : Crate) return Releases.Release
     with Pre => not This.Externals.Is_Empty;
   --  Returns a release sharing only this crate mandatory properties (see
   --  Alire.Properties.Labeled.Mandatory) that can be used as template for
   --  new releases in this crate (e.g., by externally detected releases).

   function Contains (This    : Crate;
                      Version : Semantic_Versioning.Version) return Boolean;

   function Description (This : Crate) return Description_String;
   --  Will return the last release description, or the one in the external
   --  base properties, or a message saying the crate is totally empty.

   function TTY_Description (This : Crate) return String;

   function Externals (This : Crate) return Alire.Externals.Lists.List;

   function From_Externals_Manifest (From : TOML_Adapters.Key_Queue)
                                          return Crate;
   --  Load a manifest containing only external definitions for a crate

   procedure Load_Externals
     (This   : in out Crate;
      From   :        TOML_Adapters.Key_Queue;
      Policy :        Policies.For_Index_Merging :=
        Policies.Merge_Priorizing_Existing);
   --  Load externals detectors into an existing crate

   procedure Merge_Externals
     (This   : in out Crate;
      From   :        Crate;
      Policy :        Policies.For_Index_Merging :=
                        Policies.Merge_Priorizing_Existing);
   --  Merge external definitions from both crates, applying some index merging
   --  policy.

   function Releases (This : Crate) return Containers.Release_Set;

   procedure Replace (This    : in out Crate;
                      Release : Alire.Releases.Release) with Pre =>
     This.Contains (Release.Version) or else
     raise Checked_Error with
       "Crate does not contain given release: "
       & Semantic_Versioning.Image (Release.Version);

private

   type External_Data is record
      Properties : Conditional.Properties;
      --  Properties that are defined with external definitions, used as
      --  base of any detected release (description, etc). This will be
      --  empty for crates without external definitions.

      Detectors  : Alire.Externals.Lists.List;
      --  External detectors defined for the crate
   end record;

   type Crate (Len : Natural) is tagged record
      Name      : Crate_Name (Len);
      Externals : External_Data;
      Releases  : Containers.Release_Set;
   end record;

end Alire.Crates;
