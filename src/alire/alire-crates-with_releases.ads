with Alire.Interfaces;
with Alire.Containers;
with Alire.Externals.Lists;
with Alire.Releases;
with Alire.TOML_Adapters;

with Semantic_Versioning;

package Alire.Crates.With_Releases is

   type Crate (<>) is new General and Interfaces.Detomifiable
   with private;
   --  A complete crate with its releases.

   function New_Crate (Name : Crate_Name) return Crate;

   function Name (This : Crate) return Crate_Name;

   function TTY_Name (This : Crate) return String;

   procedure Add (This    : in out Crate;
                  Release : Releases.Release) with Pre =>
     not This.Contains (Release.Version) or else
     raise Checked_Error with
       "Crate already contains given release: "
       & Semantic_Versioning.Image (Release.Version);

   function Base (This : Crate) return Releases.Release;
   --  Returns a release sharing only this crate mandatory properties (see
   --  Alire.Properties.Labeled.Mandatory) that can be used as template for
   --  new releases in this crate (e.g., by externally detected releases).

   function Contains (This    : Crate;
                      Version : Semantic_Versioning.Version) return Boolean;

   function Description (This : Crate) return Description_String;

   function TTY_Description (This : Crate) return String;

   function Externals (This : Crate) return Alire.Externals.Lists.List;

   function Releases (This : Crate) return Containers.Release_Set;

   overriding
   function From_TOML (This : in out Crate;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome;

   procedure Replace (This    : in out Crate;
                      Release : Alire.Releases.Release) with Pre =>
     This.Contains (Release.Version) or else
     raise Checked_Error with
       "Crate does not contain given release: "
       & Semantic_Versioning.Image (Release.Version);

private

   type Crate (Len : Natural) is new General and
     Interfaces.Detomifiable with
      record
         Name      : Crate_Name (1 .. Len);
         Externals : Alire.Externals.Lists.List;
         Releases  : Containers.Release_Set;
      end record;

end Alire.Crates.With_Releases;
