with Alire.Interfaces;
with Alire.Containers;
with Alire.Externals;
with Alire.Releases;
with Alire.TOML_Adapters;

with Semantic_Versioning;

package Alire.Crates.With_Releases is

   type Crate (<>) is new General and Interfaces.Detomifiable
   with private;
   --  A complete crate with its releases.

   function New_Crate (Name : Crate_Name) return Crate;

   function Name (This : Crate) return Crate_Name;

   procedure Add (This    : in out Crate;
                  Release : Releases.Release) with Pre =>
     not This.Contains (Release.Version) or else
     raise Checked_Error with
       "Crate already contains given release: "
       & Semantic_Versioning.Image (Release.Version);

   function Contains (This    : Crate;
                      Version : Semantic_Versioning.Version) return Boolean;

   function Description (This : Crate) return Description_String;

   function Externals (This : Crate) return Alire.Externals.List;

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
         Externals : Alire.Externals.List;
         Releases  : Containers.Release_Set;
      end record;

end Alire.Crates.With_Releases;
