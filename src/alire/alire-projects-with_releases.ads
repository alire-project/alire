with Alire.Interfaces;
with Alire.Containers;
with Alire.TOML_Adapters;

package Alire.Projects.With_Releases with Preelaborate is

   type Crate (<>) is new General and Interfaces.Detomifiable
   with private;
   --  A complete crate with its releases.

   function New_Crate (Name : Alire.Project) return Crate;

   function Name (This : Crate) return Alire.Project;

   function Description (This : Crate) return Description_String;

   function Releases (This : Crate) return Containers.Release_Set;

   overriding
   function From_TOML (This : in out Crate;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome;

   --  Child crate extras  --

   function Is_Child (This : Crate) return Boolean;

   function Parent_Name (This : Crate) return Alire.Project with
     Pre => This.Is_Child;

private

   type Crate (Len : Natural) is new General and
     Interfaces.Detomifiable with
   record
      Name     : Alire.Project (1 .. Len);
      Releases : Containers.Release_Set;
   end record;

end Alire.Projects.With_Releases;
