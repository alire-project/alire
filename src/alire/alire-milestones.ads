with Alire.Interfaces;

with Semantic_Versioning.Extended;

private with Alire.Utils.TTY;

package Alire.Milestones with Preelaborate is

   type Milestone (<>) is new Interfaces.Colorable with private;

   function "<" (L, R : Milestone) return Boolean;

   function New_Milestone (Name    : Crate_Name;
                           Version : Semantic_Versioning.Version)
                           return Milestone;

   function Crate (M : Milestone) return Crate_Name;

   function Version (M : Milestone) return Semantic_Versioning.Version;

   function Image (M : Milestone) return String;

   overriding
   function TTY_Image (M : Milestone) return String;

   -----------------------
   -- Milestone parsing --
   -----------------------

   type Allowed_Milestones (Len : Positive) is record
      Crate    : Alire.Crate_Name (1 .. Len);
      Versions : Semantic_Versioning.Extended.Version_Set;
   end record;

   function Crate_Versions (Spec : String) return Allowed_Milestones;
   --  Either valid set or Constraint_Error
   --  If no version was specified, Any version is returned
   --  Syntax: name[extended version set expression]

private

   package TTY renames Utils.TTY;

   type Milestone (Name_Len : Natural) is new Interfaces.Colorable with record
      Name    : Crate_Name (1 .. Name_Len);
      Version : Semantic_Versioning.Version;
   end record;

   use all type Semantic_Versioning.Version;

   function "<" (L, R : Milestone) return Boolean
   is (L.Name < R.Name
       or else
      (L.Name = R.Name and then L.Version < R.Version));

   function New_Milestone (Name    : Crate_Name;
                           Version : Semantic_Versioning.Version)
                           return Milestone
     is (Name'Length, Name, Version);

   function Crate (M : Milestone) return Crate_Name is (M.Name);

   function Version (M : Milestone) return Semantic_Versioning.Version
   is (M.Version);

   function Image (M : Milestone) return String is
     ((+M.Crate)
      & "="
      & Image (M.Version));

   overriding
   function TTY_Image (M : Milestone) return String is
     (TTY.Name (+M.Crate)
      & "="
      & TTY.Version (Image (M.Version)));

end Alire.Milestones;
