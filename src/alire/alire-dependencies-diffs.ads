with Alire.Conditional;
with Alire.Dependencies.Containers;

package Alire.Dependencies.Diffs is

   type Diff is tagged private;

   function Between (Former, Latter : Conditional.Dependencies) return Diff;

   function Between (Former, Latter : Containers.List) return Diff;

   function Added (This : Diff) return Containers.List;

   function Removed (This : Diff) return Containers.List;

   function Contains_Changes (This : Diff) return Boolean;

   procedure Print (This : Diff);

private

   type Diff is tagged record
      Added   : Containers.List;
      Removed : Containers.List;
   end record;

end Alire.Dependencies.Diffs;
