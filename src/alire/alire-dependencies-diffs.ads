with Alire.Conditional;
with Alire.Dependencies.Containers;

package Alire.Dependencies.Diffs is

   type Diff is tagged private;

   function Between (Former, Latter : Conditional.Dependencies) return Diff;

   function Between (Former, Latter : Containers.Lists.List) return Diff;

   function Contains_Changes (This : Diff) return Boolean;

   procedure Print (This : Diff);

private

   type Diff is tagged record
      Added   : Containers.Lists.List;
      Removed : Containers.Lists.List;
   end record;

end Alire.Dependencies.Diffs;
