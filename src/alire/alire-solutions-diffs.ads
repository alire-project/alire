
package Alire.Solutions.Diffs is

   type Diff is tagged private;
   --  Encapsulates the differences between two solutions

   function Between (Former, Latter : Solution) return Diff;
   --  Create a Diff from two solutions

   function Contains_Changes (This : Diff) return Boolean;
   --  Says if there are, in fact, changes between both solutions

   function Latter_Is_Complete (This : Diff) return Boolean;
   --  Says if the new solution is complete

   procedure Print (This         : Diff;
                    Changed_Only : Boolean      := not Alire.Detailed;
                    Prefix       : String       := "   ";
                    Level        : Trace.Levels := Trace.Info);
   --  Print a summary of changes between two solutions. Prefix is prepended to
   --  every line.

private

   type Diff is tagged record
      Former, Latter : Solution;
   end record;

end Alire.Solutions.Diffs;
