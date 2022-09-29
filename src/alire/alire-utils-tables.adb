with Ada.Strings.Fixed;

package body Alire.Utils.Tables is

   ------------
   -- Header --
   ------------

   procedure Header (T : in out Table; Cell : String) is
   begin
      T.Append (TTY.Emph (AAA.Strings.To_Upper_Case (Cell)));
   end Header;

   -----------
   -- Print --
   -----------

   procedure Print (T         : Table;
                    Level     : Trace.Levels            := Info;
                    Separator : String                  := " ";
                    Align     : AAA.Table_IO.Alignments := (1 .. 0 => <>))
   is

      procedure Print (Line : String) is
         use Ada.Strings;
         Trim : String renames Fixed.Trim (Line, Right);
      begin
         Trace.Log (Trim, Level);
      end Print;

   begin
      T.Print (Separator => Separator,
               Align     => Align,
               Put_Line  => Print'Access);
   end Print;

end Alire.Utils.Tables;
