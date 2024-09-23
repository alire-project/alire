with Ada.Strings.Fixed;

package body Alire.Utils.Tables is

   ------------
   -- Header --
   ------------

   overriding
   procedure Header (T : in out Table; Cell : String) is
      Text : constant String :=
               (if Structured_Output
                then AAA.Strings.To_Lower_Case (Cell)
                else TTY.Emph (AAA.Strings.To_Upper_Case (Cell)));
   begin
      Parent (T).Header (Text);
   end Header;

   ------------
   -- Header --
   ------------

   overriding
   function Header (T    : aliased in out Table;
                    Cell : String)
                    return AAA.Table_IO.Reference
   is
   begin
      T.Header (Cell);
      return AAA.Table_IO.Reference'(Table => T'Access);
   end Header;

   -----------
   -- Print --
   -----------

   procedure Print (T          : Table;
                    Level      : Trace.Levels            := Info;
                    Separator  : String                  := " ";
                    Align      : AAA.Table_IO.Alignments := (1 .. 0 => <>);
                    Structured : Boolean := Structured_Output)
   is

      procedure Print (Line : String) is
         use Ada.Strings;
         Trim : String renames Fixed.Trim (Line, Right);
      begin
         Trace.Log (Trim, Level);
      end Print;

   begin
      if Structured then
         T.Print (Structured_Output_Format,
                  Put_Line => Print'Access);
      else
         T.Print (Separator => Separator,
                  Align     => Align,
                  Put_Line  => Print'Access);
      end if;
   end Print;

end Alire.Utils.Tables;
