with Ada.Containers;
with GNAT.IO;

package body Table_IO is

   use all type Ada.Containers.Count_Type;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Table; Cell : String) is
   begin
      if T.Rows.Is_Empty then
         T.New_Row;
      end if;

      if Natural (T.Max_Widths.Length) < T.Next_Column then
         T.Max_Widths.Append (Cell'Length);
      else
         T.Max_Widths (T.Next_Column) :=
           Natural'Max (Cell'Length, T.Max_Widths (T.Next_Column));
      end if;

      T.Rows (Natural (T.Rows.Length)).Append (Cell);
      T.Next_Column := T.Next_Column + 1;
   end Append;

   -------------
   -- New_Row --
   -------------

   procedure New_Row (T : in out Table) is
   begin
      T.Next_Column := 1;
      T.Rows.Append (String_Vectors.Empty_Vector);
   end New_Row;

   -----------
   -- Print --
   -----------

   procedure Print (T : Table; Separator : String := " ") is
      use Gnat.IO;
   begin
      for Row of T.Rows loop
         for I in 1 .. Natural (Row.Length) loop
            Put (Row (I));
            Put (String'(1 .. T.Max_Widths (I) - String'(Row (I))'Length => ' '));
            if I < Natural (Row.Length) then
               Put (Separator);
            else
               New_Line;
            end if;
         end loop;
      end loop;
   end Print;

end Table_IO;
