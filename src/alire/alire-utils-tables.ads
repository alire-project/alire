with AAA.Table_IO;

with LML;

package Alire.Utils.Tables is

   subtype Formats is LML.Formats;

   Structured_Output        : Boolean := False;

   Structured_Output_Format : Formats;

   subtype Parent is AAA.Table_IO.Table;

   type Table is new Parent with null record;

   overriding
   procedure Header (T : in out Table; Cell : String);

   overriding
   function Header (T    : aliased in out Table;
                    Cell : String)
                    return AAA.Table_IO.Reference;

   procedure Print (T          : Table;
                    Level      : Trace.Levels            := Info;
                    Separator  : String                  := " ";
                    Align      : AAA.Table_IO.Alignments := (1 .. 0 => <>);
                    Structured : Boolean := Structured_Output);
   --  Hook so tables use the default output facilities of Alire. When
   --  Structured_Output is enabled, formatting information is ignored.

end Alire.Utils.Tables;
