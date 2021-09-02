with AAA.Table_IO;

package Alire.Utils.Tables with Preelaborate is

   type Table is new AAA.Table_IO.Table with null record;

   procedure Header (T : in out Table; Cell : String);

   procedure Print (T         : Table;
                    Level     : Trace.Levels            := Info;
                    Separator : String                  := " ";
                    Align     : AAA.Table_IO.Alignments := (1 .. 0 => <>));
   --  Hook so tables use the default output facilities of Alire

end Alire.Utils.Tables;
