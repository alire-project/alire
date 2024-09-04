with AAA.Table_IO;

package Alire.Utils.Tables with Preelaborate is

   subtype Parent is AAA.Table_IO.Table;

   type Table is new Parent with null record;

   overriding
   procedure Header (T : in out Table; Cell : String);

   overriding
   function Header (T    : aliased in out Table;
                    Cell : String)
                    return AAA.Table_IO.Reference;

   procedure Print (T         : Table;
                    Level     : Trace.Levels            := Info;
                    Separator : String                  := " ";
                    Align     : AAA.Table_IO.Alignments := (1 .. 0 => <>));
   --  Hook so tables use the default output facilities of Alire

end Alire.Utils.Tables;
