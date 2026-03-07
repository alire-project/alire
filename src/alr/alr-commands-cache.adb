with Alire.Cache;
with Alire.Directories;
with Alire.Formatting;
with Alire.Utils.Tables;

package body Alr.Commands.Cache is

   -------------
   -- Summary --
   -------------

   procedure Summary is
      use Alire.Directories;
      Table : Alire.Utils.Tables.Table;
      Usage : constant Alire.Cache.Usages := Alire.Cache.Usage;
      Size  : constant String :=
                TTY_Image (if Usage.Is_Empty
                           then 0
                           else Alire.Cache.Usage.First_Element.Size);
   begin
      if Alire.Formatting.Structured_Output then
         Table.Header ("Path").Header ("Size").New_Row;
         Table.Append (Alire.Cache.Path).Append (Size);
      else
         Table
           .Append ("Path:")
           .Append (Alire.Cache.Path)
           .New_Row;

         Table
           .Append ("Size:")
           .Append (TTY_Image (if Usage.Is_Empty
                    then 0
                    else Alire.Cache.Usage.First_Element.Size));
      end if;

      Table.Print (Trace.Always);
   end Summary;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      Summary;
   end Execute;

end Alr.Commands.Cache;
