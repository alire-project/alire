with AAA.Enum_Tools;
with AAA.Text_IO;

with Alire.Crates;
with Alire.Toolchains;
with Alire.Utils.Tables;
with Alire.Utils.TTY;

package body Alr.Commands.Help is

   package TTY renames Alire.Utils.TTY;

   type Help_Topics is (Identifiers,
                        Toolchains);
   --  Enumeration used to index available help topics.

   -----------------------
   -- One_Liner_Summary --
   -----------------------

   function One_Liner_Summary (Topic : Help_Topics) return String
   is (case Topic is
          when Identifiers => "Naming rules for crate and index names",
          when Toolchains  => "Configuration and use of toolchains"
      );

   -----------------
   -- Description --
   -----------------
   --  Descriptions are given as string vectors so they can be reformatted into
   --  paragraphs.

   function Description (Topic : Help_Topics) return Alire.Utils.String_Vector
   is
     (case Topic is
         when Identifiers =>
            Alire.Crates.Naming_Convention,
         when Toolchains  =>
            Alire.Toolchains.Description
     );

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help (Keyword : String) is

      ------------
      -- Format --
      ------------

      procedure Format (Text : Alire.Utils.String_Vector) is
      begin
         for Line of Text loop
            AAA.Text_IO.Put_Paragraph (Highlight_Switches (Line),
                                       Line_Prefix => "   ");
         end loop;
      end Format;

      --------------
      -- Is_Topic --
      --------------

      function Is_Topic is new AAA.Enum_Tools.Is_Valid (Help_Topics);

   begin
      if Is_Command (Keyword) then
         Display_Usage (What_Command (Keyword));

      elsif Is_Topic (Keyword) then
         Put_Line (TTY.Bold (Help_Topics'Value (Keyword)'Img));
         Format (Description (Help_Topics'Value (Keyword)));

      else
         Trace.Error ("No help found for: " & Keyword);
         Display_Global_Options;
         Display_Valid_Keywords;
         OS_Lib.Bailout (1);
      end if;
   end Display_Help;

   ----------------------------
   -- Display_Valid_Keywords --
   ----------------------------

   procedure Display_Valid_Keywords is
   begin
      New_Line;
      Display_Valid_Commands;
      New_Line;
      Display_Valid_Topics;
   end Display_Valid_Keywords;

   --------------------------
   -- Display_Valid_Topics --
   --------------------------

   procedure Display_Valid_Topics is
      Tab   : constant String (1 .. 1) := (others => ' ');
      Table : Alire.Utils.Tables.Table;
   begin
      Put_Line (TTY.Bold ("TOPICS"));
      for Topic in Help_Topics'Range loop
         Table.New_Row;
         Table.Append (Tab);
         Table.Append (TTY.Description
                         (Alire.Utils.To_Lower_Case (Topic'Img)));
         Table.Append (One_Liner_Summary (Topic));
      end loop;
      Table.Print (Always, Separator => "  ");
   end Display_Valid_Topics;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Empty_Vector)
   is
      pragma Unreferenced (Cmd);
   begin
      if Num_Arguments /= 1 then
         if Num_Arguments > 1 then
            Trace.Error ("Please specify a single help keyword");
            New_Line;
         end if;

         Put_Line (TTY.Bold ("USAGE"));
         Put_Line ("   " & TTY.Underline ("alr") & " " &
           TTY.Underline ("help") & " [<command>|<topic>]");

         New_Line;
         Put_Line (TTY.Bold ("ARGUMENTS"));
         declare
            Tab   : constant String (1 .. 1) := (others => ' ');
            Table : Alire.Utils.Tables.Table;
         begin
            Table.New_Row;
            Table.Append (Tab);
            Table.Append (TTY.Description ("<command>"));
            Table.Append ("Command for which to show a description");

            Table.New_Row;
            Table.Append (Tab);
            Table.Append (TTY.Description ("<topic>"));
            Table.Append ("Topic for which to show a description");

            Table.Print (Always, Separator => "  ");
         end;

         Display_Global_Options;

         Display_Valid_Keywords;
         OS_Lib.Bailout (1);
      end if;

      Display_Help (Argument (1));
   end Execute;

   ------------------------
   -- Highlight_Switches --
   ------------------------

   function Highlight_Switches (Line : String) return String is

      ---------------
      -- Highlight --
      ---------------
      --  Avoid highlighting non-alphanumeric characters
      function Highlight (Word : String) return String is
         subtype Valid_Chars is Character with
           Dynamic_Predicate => Valid_Chars in '0' .. '9' | 'a' .. 'z';
         I : Natural := Word'Last; -- last char to highlight
      begin
         while I >= Word'First and then Word (I) not in Valid_Chars loop
            I := I - 1;
         end loop;

         return TTY.Emph (Word (Word'First .. I)) & Word (I + 1 .. Word'Last);
      end Highlight;

      use Alire.Utils;
      use String_Vectors;
      Words : String_Vector := Split (Line, Separator => ' ');
      I, J  : String_Vectors.Cursor;
   begin
      I := Words.First;
      while Has_Element (I) loop
         declare
            Word : constant String := Element (I);
         begin
            J := Next (I);
            if Starts_With (Word, "--") and then Word'Length > 2 then
               Words.Insert (Before => J, New_Item => Highlight (Word));
               Words.Delete (I);
            end if;
            I := J;
         end;
      end loop;
      return Words.Flatten;
   end Highlight_Switches;

end Alr.Commands.Help;
