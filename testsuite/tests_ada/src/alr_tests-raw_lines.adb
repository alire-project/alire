with AAA.Strings;

with Alire.Utils.Text_Files;

procedure Alr_Tests.Raw_Lines is

   --  Read_Lines and Write_Lines keep the bytes of each line as they are, and
   --  Read_Lines takes both LF and CR LF as line endings

   use AAA.Strings;

   package Text_Files renames Alire.Utils.Text_Files;

   File : constant String := "alr_tests_raw_lines.txt";

   CR : constant Character := Character'Val (13);
   LF : constant Character := Character'Val (10);

   E_Acute : constant String := Character'Val (16#C3#) & Character'Val (16#A9#);

   function Read (Contents : String) return Vector is
   begin
      Text_Files.Write_Lines (File, To_Vector (Contents));
      --  Written as one line plus a native line ending, which Read_Lines
      --  removes too
      return Text_Files.Read_Lines (File);
   end Read;

begin
   pragma Assert (Read ("a" & CR & LF & "b" & CR & LF)
                  = To_Vector ("a").Append ("b").Append (""));
   pragma Assert (Read ("a" & LF & "b") = To_Vector ("a").Append ("b"));
   pragma Assert (Read ("a" & CR & "b") = To_Vector ("a" & CR & "b"));
   pragma Assert (Read ("") = To_Vector (""));
   pragma Assert (Read ("C" & E_Acute & "sar") = To_Vector ("C" & E_Acute & "sar"));

   --  Lines round trip, with native line endings

   Text_Files.Write_Lines (File, To_Vector ("one").Append ("").Append ("three"));
   pragma Assert (Text_Files.Read_Lines (File)
                  = To_Vector ("one").Append ("").Append ("three"));
end Alr_Tests.Raw_Lines;
