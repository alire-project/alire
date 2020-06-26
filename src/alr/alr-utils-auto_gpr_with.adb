with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;

with Alire.Directories;

package body Alr.Utils.Auto_GPR_With is

   Begin_Line : constant String := "-- begin auto-with --";
   End_Line   : constant String := "-- end auto-with --";

   ------------
   -- Update --
   ------------

   procedure Update (GPR_File : Alire.Absolute_Path;
                     Withs    : Alire.Utils.String_Set)
   is
      In_File : Ada.Text_IO.File_Type;
      Out_File : Ada.Text_IO.File_Type;
      Tmp : Alire.Directories.Temp_File;

      Skip : Boolean := False;
   begin
      Open (In_File, Ada.Text_IO.In_File, GPR_File);
      Create (Out_File, Ada.Text_IO.Out_File, Tmp.Filename);

      --  First add the new auto-with section to the output file
      Put_Line (Out_File, Begin_Line);
      Put_Line (Out_File, "--  This section was automatically added by Alire");
      for File of Withs loop
         Put_Line (Out_File, "with """ & File & """;");
      end loop;
      Put_Line (Out_File, End_Line);

      --  Copy the content of the project file except the auto-with section,
      --  if any.
      loop
         exit when End_Of_File (In_File);

         declare
            In_Line : constant String := Get_Line (In_File);
         begin

            if In_Line = Begin_Line then
               Skip := True;
            end if;

            if Skip then
               Trace.Debug ("Autowith skip: " & In_Line);
               if In_Line = End_Line then
                  Skip := False;
               end if;
            else
               Put_Line (Out_File, In_Line);
            end if;
         end;
      end loop;

      Close (In_File);
      Close (Out_File);

      Alire.Directories.Backup_If_Existing (GPR_File);
      Ada.Directories.Copy_File (Tmp.Filename, GPR_File);
   end Update;

end Alr.Utils.Auto_GPR_With;
