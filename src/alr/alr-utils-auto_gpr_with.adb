with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;

with Alire.Directories;
with Alire.Utils.User_Input;
with Alire.Config;
with Alire.Config.Edit;

package body Alr.Utils.Auto_GPR_With is

   Begin_Line : constant String := "-- begin auto-gpr-with --";
   End_Line   : constant String := "-- end auto-gpr-with --";

   ----------------
   -- Query_User --
   ----------------

   function Query_User return Boolean is
      use Alire.Utils.User_Input;
      use Alire.Config;

      Result : Boolean;
   begin
      --  First check if user already configured the auto-gpr-with policy
      if Defined ("auto-gpr-with") then

         Result := Get ("auto-gpr-with", False);

         if Result then
            Trace.Info ("Auto-gpr-with enabled by configuration.");
         else
            Trace.Info ("Auto-gpr-with disabled by configuration.");
         end if;

         return Result;
      end if;

      --  If not, ask the user
      Result := Query
        (Question => "Do you want Alire to automatically update your project" &
           " file with the new dependency solution?",
         Valid    => (Yes | No => True, others => False),
         Default  => Yes) = Yes;

      --  Offer to save this choice in configuration
      if Query
        (Question => "Do you want Alire to remember this choice?",
         Valid    => (Yes | No => True, others => False),
         Default  => No) = Yes
      then
         Edit.Set (Alire.Config.Filepath (Global), "auto-gpr-with",
                   (if Result then "true" else "false"));
      end if;

      if not Result then
         Trace.Detail ("Auto-gpr-with rejected by user.");
      end if;

      return Result;
   end Query_User;

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

      if not Query_User then
         return;
      end if;

      Open (In_File, Ada.Text_IO.In_File, GPR_File);
      Create (Out_File, Ada.Text_IO.Out_File, Tmp.Filename);

      --  First add the new auto-with section to the output file, if any
      if not Withs.Is_Empty then
         Put_Line (Out_File, Begin_Line);
         Put_Line (Out_File,
                   "--  This section was automatically added by Alire");
         for File of Withs loop
            Put_Line (Out_File, "with """ & File & """;");
         end loop;
         Put_Line (Out_File, End_Line);
      end if;

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
