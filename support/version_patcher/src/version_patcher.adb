with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Expect;
with GNAT.OS_Lib;

---------------------
-- Version_Patcher --
---------------------

procedure Version_Patcher is

   ---------------------
   -- Replace_Version --
   ---------------------

   procedure Replace_Version (Filename : String; Build_Info : String) is
      F : Ada.Text_IO.File_Type;
      O : Ada.Text_IO.File_Type;
      use Ada.Text_IO;

      Target : constant String := "Current_Str : constant String :=";
   begin
      Open (F, In_File, Filename);
      Create (O, Out_File, Filename & ".new");
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            if (for some I in Line'Range =>
                  I + Target'Length - 1 <= Line'Last and then
                  Line (I .. I + Target'Length - 1) = Target)
            then
               declare
                  Quotes_Seen : Boolean := False;
               begin
                  for Char of Line loop
                     if Char = '"' and then not Quotes_Seen then
                        Quotes_Seen := True;
                        Put (O, Char);
                     elsif (Char = '"' and then Quotes_Seen)
                       or else Char = '+'
                     then
                        Put_Line (O, "+" & Build_Info & '"' & ";");
                        exit;
                     else
                        Put (O, Char);
                     end if;
                  end loop;
               end;
            else
               Put_Line (O, Line);
            end if;
         end;
      end loop;

      Close (F);
      Close (O);

      Ada.Directories.Delete_File (Filename);
      Ada.Directories.Rename (Filename & ".new", Filename);

   end Replace_Version;

   -----------------
   -- Git_Command --
   -----------------

   type Result is record
      Output : Unbounded_String;
      Code   : Integer;
   end record;

   function Git_Command (Args : String) return Result is
      use GNAT.OS_Lib;
      Arg_List : constant Argument_List_Access :=
                   Argument_String_To_List (Args);
      Code : aliased Integer;
      Output   : constant String
        := GNAT.Expect.Get_Command_Output
          ("git", Arg_List.all, "", Code'Access, True);
   begin
      return (To_Unbounded_String (Output), Code);
   end Git_Command;

begin
   if Exists ("ALR_VERSION_DONT_PATCH") then
      Ada.Text_IO.Put_Line ("Note: skipping version update");
      return;
   end if;

   declare
      Dirty : constant String
        := (if Argument_Count > 0 then
               Argument (1)
            elsif Git_Command ("diff-index --quiet HEAD --").Code /= 0 then
               "_dirty"
            else
               "");
      Commit : constant String
        := To_String (Git_Command ("rev-parse --short HEAD").Output);
   begin
      Ada.Text_IO.Put_Line
        ("Updating version in src/alire/alire-version.ads to commit "
         & Commit & Dirty & "...");
      Replace_Version ("src/alire/alire-version.ads", Commit & Dirty);
   end;
end Version_Patcher;
