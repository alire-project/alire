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

   ------------------
   -- Replace_Info --
   ------------------

   procedure Replace_Info
     (Filename    : String;
      --  File in which to look for patterns
      Trigger     : String;
      --  Substring that will trigger replacement in a line. The only text in
      --  quotes will be replaced.
      Replacement : String)
      --  Replacement
   is
      F : Ada.Text_IO.File_Type;
      O : Ada.Text_IO.File_Type;
      use Ada.Text_IO;

      type Hit_Kind is (
         Replaced,
         --  A hit was recorded and a replacement is needed
         Identical,
         --  A hit was recorded but no replacement is necessary
         None
         --  No hit recorded
      );
      Hit : Hit_Kind := None;
   begin
      Open (F, In_File, Filename);
      Create (O, Out_File, Filename & ".new");
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            if Hit = None and then
              (for some I in Line'Range =>
                 I + Trigger'Length - 1 <= Line'Last and then
               Line (I .. I + Trigger'Length - 1) = Trigger)
            then
               declare
                  Ini : Integer := Line'First - 1;
                  Fin : Integer := Line'First - 1;
               begin
                  for I in Line'Range loop
                     if Line (I) = '"' then
                        if Ini < Line'First then
                           Ini := I;
                        elsif Fin < Line'First then
                           Fin := I;
                        else
                           raise Constraint_Error
                             with "Too many quotes in line: " & Line;
                        end if;
                     end if;
                  end loop;

                  if Ini < Line'First or else Fin < Line'First then
                     raise Constraint_Error
                       with "No quoted string in line: " & Line;
                  end if;

                  Put_Line
                    (O,
                     Line (Line'First .. Ini)
                     & Replacement
                     & Line (Fin .. Line'Last));

                  if Line (Ini + 1 .. Fin - 1) = Replacement then
                     Hit := Identical;
                  else
                     Hit := Replaced;
                  end if;
               end;
            else
               Put_Line (O, Line);
            end if;
         end;
      end loop;

      Close (F);
      Close (O);

      case Hit is
         when None =>
            raise Constraint_Error
              with "Trigger not found in file: " & Trigger;

         when Replaced =>
            Ada.Directories.Delete_File (Filename);
            Ada.Directories.Rename (Filename & ".new", Filename);

         when Identical =>
            Ada.Directories.Delete_File (Filename & ".new");
      end case;

   end Replace_Info;

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
        := (if Git_Command ("diff-index --quiet HEAD --").Code /= 0 then
               "dirty"
            else
               "clean");
      Commit_Result : constant Result :=
                        Git_Command ("rev-parse HEAD");
      Commit : constant String := To_String (Commit_Result.Output);
   begin
      if Commit_Result.Code /= 0 then
         raise Constraint_Error with
           "Git error while trying to get commit:"
           & Commit_Result.Code'Image;
      end if;
      Ada.Text_IO.Put_Line
        ("Updating src/alire/alire-meta.ads to commit [" & Commit
         & "] with status [" & Dirty & "]");
      Replace_Info ("src/alire/alire-meta.ads",
                    "Commit",
                    Commit);
      Replace_Info ("src/alire/alire-meta.ads",
                    "Changes",
                    Dirty);
   end;
end Version_Patcher;
