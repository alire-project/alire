with Ada.Command_Line;

package body Alr.OS_Lib is

   --------------------------
   -- Current_Command_Line --
   --------------------------

   function Current_Command_Line return String is
      use Ada.Command_Line;

      function Append (Arg : Positive) return String is
      begin
         if Arg > Argument_Count then
            return "";
         else
            return Argument (Arg) & " " & Append (Arg + 1);
         end if;
      end Append;

   begin
      return Append (1);
   end Current_Command_Line;

   ---------------------
   -- Traverse_Folder --
   ---------------------

   procedure Traverse_Folder (Folder : String;
                              Doing   : access procedure
                                (Item : Ada.Directories.Directory_Entry_Type;
                                 Stop : in out Boolean);
                              Recurse : Boolean := False)
   is
      use Ada.Directories;

      procedure Go_Down (Item : Directory_Entry_Type) is
         Stop : Boolean := False;
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Doing (Item, Stop);
            if Stop then
               return;
            end if;

            if Recurse and then Kind (Item) = Directory then
               Traverse_Folder (Folder / Simple_Name (Item), Doing, Recurse);
            end if;
         end if;
      end Go_Down;

   begin
      Log ("Traversing folder: " & Folder, Debug);

      Search (Folder,
              "",
              (Directory => True, Ordinary_File => True, others => False),
              Go_Down'Access);
   end Traverse_Folder;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : String) is
   begin
      if GNAT.OS_Lib.Is_Regular_File (Name) then
         Log ("Deleting file: " & Name, Debug);
         Ada.Directories.Delete_File (Name);
      else
         Log ("Skipping deletion of non-existent file: " & Name, Debug);
      end if;
   end Delete_File;

   ------------
   -- Getenv --
   ------------

   function Getenv (Var : String; Default : String := "") return String is
      use GNAT.OS_Lib;

      Env_Access : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Getenv (Var);
      Env        : constant String := Env_Access.all;
   begin
      Free (Env_Access);
      if Env = "" then
         return Default;
      else
         return Env;
      end if;
   end Getenv;

   ----------------
   -- Sed_Folder --
   ----------------

   procedure Sed_Folder (Folder  : String;
                         Pattern : String;
                         Replace : String)
   is

      ------------
      -- Rename --
      ------------

      procedure Rename (Item : Ada.Directories.Directory_Entry_Type;
                        Stop : in out Boolean)
      is
         pragma Unreferenced (Stop);
         use Ada.Directories;
         use Utils;
      begin
         if Simple_Name (Item) = "." or else Simple_Name (Item) = ".." then
            return;
         end if;

         if Kind (Item) = Directory then
            Traverse_Folder (Full_Name (Item), Rename'Access);
         end if;

         if Contains (Simple_Name (Item), Pattern) then
            Log ("Filename match: " & Simple_Name (Item), Debug);
            Rename (Full_Name (Item),
                    Containing_Directory (Full_Name (Item)) /
                      Utils.Replace (Simple_Name (Item),
                        Pattern, Replace));
         end if;
      end Rename;

   begin
      --  FIXME this is OS dependent and should be made independent (or moved
      --  to OS).

      --  File contents
      declare
         use Alire.Directories;
         G : Guard (Enter (Folder)) with Unreferenced;
      begin
         Log ("sed-ing project name in files...", Debug);
         Spawn ("find", ". -type f -exec sed -i s/" &
                  Pattern & "/" & Replace & "/g {} \;",
                Force_Quiet => True);
      end;

      --  This is not OS dependent
      --  File names
      Log ("sed-ing project in file names...", Debug);
      Traverse_Folder (Folder, Rename'Access);
   end Sed_Folder;

   -------------------------------
   -- File_Contains_Ignore_Case --
   -------------------------------

   function File_Contains_Ignore_Case (Filename, Word : String)
                                       return Boolean
   is
   begin
      --  FIXME: this is OS dependent, and it shouldn't be
      return Spawn ("grep", "-q " & Word & " " &
                      Filename, Force_Quiet => True) = 0;
--      return True;
   exception
      when Command_Failed =>
         return False;
   end File_Contains_Ignore_Case;

   --------------
   -- Is_Older --
   --------------

   function Is_Older (This : String; Than : String) return Boolean is
      use GNAT.OS_Lib;
   begin
      if Is_Regular_File (This) then
         if not Is_Regular_File (Than) then
            return True;
         elsif File_Time_Stamp (This) < File_Time_Stamp (Than) then
            Trace.Debug (This & " is older than " & Than);
            return True;
         else
            return False;
         end if;
      else
         return False;
      end if;
   end Is_Older;

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command             : String;
                    Arguments           : String := "";
                    Understands_Verbose : Boolean := False;
                    Force_Quiet         : Boolean := False)
   is
      Code : constant Integer :=
        Spawn (Command, Arguments, Understands_Verbose, Force_Quiet);
   begin
      if Code /= 0 then
         raise Child_Failed with "Exit code:" & Code'Img;
      end if;
   end Spawn;

   -----------------------
   -- Spawn_And_Capture --
   -----------------------

   procedure Spawn_And_Capture (Output     : in out Utils.String_Vector;
                                Command    : String;
                                Arguments  : String := "";
                                Err_To_Out : Boolean := False)
   is
      Exit_Code : constant Integer :=
                    Alire.OS_Lib.Subprocess.Spawn_And_Capture (Output,
                                                               Command,
                                                               Arguments,
                                                               Err_To_Out);
   begin
      if Exit_Code /= 0 then
         raise Child_Failed with "exit code:" & Exit_Code'Img;
      end if;
   end Spawn_And_Capture;

   ------------------------
   -- Spawn_And_Redirect --
   ------------------------

   procedure Spawn_And_Redirect (Out_File   : String;
                                 Command    : String;
                                 Arguments  : String := "";
                                 Err_To_Out : Boolean := False)
   is
      Exit_Code : constant Integer :=
                    Alire.OS_Lib.Subprocess.Spawn_And_Redirect (Out_File,
                                                                Command,
                                                                Arguments,
                                                                Err_To_Out);
   begin
      if Exit_Code /= 0 then
         raise Child_Failed with "exit code:" & Exit_Code'Img;
      end if;
   end Spawn_And_Redirect;

   ---------------
   -- Spawn_Raw --
   ---------------

   procedure Spawn_Raw (Command   : String;
                        Arguments : String := "")
   is
      Code : Integer;
   begin
      Trace.Debug ("Spawning " & Command & " " & Arguments);

      Code := GNAT.OS_Lib.Spawn
        (Alire.OS_Lib.Subprocess.Locate_In_Path (Command),
         GNAT.OS_Lib.Argument_String_To_List (Arguments).all);

      if Code /= 0 then
         raise Child_Failed with "Exit code:" & Code'Image;
      end if;
   end Spawn_Raw;

end Alr.OS_Lib;
