package body Alr.OS_Lib is

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
      Trace.Debug ("Traversing folder: " & Folder);

      Search (Folder,
              "",
              (Directory => True, Ordinary_File => True, others => False),
              Go_Down'Access);
   end Traverse_Folder;

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
