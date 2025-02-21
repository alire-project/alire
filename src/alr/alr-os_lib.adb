with Alire.OS_Lib.Subprocess;

package body Alr.OS_Lib is

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

   ---------------
   -- Spawn_Raw --
   ---------------

   procedure Spawn_Raw (Command   : String;
                        Arguments : String := "")
   is
      Code : Integer;
   begin
      Trace.Debug ("Spawning " & Command & " " & Arguments);

      declare
         Full_Path : constant String :=
                       Alire.OS_Lib.Subprocess.Locate_In_Path (Command);
      begin
         if Full_Path = "" then
            Alire.Raise_Checked_Error
              ("Executable not found in PATH when spawning: "
               & TTY.Terminal (Command & " " & Arguments));
         end if;

         Code := GNAT.OS_Lib.Spawn
           (Full_Path,
            GNAT.OS_Lib.Argument_String_To_List (Arguments).all);
      end;

      if Code /= 0 then
         raise Child_Failed with "Exit code:" & Code'Image;
      end if;
   end Spawn_Raw;

end Alr.OS_Lib;
