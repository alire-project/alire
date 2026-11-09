with Alire.OS_Lib;

with CLIC.User_Input;

with GNAT.IO;

package body Alr.OS_Lib is

   -------------
   -- Bailout --
   -------------

   procedure Bailout (Code : Integer := 0) is
   begin
      if Pause_Before_Exit and then not CLIC.User_Input.Not_Interactive then
         Trace.Info ("Press enter to continue...");
         declare
            Line : String (1 .. 1024);
            Last : Natural;
         begin
            GNAT.IO.Get_Line (Line, Last);
         end;
      end if;

      Alire.OS_Lib.Bailout (Code);
   end Bailout;

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
end Alr.OS_Lib;
