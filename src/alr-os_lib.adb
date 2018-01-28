with Ada.Directories;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.OS_Lib is

   -------------
   -- Command --
   -------------

   function Command (Name : String) return String is
      Target : String_Access := Locate_Exec_On_Path (Name);
   begin
      if Target /= null then
         return Result : constant String := Target.all do
            Free (Target);
         end return;
      else
         raise Program_Error with "Couldn't locate " & Name & " in PATH";
      end if;
   end Command;


   --------------------
   -- Git_Executable --
   --------------------

   Git : GNAT.OS_Lib.String_Access;

   -----------------
   -- GPR_Rebuild --
   -----------------

   procedure GPR_Rebuild (Folder : String) is
   begin
      Ada.Directories.Set_Directory (Folder);
      Spawn (Command ("gprbuild"));
   end GPR_Rebuild;

   -----------
   -- Spawn --
   -----------

   function Spawn
     (Command : String;
      Arg1    : String := "";
      Arg2    : String := "";
      Arg3    : String := "")
      return Integer
   is
      Arg_Count : Natural;
      Args      : Argument_List (1 .. 3);
   begin
      if Arg3 /= "" then
         Arg_Count := 3;
      elsif Arg2 /= "" then
         Arg_Count := 2;
      elsif Arg1 /= "" then
         Arg_Count := 1;
      else
         Arg_Count := 0;
      end if;

      --  FIXME free that memory if we care at all...
      Args (1) := new String'(Arg1);
      Args (2) := new String'(Arg2);
      Args (3) := new String'(Arg3);

      return Spawn (Command, Args (1 .. Arg_Count));
   end Spawn;

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command : String;
                    Arg1    : String := "";
                    Arg2    : String := "";
                    Arg3    : String := "")
   is
      Code : constant Integer := Spawn (Command, Arg1, Arg2, Arg3);
   begin
      if Code /= 0 then
         raise Program_Error with Command & " failed with exit code" & Code'Image;
      end if;
   end Spawn;

end Alr.OS_Lib;
