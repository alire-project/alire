package Alr.OS_Lib is

   -- Things that are OS dependant but already abstracted by GNAT, so not platform-specific
   -- Platform-specific things are in Alire.OS

   function Command (Name : String) return String;
   --  Given a command name, e.g. "git", returns its canonical executable path, e.g. "/usr/bin/git"

   procedure GPR_Rebuild (Folder : String);
   --  gprbuild the default project in given folder

   function Spawn (Command : String;
                   Arg1    : String := "";
                   Arg2    : String := "";
                   Arg3    : String := "") return Integer;
   -- Wraps the pointer hell in GNAT.OS_Lib
   -- Returns the Command exit code

   procedure Spawn (Command : String;
                    Arg1    : String := "";
                    Arg2    : String := "";
                    Arg3    : String := "");
   --  As the function, but raises exception if exit code was <> 0

end Alr.OS_Lib;
