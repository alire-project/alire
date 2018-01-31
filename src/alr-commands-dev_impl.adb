with Alr.OS_Lib;

package body Alr.Commands.Dev_Impl is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Locate_Alr then
         Log ("Project file: " & OS_Lib.Locate_Any_Index_File);
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
   begin
      GNAT.Command_Line.Define_Switch (Config,
                                       Cmd.Locate_Alr'Access,
                                       "-l", "--locate",
                                       "Tries to locate a project file in scope.");
   end Setup_Switches;

end Alr.Commands.Dev_Impl;
