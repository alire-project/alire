with Alr.OS_Lib;

package body Alr.Spawn is

   -------------
   -- Command --
   -------------

   procedure Command (Cmd                 : String;
                      Args                : String := "";
                      Understands_Verbose : Boolean := False;
                      Force_Quiet         : Boolean := False) is
   begin
      if OS_Lib.Spawn (Cmd,
                       Args,
                       Understands_Verbose,
                       Force_Quiet) /= 0
      then
         raise Child_Failed;
      end if;
   end Command;

   --------------
   -- Gprbuild --
   --------------

   procedure Gprbuild (Project_File        : String;
                       Extra_Args          : String := "")
   is
   begin
      Command ("gprbuild",
               "-gnatwU -j0 -p " &
               --  Supress warnings on unused (may happen in prj_alr.ads)
                 Extra_Args & (if Extra_Args /= "" then " " else "") &
                 "-P " & Project_File,
               Understands_Verbose => True);
   end Gprbuild;

end Alr.Spawn;
