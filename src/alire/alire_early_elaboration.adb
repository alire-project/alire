with Alire; -- Kind of circularity here but somehow it slips past static GNAT rules??

with GNAT.Command_Line;
with GNAT.OS_Lib;

with Simple_Logging;

package body Alire_Early_Elaboration is

   ----------------------------
   -- Early_Switch_Detection --
   ----------------------------

   procedure Early_Switch_Detection is
      use GNAT.Command_Line;
   begin

      --  We use the simpler Getopt form to avoid built-in help and other shenanigans
      begin
         loop
            case Getopt ("* d q v") is
               when ASCII.NUL => exit;
               when 'd' => Switch_D := True;
               when 'q' => Switch_Q := True;
               when 'v' => Switch_V := True;
               when others => null;
            end case;
         end loop;
      exception
         when Exit_From_Command_Line =>
            --  Something unexpected happened but it will be properly dealt with later on,
            --  in the regular command-line parser
            null;
      end;

      --  Exclusivity check
      if (Switch_D and Switch_V) or (Switch_D and Switch_Q) or (Switch_V and Switch_Q) then
         Alire.Trace.Info ("Ada Library Repository manager (alr)");
         Alire.Trace.Error ("Only one verbosity switch allowed (either -d, -v or -q)");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      --  Level setting
      if Switch_D then
         Alire.Log_Level := Simple_Logging.Debug;
      elsif Switch_V then
         Alire.Log_Level := Simple_Logging.Detail;
      elsif Switch_Q then
         Alire.Log_Level := Simple_Logging.Error;
      end if;
   end Early_Switch_Detection;

begin
   Early_Switch_Detection;
end Alire_Early_Elaboration;
