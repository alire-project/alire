with Alire;
--  Kind of circularity here but somehow it slips past static GNAT rules??

with GNAT.Command_Line;
with GNAT.OS_Lib;

with Simple_Logging;

package body Alire_Early_Elaboration is

   ----------------------------
   -- Early_Switch_Detection --
   ----------------------------

   procedure Early_Switch_Detection is
      use GNAT.Command_Line;

      --------------------
      -- Check_Switches --
      --------------------

      procedure Check_Switches is
      begin
         loop
            --  We use the simpler Getopt form to avoid built-in help and other
            --  shenanigans.
            case Getopt ("* d --debug q v") is
               when ASCII.NUL =>
                  exit;
               when '*' =>
                  if Full_Switch = "--debug" then
                     Switch_D := True;
                  end if;
               when 'd' =>
                  Switch_D := True;
               when 'q' =>
                  Switch_Q := True;
               when 'v' =>
                  if Switch_V and then not Switch_VV then
                     Switch_VV := True;
                     Switch_V  := False;
                  elsif not (Switch_V or else Switch_VV) then
                     Switch_V := True;
                  else
                     Alire.Trace.Error ("Only one or two -v allowed");
                     GNAT.OS_Lib.OS_Exit (1);
                  end if;
               when others =>
                  null;
            end case;
         end loop;
      exception
         when Exit_From_Command_Line =>
            --  Something unexpected happened but it will be properly dealt
            --  with later on, in the regular command-line parser.
            null;
      end Check_Switches;

   begin
      Check_Switches;

      --  Exclusivity check
      if (Switch_Q and Switch_V) or (Switch_Q and Switch_VV)
      then
         Alire.Trace.Error
           ("Use only one of -q or -v");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      --  Level setting
      if Switch_VV then
         Alire.Log_Level := Simple_Logging.Debug;
      elsif Switch_V then
         Alire.Log_Level := Simple_Logging.Detail;
      elsif Switch_Q then
         Alire.Log_Level := Simple_Logging.Error;
      end if;

      --  Debug channel
      if Switch_D then
         Alire.Log_Debug := True;
      end if;
   end Early_Switch_Detection;

begin
   Early_Switch_Detection;
end Alire_Early_Elaboration;
