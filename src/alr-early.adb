with Ada.Command_Line;

package body Alr.Early is

----------------------------
-- Early_Switch_Detection --
----------------------------

   procedure Early_Switch_Detection is
      use Ada.Command_Line;
      Found : Natural := 0;
   begin
      for I in 1 .. Argument_Count loop
         if Argument (I) = "-d" then
            Simple_Logging.Level := Debug;
            Found := Found + 1;
         elsif Argument (I) = "-v" then
            Simple_Logging.Level := Detail;
            Found := Found + 1;
         elsif Argument (I) = "-q" then
            Simple_Logging.Level := Warning;
            Found := Found + 1;
         end if;
      end loop;

      if Found > 1 then
         Log ("Option -d, -q and -v are mutually exclusive, defaulting to normal verbosity", Warning);
      end if;
   end Early_Switch_Detection;

begin
   Early_Switch_Detection;
end Alr.Early;
