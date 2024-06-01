package Alire.Solver.Predefined_Options is

   Default_Options : Query_Options renames Solver.Default_Options;

   Best_Effort : constant Query_Options :=
                   (Stopping => Stop,
                    others   => <>);
   --  Look until first timeout

   Interactive : constant Query_Options :=
                   (Stopping => Ask,
                    others   => <>);
   --  Ask of timeout

   Exhaustive  : constant Query_Options :=
                   (Stopping   => Continue,
                    others     => <>);
   --  Try as long as it takes to find a complete solution

end Alire.Solver.Predefined_Options;
