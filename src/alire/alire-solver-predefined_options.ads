package Alire.Solver.Predefined_Options is

   Default_Options_Not_Interactive : constant Query_Options :=
                                       (On_Timeout => Stop,
                                        others     => <>);

   Complete_Only : constant Query_Options :=
                     (Exhaustive => False, -- only attempt complete ones
                      On_Timeout => Continue,
                      others     => <>);
   --  Only return a complete solution, but try for as long as it takes

   First_Incomplete : constant Query_Options :=
                        (On_Timeout => Continue_While_Complete_Then_Stop,
                         others     => <>);
   --  Intended to find a complete solution, or else return an incomplete one
   --  that helps with diagnosing the trouble. This one looks for incompletes
   --  during one timeout period after all complete have been explored without
   --  timeout.

   Exhaustive_Options : constant Query_Options :=
                          (Completeness => All_Incomplete,
                           others       => <>);
   --  Explore the full solution space

   Find_Best_Options  : constant Query_Options :=
                          (Completeness => All_Complete,
                           others       => <>);
   --  Find all complete solutions and return the "best" one (see
   --  Solutions.Is_Better). It does not yet make sense to use this setting
   --  because with the current Is_Better implementation, the first complete
   --  solution found is the one considered better anyway.

end Alire.Solver.Predefined_Options;
