with Alire;

package Alr.Bootstrap is

   ---------------------
   --  SESSION STATE  --
   ---------------------

   type Session_States is
     (Outside,   -- Not in any Alire context
      Project,   -- Top-level folder is a single project
      Sandbox    -- Top-level folder is a sandbox with several projects
     );
   --  Sandbox mode is not implemented yet

   function Session_State return Session_States;
   --  Note that even if you're in a project within a sandbox, result is sandbox

   -------------
   --  OTHER  --
   -------------

   procedure Check_Ada_Tools;
   --  Check gprbuild/gnatmake are within reach

   procedure Checkout_Alr_Sources (To_Path : String);
   --  Checks out current master branch

   function Status_Line return String;
   --  One-liner reporting most interesting information

   function No_Dependencies return Types.Platform_Dependencies
     renames Types.No_Dependencies;

end Alr.Bootstrap;
