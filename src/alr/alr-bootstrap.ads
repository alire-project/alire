with Alire.Types;

package Alr.Bootstrap is

   ---------------------
   --  SESSION STATE  --
   ---------------------

   type Session_States is
     (Outside,   -- Not in any Alire context
      Broken,    -- Top-level folder is a single release, invalid TOML file
      Release,   -- Top-level folder is a single release
      Sandbox    -- Top-level folder is a sandbox with several releases
     );
   --  Sandbox mode is not implemented yet

   subtype Valid_Session_States is Session_States range Release .. Sandbox;

   function Session_State return Session_States;
   --  Note that even if you're in a release within a sandbox, result is
   --  sandbox.

   -------------
   --  OTHER  --
   -------------

   function Status_Line return String;
   --  One-liner reporting most interesting information

   function No_Dependencies return Alire.Types.Platform_Dependencies
     renames Alire.Types.No_Dependencies;

end Alr.Bootstrap;
