package Alire.Config with Preelaborate is

   function Path return String;
   --  The in-use config path.
   --  In order of decreasing precedence:
   --  - A --config <path> given folder
   --  - An ALR_CONFIG env given folder
   --  - Default per-platform path (see alire-platforms-*)
   
   --  Detection happens during elaboration.

end Alire.Config;
