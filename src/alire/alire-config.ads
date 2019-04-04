package Alire.Config with Preelaborate is

   --  For Alire to be usable as a library, options here aren't parsed
   --  from command-line but set by someone else that has done the parsing.
   --  Right now the only client is alr.
   
   function Path return String;
   --  The in-use config path.
   --  In order of decreasing precedence:
   --  * A manually set path with Set_Path (below)
   --  * An ALR_CONFIG env given folder
   --  * Default per-platform path (see alire-platforms-*)
   
   --  Detection happens during elaboration.
   
   procedure Set_Path (Path : String);
   --  Override

end Alire.Config;
