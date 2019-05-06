package Alr.Interactive with Preelaborate is

   Not_Interactive : aliased Boolean := False;
   --  When not interactive, alr will press ahead without user confirmations
   --  with the default option.

   procedure Enter_Or_Ctrl_C;
   --  If interactive will show the message and wait for enter; or do nothing
   --  otherwise.

end Alr.Interactive;
