private package Alire.Publish.Automate is

   --  Steps for the assistant, not intended to be called directly

   procedure Fork (Context : in out Data);

   procedure Clone (Unused_Context : in out Data);

   procedure Push (Context : in out Data);

   procedure Submit (Context : in out Data);

end Alire.Publish.Automate;
