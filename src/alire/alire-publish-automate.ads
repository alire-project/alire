private package Alire.Publish.Automate is

   --  Steps for the assistant, not intended to be called directly

   procedure Exists (Context : in out Data);
   --  Check if there's a PR already for this release

   procedure Fork (Context : in out Data);
   --  Fork the community index to the user account

   procedure Clone (Context : in out Data);
   --  Clone the user fork to our local cache

   procedure Push (Context : in out Data);
   --  Commit and push the generated manifest to the user fork

   procedure Submit (Context : in out Data);
   --  Create the actual PR against the community repo

end Alire.Publish.Automate;
