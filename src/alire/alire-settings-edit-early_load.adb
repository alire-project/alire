package body Alire.Settings.Edit.Early_Load is

   -------------------
   -- Load_Settings --
   -------------------

   procedure Load_Settings is
   begin
      Alire.Settings.Edit.Load_Config;
   end Load_Settings;

begin
   Load_Settings;
end Alire.Settings.Edit.Early_Load;
