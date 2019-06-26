with Alire.Environment;
with Alire.Platform;

with GNAT.IO;

package body Alire.Config is

   type String_Access is access String;
   Config_Path : String_Access;

   ---------------------
   -- Enter_Or_Ctrl_C --
   ---------------------

   procedure Enter_Or_Ctrl_C is
      use GNAT.IO;
      Foo : String := "bar";
      Bar : Integer;
   begin
      if not Interactive then
         Trace.Detail ("Non-interactive session, continuing");
      else
         Put_Line ("Press Enter to continue or Ctrl-C to abort");
         Get_Line (Foo, Bar);
      end if;
   end Enter_Or_Ctrl_C;

   ----------
   -- Path --
   ----------

   function Path return String is
   begin
      if Config_Path /= null then -- Case with switch (TODO)
         return Config_Path.all;
      else
         return OS_Lib.Getenv (Environment.Config,
                               Platform.Default_Config_Folder);
      end if;
   end Path;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path (Path : String) is
   begin
      if Config_Path /= null then
         raise Constraint_Error with "Custom path already set";
      else
         Config_Path := new String'(Path);
      end if;
   end Set_Path;

end Alire.Config;
