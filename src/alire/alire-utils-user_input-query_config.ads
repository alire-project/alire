with CLIC.User_Input;

package Alire.Utils.User_Input.Query_Config is

   use type CLIC.User_Input.String_Validation_Access;

   function Config_Or_Query_String
     (Config_Key : String;
      Question   : String;
      Default    : String;
      Validation : CLIC.User_Input.String_Validation_Access)
      return String
     with Pre => Validation = null or else Validation (Default);
   --  Same as Query_String but first looks for a configuration value before
   --  querying the user. If the answer is different from Default, it is saved
   --  in the global configuration.

   ---------------------
   -- Query or config --
   ---------------------
   --  The following function will get their value from:
   --   - The config if defined
   --   - The user if in interactive mode
   --   - A default value otherwise

   function User_Name return String;

   function User_GitHub_Login return String
     with Post =>
       (User_GitHub_Login'Result = ""
        or else Is_Valid_GitHub_Username (User_GitHub_Login'Result));

   function User_Email return String
     with Post => Could_Be_An_Email (User_Email'Result, With_Name => False);

end Alire.Utils.User_Input.Query_Config;
