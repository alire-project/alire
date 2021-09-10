with Alire.Config.Edit;
with CLIC.User_Input; use CLIC.User_Input;

package body Alire.Utils.User_Input.Query_Config is

   ----------------------------
   -- Config_Or_Query_String --
   ----------------------------

   function Config_Or_Query_String (Config_Key : String;
                                    Question   : String;
                                    Default    : String;
                                    Validation : String_Validation_Access)
                                    return String
   is
      use Alire.Config;
   begin
      if Config.DB.Defined (Config_Key) then
         return Config.DB.Get (Config_Key, Default);
      else
         declare
            Result : constant String :=
              Query_String (Question, Default, Validation);
         begin
            if Result /= Default then
               Alire.Config.Edit.Set_Globally (Config_Key, Result);
            end if;

            return Result;
         end;
      end if;
   end Config_Or_Query_String;

   ---------------
   -- User_Name --
   ---------------

   function User_Name return String
   is (Config_Or_Query_String (Config_Key => "user.name",
                               Question   => "Please enter your full name:",
                               Default    => "Your Name",
                               Validation => null));

   -----------------------
   -- User_GitHub_Login --
   -----------------------

   function User_GitHub_Login return String
   is (Config_Or_Query_String (Config_Key => "user.github_login",
                               Question   => "Please enter your GitHub login:",
                               Default    => "github-username",
                               Validation => Is_Valid_GitHub_Username'Access));

   -----------------
   -- Check_Email --
   -----------------

   function Check_Email (Str : String) return Boolean
   is (Could_Be_An_Email (Str, With_Name => False));

   ----------------
   -- User_Email --
   ----------------

   function User_Email return String
   is (Config_Or_Query_String
       (Config_Key => "user.email",
        Question   => "Please enter your email address:",
        Default    => "example@example.com",
        Validation => Check_Email'Access));

end Alire.Utils.User_Input.Query_Config;
