with AnsiAda;
with CLIC.TTY;

package Alire.Utils.TTY with Preelaborate is

   package ANSI renames AnsiAda;

   function Alr return String is (CLIC.TTY.Terminal ("alr"));

   function Name (Crate : Crate_Name) return String is
     (CLIC.TTY.Bold (+Crate));

   function Name (Crate : String) return String is
     (CLIC.TTY.Bold (Crate));

   function Description (Text : String) return String is
     (CLIC.TTY.Format (Text,
                      Fore  => ANSI.Light_Cyan));

   function Version (Text : String) return String is
     (CLIC.TTY.Format (Text,
                       Fore  => ANSI.Magenta,
                       Style => ANSI.Bright));

   function URL (Text : String) return String renames Version;

end Alire.Utils.TTY;
