with Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alire.OS is

   Separator : Character renames GNAT.OS_Lib.Directory_Separator;

   function Getenv (Var : String; Default : String := "") return String;

   ------------------
   -- Alire_Folder --
   ------------------

   function Alire_Folder return String is
     (Config_Folder & Separator & "alire");

   -------------------------
   -- Alire_Source_Folder --
   -------------------------

   function Alire_Source_Folder return String is
      (Alire_Folder & Separator & "alire");

   -------------------
   -- Config_Folder --
   -------------------

   function Config_Folder return String is
   begin
      return Getenv ("XDG_CONFIG_HOME",
                     Getenv ("HOME") & Separator & ".config");
   end Config_Folder;

   --------------------------
   -- Create_Config_Folder --
   --------------------------

   procedure Create_Config_Folder is
      use Ada.Directories;
   begin
      if not Is_Directory (Config_Folder) then
         Create_Directory (Config_Folder);
      end if;

      if not Is_Directory (Alire_Folder) then
         Create_Directory (Alire_Folder);
      end if;
   end Create_Config_Folder;

   ------------
   -- Getenv --
   ------------

   function Getenv (Var : String; Default : String := "") return String is
      Env_Access : String_Access := GNAT.OS_Lib.Getenv (Var);
      Env        : constant String := Env_Access.all;
   begin
      Free (Env_Access);
      if Env = "" then
         return Default;
      else
         return Env;
      end if;
   end Getenv;

end Alire.OS;
