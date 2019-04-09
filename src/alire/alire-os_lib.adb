with GNAT.OS_Lib;

package body Alire.OS_Lib is

   function "/" (L, R : String) return String is
     (L & GNAT.OS_Lib.Directory_Separator & R);

   ----------------
   -- Exe_Suffix --
   ----------------

   function Exe_Suffix return String is
      --  Shenanigans needed to stay preelaborable
      use GNAT.OS_Lib;

      Suffix : String_Access := Get_Executable_Suffix;
   begin
      return S : constant String := Suffix.all do
         Free (Suffix);
      end return;
   end Exe_Suffix;

   ------------
   -- Getenv --
   ------------

   function Getenv (Var : String; Default : String := "") return String is
      use GNAT.OS_Lib;

      Env_Access : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Getenv (Var);
      Env        : constant String := Env_Access.all;
   begin
      Free (Env_Access);
      if Env = "" then
         return Default;
      else
         return Env;
      end if;
   end Getenv;

end Alire.OS_Lib;
