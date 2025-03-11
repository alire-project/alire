with GNAT.OS_Lib;

package body Alire.OS_Lib is

   ---------
   -- "/" --
   ---------

   function "/" (L : Any_Path; R : Relative_Path) return Any_Path is
     (L &
        (if R /= ""
         then GNAT.OS_Lib.Directory_Separator & R
         else ""));

   -------------
   -- Bailout --
   -------------

   procedure Bailout (Code : Integer := 0) is
   begin
      GNAT.OS_Lib.OS_Exit (Code);
   end Bailout;

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

   function Getenv (Name : String; Default : String := "") return String is
      use GNAT.OS_Lib;

      Env_Access : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Getenv (Name);
      Env        : constant String := Env_Access.all;
   begin
      Free (Env_Access);
      if Env = "" then
         return Default;
      else
         return Env;
      end if;
   end Getenv;

   ------------
   -- Setenv --
   ------------

   procedure Setenv (Name : String; Value : String) is
   begin
      Trace.Debug ("Setenv " & Name & "=" & Value);
      GNAT.OS_Lib.Setenv (Name, Value);
   end Setenv;

   -------------------------
   -- Locate_Exec_On_Path --
   -------------------------

   function Locate_Exec_On_Path (Exec_Name : String) return String is
      Located : GNAT.OS_Lib.String_Access :=
                  GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name);
   begin
      if Located not in null then
         return Result : constant String := Located.all do
            GNAT.OS_Lib.Free (Located);
         end return;
      else
         return "";
      end if;
   end Locate_Exec_On_Path;

end Alire.OS_Lib;
