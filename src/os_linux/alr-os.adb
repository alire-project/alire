with Ada.Directories;

with Alire.Os_Lib; use Alire.OS_Lib;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Interfaces.C;

package body Alr.OS is

   function Getenv (Var : String; Default : String := "") return String;

   ------------------
   -- Cache_Folder --
   ------------------

   function Cache_Folder return String is
   begin
      return Getenv ("XDG_CACHE_HOME",
                     Default => Getenv ("HOME") / ".cache" / "alire");
   end Cache_Folder;

   -------------------
   -- Config_Folder --
   -------------------

   function Config_Folder return String is
   begin
      return Getenv ("XDG_CONFIG_HOME",
                     Default => Getenv ("HOME") / ".config" / "alire");
   end Config_Folder;

   ------------------
   -- Devel_Folder --
   ------------------

   function Devel_Folder return String is
     (Getenv ("HOME") / "local" / "alr");

   --------------------
   -- Devel_Telltale --
   --------------------

   function Devel_Telltale return String is
     (Config_Folder / "enable-devel");

   ---------------------
   -- Projects_Folder --
   ---------------------

   function Projects_Folder return String is
   begin
      return Cache_Folder / "projects";
   end Projects_Folder;

   --------------------
   -- Session_Folder --
   --------------------

   function Session_Folder return String is
      Path : constant String := Cache_Folder / "sessions" / "common";
      --  FIXME: right now there are no sessions, only this one for everything
      --  Might not be a problem if alr is rebuild whenever run within an alire project
   begin
      if not Ada.Directories.Exists (Path) then
         Create_Folder (Path);
      end if;
      return Path;
   end Session_Folder;

   --------------------------
   -- Create_Config_Folder --
   --------------------------

   procedure Create_Folder (Path : String) is
   begin
      Alire.OS_Lib.Spawn ("mkdir", "-p " & Path);
   end Create_Folder;

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

   --------------------
   -- Own_Executable --
   --------------------

   function Own_Executable return String is
      -- (int buflen, char *buffer, int *len)
      use Interfaces;
      use type C.Size_T;

      Buffer : C.Char_Array (1 .. 1024);
      Used   : C.size_t;
      procedure Own_Exec_C (Buflen : C.size_t; Buffer : out C.char_array; Len : out C.size_t);
      pragma Import (C, Own_Exec_C, "own_exec");

   begin
      Own_Exec_C (Buffer'Length, Buffer, Used);
      return C.To_Ada (Buffer (Buffer'First .. Buffer'First + Used - 1), Trim_Nul => False);
   end Own_Executable;

end Alr.OS;
