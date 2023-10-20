private with AAA.Strings;

with GNATCOLL.OS.Constants;

package Alire.OS_Lib with Preelaborate is

   function "/" (L, R : String) return String;
   --  Shorthand for path composition

   --  Package to enable easy use of "/"
   package Operators is
      function "/" (L, R : String) return String renames OS_Lib."/";
   end Operators;

   procedure Bailout (Code : Integer := 0);

   function Exe_Suffix return String;

   function Getenv (Name : String; Default : String := "") return String;

   procedure Setenv (Name : String; Value : String);

   function Locate_Exec_On_Path (Exec_Name : String) return String;
   --  Return the location of an executable if found on PATH, or "" otherwise.
   --  On Windows, no need to append ".exe" as it will be found without it.

   Forbidden_Dir_Separator : constant Character :=
                               (case GNATCOLL.OS.Constants.Dir_Sep is
                                   when '/' => '\',
                                   when '\' => '/',
                                   when others =>
                                      raise Unimplemented
                                        with "Unknown dir separator");

   --  For things that may contain path fragments but are not proper paths

   Dir_Separator : Character renames GNATCOLL.OS.Constants.Dir_Sep;

   subtype Native_Path_Like is String
     with Dynamic_Predicate =>
       (for all Char of Native_Path_Like => Char /= Forbidden_Dir_Separator);

   subtype Portable_Path_Like is String
     with Dynamic_Predicate =>
       (for all Char of Portable_Path_Like => Char /= '\');

   function To_Portable (Path : Native_Path_Like) return Portable_Path_Like;

   function To_Native (Path : Portable_Path_Like) return Native_Path_Like;

private

   use AAA.Strings;
   use all type GNATCOLL.OS.OS_Type;

   ----------------------
   -- To_Portable_Like --
   ----------------------

   function To_Portable (Path : Native_Path_Like)
                              return Portable_Path_Like
   is (case GNATCOLL.OS.Constants.OS is
          when MacOS | Unix => Path,
          when Windows      => Replace (Path, "\", "/"));

   --------------------
   -- To_Native_Like --
   --------------------

   function To_Native (Path : Portable_Path_Like) return Native_Path_Like
   is (case GNATCOLL.OS.Constants.OS is
          when MacOS | Unix => Path,
          when Windows      => Replace (String (Path), "/", "\"));

end Alire.OS_Lib;
