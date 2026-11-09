with Alire.Directories;
private with Alire.OS_Lib;

with AAA.Strings; use AAA.Strings;

package Alire.VFS is

   --  Portable paths are relative and use forward slashes, so that they mean
   --  the same on every platform. Absolute paths, and paths with ':' (Windows
   --  drives), cannot be portable. Portable paths may contain ".." segments,
   --  and so point outside the folder they are relative to (e.g., pins to
   --  sibling folders). Error_In_Contained_Path checks paths that must not.

   function Error_In_Portable_Path (Path : String;
                                    What : String := "path") return String;
   --  Returns "" if Path is portable, or else an error message naming it as
   --  What (e.g., "subdir") and saying why it is not. This does not depend on
   --  the current platform: backslashes, a leading '/' and ':' are rejected.

   function Error_In_Contained_Path (Path   : String;
                                     What   : String := "path";
                                     Within : String := "") return String;
   --  As Error_In_Portable_Path, and Path, taken from the folder Within, must
   --  not go above the folder that Within is relative to. E.g., for a crate
   --  at Within = "crates/foo" in its repository, Path = "../../gnat/x.gpr"
   --  is fine, but "../../../x.gpr" leaves the repository. Within must be
   --  contained itself. For paths in index manifests, which must not reach
   --  outside the deployed origin.

   function Is_Portable (Path : Any_Path) return Boolean;
   --  Say if the path may be safely cast to a portable path. Equivalent to
   --  success with Error_In_Portable_Path.

   function Attempt_Portable (Path : Any_Path;
                              From : Any_Path := Directories.Current)
                              return String;
   --  If Path seen from From is relative, convert to portable, else return
   --  as-is

   function Parent (Path : Portable_Path) return Portable_Path;
   --  Like Ada.Directories.Containing_Directory. Will return "." for simple
   --  names.

   function Simple_Name (Path : Portable_Path) return String;
   --  Like Ada.Directories.Simple name but for portable paths no matter the OS

   function To_Portable (Path : Relative_Path) return Portable_Path;

   function To_Native (Path : Portable_Path) return Relative_Path;

   function Is_Same_Dir (P1, P2 : Any_Path) return Boolean;
   --  Check if two paths are to the same dir, even if they're given as
   --  different equivalent full paths in the filesystem (e.g., Windows
   --  short and long names).
   --
   --  In some cases, equivalent paths may return `False` if we don't have
   --  write access to `P1` to create a temporary probe file.

private

   -----------------
   -- Is_Portable --
   -----------------

   function Is_Portable (Path : Any_Path) return Boolean
   is (Error_In_Portable_Path (Path) = "");

   ------------
   -- Parent --
   ------------

   function Parent (Path : Portable_Path) return Portable_Path
   is (To_Portable
        (Directories.Adirs.Containing_Directory
          (To_Native (Path))));

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (Path : Portable_Path) return String
   is (if Contains (String (Path), "/")
       then Directories.Adirs.Simple_Name (To_Native (Path))
       else String (Path));

   -----------------
   -- To_Portable --
   -----------------

   function To_Portable (Path : Relative_Path) return Portable_Path
   is (Portable_Path
       (OS_Lib.To_Portable
          (Path)));

   ---------------
   -- To_Native --
   ---------------

   function To_Native (Path : Portable_Path) return Relative_Path
   is (Relative_Path (OS_Lib.To_Native (OS_Lib.Portable_Path_Like (Path))));

end Alire.VFS;
