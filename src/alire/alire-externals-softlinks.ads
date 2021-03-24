with Ada.Directories;

with Alire.Interfaces;
with Alire.Origins.Deployers;
with Alire.TOML_Adapters;
private with Alire.VFS;

with TOML;

package Alire.Externals.Softlinks is

   --  A do-nothing external that is used to use a dir as an in-progress crate.
   --  This external provides its path to be used for GPR_INCLUDE_PATH.

   type External (<>) is
     new Externals.External
     and Interfaces.Tomifiable
   with private;

   function New_Softlink (From : Any_Path) return External;
   --  Create a softlink for a local dir. From must be absolute or relative to
   --  Ada.Directories.Current.

   function New_Remote (Origin : Origins.Origin;
                        Path   : Relative_Path) return External;
   --  Create a softlink with an associated remote source. Path is where it
   --  has been/will be deployed. Path must be relative to the root using the
   --  softlink.

   function Deploy (This : External) return Outcome;
   --  For a remote pin, redeploy sources if they're not at the expected
   --  location. For a local pin, do nothing.

   overriding
   function Detect (This        : External;
                    Unused_Name : Crate_Name) return Containers.Release_Set
   is (Containers.Release_Sets.Empty_Set);
   --  Never detected, as we want these crates to work as a wildcard for any
   --  version.

   function Is_Remote (This : External) return Boolean;
   --  Say if this is a softlink with a remote origin

   function Is_Valid (This : External) return Boolean;
   --  Check that the pointed-to folder exists

   function Is_Broken (This : External) return Boolean
   is (not This.Is_Valid);

   overriding
   function Image (This : External) return String;

   overriding
   function Detail (This          : External;
                    Unused_Distro : Platforms.Distributions)
                    return Utils.String_Vector
   is (Utils.Empty_Vector.Append ("User-provided external crate"));

   overriding
   function Kind (This : External) return String is ("Symbolic link");

   function Project_Paths (This : External) return Utils.String_Vector;
   --  For now it returns the root path given by the user. We could consider
   --  adding more paths at configuration time.

   function Path (This : External) return Any_Path;

   function From_TOML (Table : TOML_Adapters.Key_Queue) return External;

   overriding
   function To_TOML (This : External) return TOML.TOML_Value;

private

   type Optional_Remote (Used : Boolean) is record
      case Used is
         when True => Remote : Origins.Origin;
         when False => null;
      end case;
   end record;

   type External (Has_Remote, Relative : Boolean; Path_Length : Positive) is
     new Externals.External
     and Interfaces.Tomifiable with record
      Remote : Optional_Remote (Has_Remote);
      case Relative is
         when True  => Rel_Path : Portable_Path (1 .. Path_Length);
         when False => Abs_Path : Absolute_Path (1 .. Path_Length);
      end case;
   end record;

   ------------
   -- Deploy --
   ------------

   function Deploy (This : External) return Outcome
   is (if This.Has_Remote
       then (if GNAT.OS_Lib.Is_Directory (This.Path)
             then Outcome_Success
             else Origins.Deployers.New_Deployer (This.Remote.Remote)
                                   .Deploy (This.Path))
       else Outcome_Success);

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : External) return String
   is ("User-provided at " & This.Path);

   ---------------
   -- Is_Remote --
   ---------------

   function Is_Remote (This : External) return Boolean
   is (This.Has_Remote);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : External) return Boolean
   is (GNAT.OS_Lib.Is_Directory (This.Path));

   ----------
   -- Path --
   ----------

   function Path (This : External) return Any_Path
   is (if This.Relative
       then VFS.To_Native (This.Rel_Path)
       else This.Abs_Path);

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (This : External) return Utils.String_Vector
   is (Utils.To_Vector (Ada.Directories.Full_Name (This.Path)));
   --  As the path may be relative, we make it absolute to avoid duplicates
   --  with absolute paths reported by a Release.Project_Paths.

end Alire.Externals.Softlinks;
