with Alire.Errors;

private with Alire.Utils;
private with URI;

package Alire.URI with Preelaborate is

   --  Helpers to process URLs provided by the user. Note: there's already an
   --  Alire.URL type which is simply a String renaming without any additional
   --  constraints.

   --  See https://tools.ietf.org/html/rfc3986 for full details.
   --
   --  http://user:pass@www.here.com:80/dir1/dir2/xyz.html?p=8&x=doh#anchor
   --   |                    |       | |          |       |         |
   --   protocol             host port path       file   parameters fragment
   --
   --        foo://example.com:8042/over/there?name=ferret#nose
   --        \_/   \______________/\_________/ \_________/ \__/
   --         |           |            |            |        |
   --      scheme     authority       path        query   fragment
   --         |   _____________________|__
   --        / \ /                        \
   --        urn:example:animal:ferret:nose

   type Schemes is
     (None,
      --  For URLs without scheme (to be interpreted as local paths)

      External,
      --  external: denotes a crate detected by some external definition

      File,
      --  A file: URI

      Git,
      --  Anything understood by git, expressed as git+<actual protocol>, e.g.:
      --  git+http[s], git+file

      Hg,
      SVN,
      --  Same considerations as for Git

      HTTP,
      --  Either http or https, since we don't differentiate treatment

      System,
      --  system:package is used to denote a native package from the platform

      Unknown
      --  Anything else
     );
   --  Protocols recognized by Alire

   subtype VCS_Schemes is Schemes range Git .. SVN;

   subtype File_Schemes is Schemes with
     Static_Predicate => File_Schemes in None | File;

   function Scheme (This : URL) return Schemes;
   --  Extract the Scheme part of a URL

   function Authority (This : URL) return String;

   function Local_Path (This : URL) return String
     with Pre => Scheme (This) in None | File
     or else raise Checked_Error with Errors.Set
       ("Given URL does not seem to denote a path: " & This);
   --  Extract complete path from a URL intended for a local path: According to
   --  the URIs RFC, we (I) are using improperly the file: scheme. An absolute
   --  path should be file:/path/to/file, while a relative one should be
   --  file:rel/ati/ve. By using things like file://../path/to, ".." becomes
   --  the authority and "/path/to" the absolute path. This function, for use
   --  with Alire, returns the authority+path as the whole path, so there's no
   --  possible misinterpretation and any file:[/[/[/]]] combination should be
   --  properly interpreted. TL;DR: this should work without further concerns.
   --
   --  TODO: fix incorrectly emitted file:// paths in Origins so at least we
   --  are not generating improper URIs.

   function Path (This : URL) return String;
   --  The path as properly defined (without the authority, if any)

private

   package U renames Standard.URI;

   function L (Str : String) return String renames Utils.To_Lower_Case;

   ---------------
   -- Authority --
   ---------------

   function Authority (This : URL) return String
   is (U.Extract (This, U.Authority));

   ----------------
   -- Local_Path --
   ----------------

   function Local_Path (This : URL) return String
   is (U.Permissive_Path (This));

   ----------
   -- Path --
   ----------

   function Path (This : URL) return String
   is (U.Extract (This, U.Path));

   ------------
   -- Scheme --
   ------------

   function Scheme (This : URL) return Schemes
   is (if U.Scheme (This) = "" then
          None
       elsif L (U.Scheme (This)) = "external" then
          External
       elsif L (U.Scheme (This)) = "file" then
          File
       elsif Utils.Starts_With (L (U.Scheme (This)), "git+") then
          Git
       elsif Utils.Starts_With (L (U.Scheme (This)), "hg+") then
          Hg
       elsif Utils.Starts_With (L (U.Scheme (This)), "svn+") then
          SVN
       elsif L (U.Scheme (This)) = "http" then
          HTTP
       elsif L (U.Scheme (This)) = "https" then
          HTTP
       elsif L (U.Scheme (This)) = "system" then
          System
       else
          Unknown);

end Alire.URI;
