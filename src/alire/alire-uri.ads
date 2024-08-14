with AAA.Strings;

with Alire.Errors;

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

   package Operators is

      function "/" (L, R : String) return String;
      --  Concatenate with forward slash

   end Operators;

   type URI_Kinds is
     (External,
      --  Scheme is "external:"
      --  Denotes a crate detected by some external definition

      System,
      --  Scheme is "system:"
      --  The form "system:package" is used to denote a native package from
      --  the platform

      Local_Git,
      --  Scheme is "git+file:"

      Public_Git,
      --  Recognisably git and scheme is "http:"/"https:"

      Private_Git,
      --  git@host:path, or recognisably git and scheme is "ssh:", or scheme
      --  is "git+<proto>:" (where proto is not "http"/"https" or "file")

      Local_Hg,
      Public_Hg,
      Private_Hg,
      Local_SVN,
      Public_SVN,
      Private_SVN,
      --  Same considerations as for Git

      Bare_Path,
      --  A local path, not a URI (note that this is often still a git repo, it
      --  just isn't apparent from the string alone)

      File,
      --  Scheme is "file:"

      Public_Other,
      --  http/https, but not a recognised VCS

      SSH_Other,
      --  ssh, but not a recognised VCS

      Unknown
      --  Anything else
     );

   subtype Local_Other is URI_Kinds range Bare_Path .. File;

   subtype VCS_URIs is URI_Kinds range Local_Git .. Private_SVN;

   subtype Local_VCS_URIs is URI_Kinds
     with Static_Predicate =>
       Local_VCS_URIs in Local_Git | Local_Hg | Local_SVN;

   subtype Local_URIs is URI_Kinds
     with Static_Predicate =>
       Local_URIs in Local_VCS_URIs | Local_Other;

   subtype Public_VCS_URIs is URI_Kinds
     with Static_Predicate =>
       Public_VCS_URIs in Public_Git | Public_Hg | Public_SVN;

   subtype Public_URIs is URI_Kinds
     with Static_Predicate =>
       Public_URIs in Public_VCS_URIs | Public_Other;

   subtype Private_VCS_URIs is URI_Kinds
     with Static_Predicate =>
       Private_VCS_URIs in Private_Git | Private_Hg | Private_SVN;

   subtype Private_URIs is URI_Kinds
     with Static_Predicate =>
       Private_URIs in Private_VCS_URIs | SSH_Other;

   subtype Git_URIs is URI_Kinds range Local_Git .. Private_Git;
   subtype Hg_URIs is URI_Kinds range Local_Hg .. Private_Hg;
   subtype SVN_URIs is URI_Kinds range Local_SVN .. Private_SVN;

   function URI_Kind (This : String) return URI_Kinds;
   --  Attempt to identify the nature of a resource from its URI.
   --
   --  Formats currently not recognised include:
   --    user@host:/path/to/repo.git      [returns Unknown]
   --    host.name:/path/to/repo.git      [returns Unknown]
   --    git://host/path/to/repo.git      [returns Unknown]
   --    svn://(something)                [returns Unknown]
   --    https://user:pass@host/repo.git  [returns Public_Git]
   --    https://user@host/repo.git       [returns Public_Git]
   --    https://user:pass@host/path      [returns Public_Other]
   --    https://user@host/path           [returns Public_Other]

   function Authority (This : URL) return String;
   --  The authority includes credentials : user:pass@websi.te

   function Authority_Without_Credentials (This : URL) return String;
   --  Only the part after @ in an authority

   function Local_Path (This : URL) return String
     with Pre => In_Local_URIs (URI_Kind (This))
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

   function Strip_VCS_Prefixes (URL : String) return String;
   --  Return the URL without any "git+" prefix or similar.

   function In_Local_URIs (K : URI_Kinds) return Boolean;
   --  For use by this package only
   --
   --  Equivalent to "K in Local_URIs", but used as a workaround for the fact
   --  that GNAT 10 seems to fall over if a subtype with a static predicate is
   --  referenced in the same file as its definition

private

   use AAA.Strings;

   package U renames Standard.URI;

   function L (Str : String) return String renames To_Lower_Case;

   ---------------
   -- Authority --
   ---------------

   function Authority (This : URL) return String
   is (U.Extract (This, U.Authority));

   ----------------
   -- Local_Path --
   ----------------

   function Local_Path (This : URL) return String
   is (if URI_Kind (This) in Bare_Path then This
       elsif In_Local_URIs (URI_Kind (This)) then U.Permissive_Path (This)
       else raise Program_Error with "not applicable");

   ----------
   -- Path --
   ----------

   function Path (This : URL) return String
   is (U.Extract (This, U.Path));

   --------------------
   -- Has_Git_Suffix --
   --------------------

   function Has_Git_Suffix (URL : String) return Boolean
   is (Has_Suffix (L (URL), ".git") or else Has_Suffix (L (URL), ".git/"));

   -----------------------
   -- Is_Known_Git_Host --
   -----------------------

   function Is_Known_Git_Host (Host : String) return Boolean
   is (Host = "github.com"
       or else Host = "gitlab.com"
       or else Host = "bitbucket.org");
   --  Return whether a string is a known host which definitely uses git

end Alire.URI;
