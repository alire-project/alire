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

      Public_Probably_Git,
      --  Scheme is "http:"/"https:", userinfo component is empty or absent and
      --  host is known to serve git repos
      --
      --  Such URLs are occasionally raw file downloads, not repositories, but
      --  will never be a non-git VCS.

      Public_Definitely_Git,
      --  Scheme is "git+http"/"git+https", or scheme is "http:"/"https:" with
      --  ".git" or ".git/" suffix

      SCP_Style_Git,
      --  An SCP-like git remote, with the form git@host:path

      Private_Probably_Git,
      --  Scheme is "http:"/"https:", userinfo component is non-empty and host
      --  is known to serve git repos
      --
      --  Such URLs are occasionally raw file downloads, not repositories, but
      --  will never be a non-git VCS.

      Private_Definitely_Git,
      --  Recognizably git and scheme is "ssh:", or scheme
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

      Public_HTTP_Other,
      --  http/https, but not a recognized VCS

      Private_HTTP_Other,
      --  Scheme is http/https, userinfo is non-empty and not a recognized VCS

      SSH_Other,
      --  ssh, but not a recognized VCS

      Unknown
      --  Anything else
     );

   subtype Local_Other is URI_Kinds range Bare_Path .. File;

   subtype HTTP_Other is URI_Kinds
     range Public_HTTP_Other .. Private_HTTP_Other;

   subtype Public_Git is URI_Kinds
     range Public_Probably_Git .. Public_Definitely_Git;
   --  Note that this includes the ambiguous case Public_Probably_Git

   subtype Private_Git is URI_Kinds
     range SCP_Style_Git .. Private_Definitely_Git;
   --  Note that this includes the ambiguous case Public_Probably_Git

   subtype Probably_Git is URI_Kinds
     with Static_Predicate =>
       Probably_Git in Private_Probably_Git | Public_Probably_Git;

   subtype Private_Other is URI_Kinds range Private_HTTP_Other .. SSH_Other;

   subtype Non_URLs is URI_Kinds
     with Static_Predicate =>
       Non_URLs in External | System | SCP_Style_Git | Bare_Path | Unknown;

   subtype VCS_URIs is URI_Kinds range Local_Git .. Private_SVN;
   --  Note that this includes the ambiguous cases Public_Probably_Git and
   --  Private_Probably_Git

   subtype Local_VCS_URIs is URI_Kinds
     with Static_Predicate =>
       Local_VCS_URIs in Local_Git | Local_Hg | Local_SVN;

   subtype Local_URIs is URI_Kinds
     with Static_Predicate =>
       Local_URIs in Local_VCS_URIs | Local_Other;

   subtype Public_VCS_URIs is URI_Kinds
     with Static_Predicate =>
       Public_VCS_URIs in Public_Git | Public_Hg | Public_SVN;
   --  Note that this includes the ambiguous case Public_Probably_Git

   subtype Public_URIs is URI_Kinds
     with Static_Predicate =>
       Public_URIs in Public_VCS_URIs | Public_HTTP_Other;

   subtype Private_VCS_URIs is URI_Kinds
     with Static_Predicate =>
       Private_VCS_URIs in Private_Git | Private_Hg | Private_SVN;
   --  Note that this includes the ambiguous case Private_Probably_Git

   subtype Private_URIs is URI_Kinds
     with Static_Predicate =>
       Private_URIs in Private_VCS_URIs | Private_Other;

   subtype Git_URIs is VCS_URIs range Local_Git .. Private_Definitely_Git;
   --  Note that this includes the ambiguous cases Public_Probably_Git and
   --  Private_Probably_Git

   subtype Hg_URIs is VCS_URIs range Local_Hg .. Private_Hg;

   subtype SVN_URIs is VCS_URIs range Local_SVN .. Private_SVN;

   function URI_Kind (This : String) return URI_Kinds;
   --  Attempt to identify the nature of a resource from its URI.
   --
   --  Formats currently not recognized include:
   --    user@host:/path/to/repo.git      [returns Unknown]
   --    host.name:/path/to/repo.git      [returns Unknown]
   --    git://host/path/to/repo.git      [returns Unknown]
   --    ftp://host/path/to/repo.git      [returns Unknown]
   --    svn://host/path/to/repo          [returns Unknown]

   type VCS_Kinds is (Git, Hg, SVN);

   function VCS_Kind (This : String) return VCS_Kinds
     with Pre => URI_Kind (This) in VCS_URIs;

   function Authority (This : URL) return String;
   --  The authority includes credentials : user:pass@websi.te

   function Authority_Without_Credentials (This : URL) return String;
   --  Only the part after @ in an authority

   function Userinfo (This : URL) return String;
   --  Only the part before @ in an authority
   --
   --  Returns an empty string if there is no @.

   function Host (This : URL) return String;
   --  The host part of a remote URL
   --
   --  Remotes of the form 'git@host.name:/some/path' (which are not valid
   --  URIs) return the 'host.name' part.
   --
   --  Returns an empty string for other non-URLs or local filesystem URLs.

   function Fragment (This : URL) return String;
   --  The fragment part of a URL
   --
   --  Returns empty string for non-URLs or local filesystem URLs, or if there
   --  is no fragment.

   function Local_Path (This : URL) return String
     with Pre => In_Local_URIs (URI_Kind (This))
     or else raise Checked_Error with Errors.Set
       ("Given URL does not seem to denote a path: " & This);
   --  Extract complete path from a "file:" URL; return a path unchanged.
   --
   --  We are using the "file:" scheme improperly: according to RFC 8089
   --  (https://tools.ietf.org/html/rfc8089), an absolute path should be either
   --  "file:/path/to/file" or "file:///path/to/file", a relative path is not
   --  permitted, and any "?" or "#" character terminates the path (defining a
   --  query or fragment respectively). This function, for use with Alire,
   --  accepts either "file:rel/ati/ve" or "file://rel/ati/ve" as relative
   --  paths, in addition to the aforementioned absolute path formats, and
   --  treats "?" and "#" characters as literal components of the path.
   --
   --  Percent-encoding is not decoded.

   function Path (This : URL) return String;
   --  The path as properly defined (without the authority, if any)

   function To_URL (Path : Any_Path) return URL
     with Pre => URI_Kind (Path) = Bare_Path;
   --  The 'file:' URL equivalent of a local path
   --
   --  Like Local_Path, this does not strictly adhere to RFC 8089.
   --  Specifically, percent-encoding is not performed, so paths containing
   --  '?' and '#' may cause issues.

   function Strip_VCS_Prefixes (This : String) return String;
   --  Return the URL without any "git+" prefix or similar.

   function Make_VCS_Explicit (This : String; Kind : VCS_Kinds) return String
     with Post => URI_Kind (Make_VCS_Explicit'Result) in VCS_URIs;
   --  Return the URL modified to ensure the VCS is recognizable.
   --
   --  For example, This => "https://host/path" with Kind => Git returns
   --  "git+https://host/path", and This => "/some/path" with Kind => Hg
   --  returns "hg+file:/some/path".
   --
   --  Raises Program_Error if the URL already looks like a different VCS
   --  or has a URI_Kind of External or System.

   function In_Local_URIs (K : URI_Kinds) return Boolean;
   --  For use by this package only
   --
   --  Equivalent to "K in Local_URIs", but used as a workaround for the fact
   --  that GNAT 10 seems to fall over if a subtype with a static predicate is
   --  referenced in the same file as its definition

private

   use AAA.Strings;

   package U renames Standard.URI;

   type String_Access is access constant String;
   type Prefix_Array is array (VCS_Kinds) of String_Access;

   Prefix_Git : aliased constant String := "git+";
   Prefix_Hg  : aliased constant String := "hg+";
   Prefix_SVN : aliased constant String := "svn+";

   VCS_Prefixes : constant Prefix_Array := (Git => Prefix_Git'Access,
                                            Hg  => Prefix_Hg'Access,
                                            SVN => Prefix_SVN'Access);

   function L (Str : String) return String renames To_Lower_Case;

   --------------
   -- VCS_Kind --
   --------------

   function VCS_Kind (This : String) return VCS_Kinds
   is (case URI_Kind (This) is
       when Git_URIs => Git,
       when Hg_URIs => Hg,
       when SVN_URIs => SVN,
       when others => raise Program_Error with "unreachable case");

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
       else U.Extract (This, U.Authority, U.Fragment));

   ----------
   -- Path --
   ----------

   function Path (This : URL) return String
   is (U.Extract (This, U.Path));

   ------------
   -- To_URL --
   ------------

   function To_URL (Path : Any_Path) return URL
   is ("file:" & Path);

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
