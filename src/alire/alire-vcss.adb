with Alire.URI;

package body Alire.VCSs is

   use AAA.Strings;

   ------------
   -- Commit --
   ------------

   function Commit (Origin : URL) return String is
     (URI.Fragment (Origin));

   --------------
   -- Repo_URL --
   --------------

   function Repo_URL (Origin : URL) return String is
     (if URI.URI_Kind (Origin) in URI.Non_URLs
      then Origin
      elsif URI.URI_Kind (Origin) in URI.Local_URIs
      then URI.Local_Path (Origin)
      else Head (URI.Strip_VCS_Prefixes (Origin), '#'));

end Alire.VCSs;
