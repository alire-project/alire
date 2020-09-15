with Alire.Utils;

package Alire.VCSs.Git is

   Known_Transformable_Hosts : constant Utils.String_Vector;
   --  Known hosts that honor the git@ --> https:// transformation

   type VCS (<>) is new VCSs.VCS with private;

   function Handler return VCS;

   not overriding
   function Branch (This : VCS;
                    Path : Directory_Path)
                    return String;
   --  Returns the branch name of the repo checked out at Path.

   overriding
   function Clone (This : VCS;
                   From : URL;
                   Into : Directory_Path)
                   return Outcome;

   not overriding
   function Revision_Commit (This   : VCS;
                             Repo   : Directory_Path;
                             Rev    : String)
                             return String;
   --  Returns the commit for a revision, if the repository and revision
   --  (commit, tag, branch...) exist. Should be a no-op for a commit.

   not overriding
   function Is_Detached (This : VCS;
                         Path : Directory_Path) return Boolean;
   --  Says if the repo checked out at Path is in a detached HEAD state.

   not overriding
   function Is_Repository (This : VCS;
                           Path : Directory_Path) return Boolean;
   --  Check if a repo exists at Path

   not overriding
   function Remote (This : VCS; Path : Directory_Path) return String;
   --  Retrieve current remote name (usually "origin")

   overriding
   function Update (This : VCS;
                    Repo : Directory_Path)
                    return Outcome;

   type States is (Clean, Ahead, Dirty);
   --  Three states we are interested in for publishing: clean and up-to-date,
   --  clean but not yet pushed, and dirty.

   not overriding
   function Status (This : VCS;
                    Repo : Directory_Path)
                    return States;

   not overriding
   function Fetch_URL (This   : VCS;
                       Repo   : Directory_Path;
                       Origin : String := "origin";
                       Public : Boolean := True)
                       return URL;
   --  Retrieve the "fetch" url of the given origin, or "" if no repo, no
   --  origin, or any other unforeseen circumstance. If Public, a git@github
   --  private URL is transformed into its equivalent https:// public URL.

   not overriding
   function Head_Commit (This : VCS;
                         Repo : Directory_Path)
                         return String;
   --  Obtain the currently checked out Rev in the repository

   function Transform_To_Public (Remote : String) return URL;
   --  For a Known_Transformable_Host, return the https:// equivalent of a
   --  git@... address. Otherwise return Remote unmodified.

private

   type VCS is new VCSs.VCS with null record;

   function Handler return VCS is (null record);

   use Utils;

   Known_Transformable_Hosts : constant String_Vector :=
                                 Empty_Vector
                                 & "github.com"
                                 & "gitlab.com";

end Alire.VCSs.Git;
