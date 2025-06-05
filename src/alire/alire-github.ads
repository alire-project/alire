with Alire.Index;
with Alire.Milestones;
with Alire.Utils.User_Input.Query_Config;

with GNATCOLL.JSON;

package Alire.GitHub is

   subtype JSON_Value is GNATCOLL.JSON.JSON_Value;

   Env_GH_Token : constant String := "GH_TOKEN";
   --  This is the environment variable used by the `gh` tool to look for the
   --  user token. We can reuse it so if it's available, we need not pester the
   --  user for it.

   URL_Tokens   : constant String := "https://github.com/settings/tokens";
   --  URL in which Personal Access Tokens (PATs) can be created

   package User_Info renames Alire.Utils.User_Input.Query_Config;

   type Async_Result is (Completed, Pending);

   function Branch_Exists
     (User   : String := User_Info.User_GitHub_Login;
      Repo   : String := Index.Community_Repo_Name;
      Branch : String := Index.Community_Branch)
      return Boolean;
   --  Check that a branch exists in a user's repository

   function Create_Pull_Request
     (User                  : String  := User_Info.User_GitHub_Login;
      Base                  : String  := Index.Community_Organization;
      Repo                  : String  := Index.Community_Repo_Name;
      Head_Branch           : String  := Index.Community_Branch;
      Base_Branch           : String  := Index.Community_Branch;
      Draft                 : Boolean := False;
      Maintainer_Can_Modify : Boolean := True;
      Token                 : String;
      Title                 : String;
      Message               : String  -- What goes in the body of the PR
     ) return Natural;
   --  Returns the number of the PR just created

   function Find_Pull_Request (M : Milestones.Milestone)
                               return JSON_Value;
   --  Find a pull request that matches the user and branch, and return the raw
   --  JSON info. It will return the unique open PR, or the most recent closed
   --  one.

   function Find_Pull_Request (Number : Natural)
                               return JSON_Value;
   --  Find the PR with the given number, in any state

   function Find_Pull_Requests return JSON_Value;
   --  Return open pull requests created by the user

   procedure Comment (Number : Natural; Text : String);
   --  Add a comment to an issue/pull request. Use plain text or markdown.

   procedure Close (Number : Natural; Reason : String);
   --  Close an issue/pull request and add a comment giving the reason. The
   --  comment is added after the closure.

   function Fork
     (User    : String := User_Info.User_GitHub_Login;
      Owner   : String;
      Repo    : String;
      Token   : String;
      Timeout : Duration := 10.0) return Async_Result;
   --  User is the one into which the fork will appear; Owner is the one we are
   --  forking Repo from. Forks are done in the background after the request is
   --  accepted, so we have to busy wait for it to become available. If Timeout
   --  elapses without succeeding, it will return Pending. It'll only raise if
   --  the initial request is denied.

   procedure Request_Review (Number  : Natural;
                             Node_ID : String);
   --  The Node_ID is the "node_id" returned by the REST API, which is the "id"
   --  needed by the GraphQL API.

   function Checks (SHA : String) return JSON_Value;
   --  Get the workflow run results on a commit

   function Reviews (PR : Natural) return JSON_Value;
   --  Get the reviews for a pull request

   function Repo_Exists
     (User : String := User_Info.User_GitHub_Login;
      Repo : String := Index.Community_Repo_Name)
      return Boolean;
   --  Check that a user has a certain public repo

   function User_Exists
     (User : String := User_Info.User_GitHub_Login)
     return Boolean;
   --  Check that a user exists in GitHub

   function Get_Latest_Alire_Release return String;
   --  Get the tag name for the latest Alire GitHub release

   function Check_Alire_Binary_Release (Tag, Archive : String) return Outcome;
   --  Check that a prebuilt archive exists given a tag name and an archive
   --  name, for ex:
   --  Check_Alire_Binary ("v2.1.0", "alr-2.1.0-bin-aarch64-linux.zip")
   --
   --  In case of error, the resulting Outcome will contain an error message
   --  specifying whether the release or the specific archive was not found.

end Alire.GitHub;
