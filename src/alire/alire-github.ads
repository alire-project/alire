with Alire.Config;
with Alire.Index;

package Alire.GitHub is

   function Branch_Exists
     (User   : String := Config.Get (Config.Keys.User_Github_Login,
                                     Default => "");
      Repo   : String := Index.Community_Repo_Name;
      Branch : String := Index.Community_Branch)
      return Boolean;
   --  Check that a branch exists in a user's repository

   function Repo_Exists
     (User : String := Config.Get (Config.Keys.User_Github_Login,
                                   Default => "");
      Repo : String := Index.Community_Repo_Name)
      return Boolean;
   --  Check that a user has a certain public repo

   function User_Exists
     (User : String := Config.Get (Config.Keys.User_Github_Login,
                                   Default => ""))
     return Boolean;
   --  Check that a user exists in GitHub

end Alire.GitHub;
