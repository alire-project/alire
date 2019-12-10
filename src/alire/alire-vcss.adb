with Alire.Utils;
with Alire.VCSs.Git;

package body Alire.VCSs is

   -----------
   -- Clone --
   -----------

   function Clone (From : URL;
                   Into : Directory_Path)
                   return Outcome
   is
   begin
      case Kind (From) is
         when VCS_Git =>
            return Git.Handler.Clone (Repo_And_Commit (From), Into);
         when VCS_Unknown =>
            return Outcome_Failure ("Unknown VCS requested: " & From);
      end case;
   end Clone;

   ------------
   -- Commit --
   ------------

   function Commit (Origin : URL) return String is
     (if Utils.Contains (Origin, "@")
      then Utils.Tail (Origin, '@')
      else "");

   ----------
   -- Kind --
   ----------

   function Kind (Origin : URL) return Kinds is
     (if Utils.Starts_With (Origin, "git+") then VCS_Git
      else VCS_Unknown);

   ----------
   -- Repo --
   ----------

   function Repo (Origin : URL) return String is
     (Utils.Head (Repo_And_Commit (Origin), '@'));

   ---------------------
   -- Repo_And_Commit --
   ---------------------

   function Repo_And_Commit (Origin : URL) return String is
     (if Utils.Contains (Origin, "+http")
      then Utils.Tail (Origin, '+')
      else Origin);

end Alire.VCSs;
