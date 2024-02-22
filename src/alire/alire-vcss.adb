with Alire.VCSs.Git;

package body Alire.VCSs is

   use AAA.Strings;

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
     (if Contains (Origin, "#")
      then Tail (Origin, '#')
      else "");

   ----------
   -- Kind --
   ----------

   function Kind (Origin : URL) return Kinds is
     (if Has_Prefix (Origin, "git+") or Has_Prefix (Origin, "git@")
      then VCS_Git
      else VCS_Unknown);

   ----------
   -- Repo --
   ----------

   function Repo (Origin : URL) return String is
     (Head (Repo_And_Commit (Origin), '#'));

   ---------------------
   -- Repo_And_Commit --
   ---------------------

   function Repo_And_Commit (Origin : URL) return String
   is (if Contains (Origin, "+http") or else Has_Prefix (Origin, "git+")
       then Tail (Origin, '+')
       elsif Has_Prefix (Origin, "file://")
       then Origin (Origin'First + 7 .. Origin'Last)
       else Origin);

end Alire.VCSs;
