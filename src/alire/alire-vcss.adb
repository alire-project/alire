with Alire.URI;
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
     (if Alire.URI.URI_Kind (Origin) in Alire.URI.Git_URIs
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

   function Repo_And_Commit (Origin : URL) return String is
      Stripped : constant String := URI.Strip_VCS_Prefixes (Origin);
   begin
      if Has_Prefix (Stripped, "file://") then
         return Stripped (Stripped'First + 7 .. Stripped'Last);
      elsif Has_Prefix (Stripped, "file:") then
         return Stripped (Stripped'First + 5 .. Stripped'Last);
      else
         return Stripped;
      end if;
   end Repo_And_Commit;

end Alire.VCSs;
