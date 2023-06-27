with Alire.Index;
with Alire.URI;

package body Alire.Publish.States is

   use URI.Operators;

   -------------
   -- Webpage --
   -------------

   function Webpage (PR : Natural) return URL
   is (Index.Community_Host
       / Index.Community_Organization
       / Index.Community_Repo_Name
       / "pull"
       / AAA.Strings.Trim (PR'Image));

   -------------
   -- Webpage --
   -------------

   function Webpage (PR : PR_Status) return URL
   is (Webpage (PR.Number));

   -----------------------
   -- Find_Pull_Request --
   -----------------------

   function Find_Pull_Request (M : Milestones.Milestone) return PR_Status
   is ((Exists => False));

end Alire.Publish.States;
