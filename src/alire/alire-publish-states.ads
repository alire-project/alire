with Alire.Milestones;

package Alire.Publish.States is

   type Check_States is (Pending, Running, Failed, Succeeded);

   type Life_States is (Open, Merged, Changes_Requested, Rejected);
   --  Those are specific to our publishing needs, and cooked from github data

   type PR_Status (Exists : Boolean) is tagged record
      case Exists is
         when False => null;
         when True  =>
            Number            : Natural      := 0;
            Status            : Life_States  := Open;
            Checks            : Check_States := Pending;
            Changes_Requested : Boolean      := False;
      end case;
   end record;

   function Is_Open (PR : PR_Status) return Boolean
   is (PR.Exists and then PR.Status in Open | Changes_Requested);

   function Webpage (PR : PR_Status) return URL;

   function Webpage (PR : Natural) return URL;
   --  Build the URL from the PR number

   function Find_Pull_Request (M : Milestones.Milestone) return PR_Status;
   --  Find the status of a PR. Only one can be open, that will be returned in
   --  preference. Otherwise, the one closed more recently will be returned.

end Alire.Publish.States;
