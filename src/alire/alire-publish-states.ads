with Alire.Milestones;

package Alire.Publish.States is

   type Check_States is (Pending, Running, Failed, Succeeded);

   type Life_States is (Open, Changes_Requested, Merged, Rejected);

   subtype Open_States is Life_States range Open .. Changes_Requested;

   type PR_Status (Exists : Boolean) is tagged record
      case Exists is
         when False => null;
         when True  =>
            Branch  : UString; -- In truth, it's `user:branch`
            Number  : Natural      := 0;
            Title   : UString;
            Status  : Life_States  := Open;
            Checks  : Check_States := Pending;
      end case;
   end record;

   function Webpage (PR : PR_Status) return URL;

   function Webpage (PR : Natural) return URL;

   subtype Existing_PR_Status is PR_Status (Exists => True);

   type Status_Array is array (Positive range <>) of Existing_PR_Status;

   function Is_Open (PR : PR_Status) return Boolean
   is (PR.Exists and then PR.Status in Open_States);

   function Find_Pull_Request (M : Milestones.Milestone) return PR_Status;
   --  Find the status of a PR. Only one can be open, that will be returned in
   --  preference. Otherwise, the one closed more recently will be returned.

   function Find_Pull_Requests return Status_Array
     with Post => (for all PR of Find_Pull_Requests'Result =>
                     PR.Status in Open_States);
   --  Find all open pull requests created by the user

   procedure Print_Status;

   procedure Cancel (PR : Natural; Reason : String);

end Alire.Publish.States;
