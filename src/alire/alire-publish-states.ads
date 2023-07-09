with Alire.Milestones;

package Alire.Publish.States is

   type Lifecycle_States is
     (Checks_Ongoing,    -- Waiting for checks to complete, draft or not
      Checks_Failed,     -- Some automated check failed
      Checks_Passed,     -- Checks successful, still in draft mode
      Under_Review,      -- Checks successful, no longer a draft, devs notified
      Changes_Requested, -- Open with changes requested by some reviewer
      Merged,            -- Closed with merge
      Rejected           -- Closed without merge
     );
   --  These states correspond to our desired workflow and not exactly to GH
   --  states. See explanations for each state. It uses a combo of PR status,
   --  checks, and reviews.

   subtype Check_States
     is Lifecycle_States range Checks_Ongoing .. Checks_Passed;

   subtype Open_States
     is Lifecycle_States range Checks_Ongoing .. Changes_Requested;

   type PR_Status (Exists : Boolean) is tagged record
      case Exists is
         when False => null;
         when True  =>
            Branch  : UString; -- In truth, it's `user:branch`
            Number  : Natural           := 0;
            Title   : UString;
            Status  : Lifecycle_States  := Checks_Ongoing;
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
