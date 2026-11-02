with AAA.Enum_Tools;

with Alire.Errors;
with Alire.GitHub;
with Alire.Index;
with Alire.URI;
with Alire.Utils.Tables;
with Alire.Utils.User_Input.Query_Config;

with AnsiAda;

with CLIC.User_Input;

package body Alire.Publish.States is

   use URI.Operators;

   subtype JSON_Value is GitHub.JSON_Value;

   -------------
   -- Matches --
   -------------
   --  The GitHub API isn't fully consistent on the case of returned enums
   function Matches (S : String; Target : String) return Boolean
   is
      use AAA.Strings;
   begin
      return To_Lower_Case (S) = To_Lower_Case (Target);
   end Matches;

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

   ---------
   -- Key --
   ---------

   package Key is
      Conclusion : constant String := "conclusion";
      Draft      : constant String := "draft";
      Head       : constant String := "head";
      Label      : constant String := "label";
      Login      : constant String := "login";
      Merged     : constant String := "merged";
      Node_ID    : constant String := "node_id";
      Number     : constant String := "number";
      SHA        : constant String := "sha";
      State      : constant String := "state";
      Title      : constant String := "title";
      User       : constant String := "user";
   end Key;

   package Val is
      --  Case will be dealt with by Matches
      Changes_Requested : constant String := "CHANGES_REQUESTED";
      Closed            : constant String := "closed";
      Open              : constant String := "open";
   end Val;

   ---------------
   -- To_Status --
   ---------------

   function To_Status (Info : JSON_Value) return PR_Status
   is
   begin
      if Info.Is_Nil or else Info.Is_Empty then
         return (Exists => False);
      end if;

      declare
         Number  : constant Natural := Natural (Info (Key.Number).As_Int);

         -------------------
         -- Needs_Changes --
         -------------------

         function Needs_Changes return Boolean is
            Busy    : Simple_Logging.Ongoing :=
                        Simple_Logging.Activity
                          ("Retrieving reviews for PR" & Number'Image)
                          with Unreferenced;
            Reviews : constant JSON_Value := GitHub.Reviews (Number);

            --  Reviews pile up and only the last one of each reviewer is
            --  important, so we have to keep track of reviewers seen.
            Reviewers : AAA.Strings.Map;

         begin
            for Review of Reviews loop
               Reviewers.Include
                 (Review (Key.User) (Key.Login).As_UTF_8,
                  Review (Key.State).As_UTF_8);
            end loop;

            if (for some Review of Reviewers =>
                  Matches (Review, Val.Changes_Requested))
            then
               return True;
            end if;

            return False;
         end Needs_Changes;

         -------------------
         -- Checks_Status --
         -------------------

         function Checks_Status (SHA : String) return Check_States is
            type Conclusions is (Success, Failure, Neutral, Cancelled,
                                 Skipped, Timed_Out, Action_Required,
                                 Pending, Unknown);

            function Is_Valid is new AAA.Enum_Tools.Is_Valid (Conclusions);

            Busy    : Simple_Logging.Ongoing :=
                        Simple_Logging.Activity
                          ("Retrieving checks for PR" & Number'Image)
                          with Unreferenced;
            Response : constant JSON_Value := GitHub.Checks (SHA);
            Checks   : constant JSON_Value := Response ("workflow_runs");

            Some_Incomplete : Boolean := False;
         begin
            if Checks.Is_Empty then
               return Checks_Ongoing;
            end if;

            for Check of Checks loop
               declare
                  Conclusion : constant Conclusions
                    := (if not Check.Has_Field (Key.Conclusion)
                           or else Check (Key.Conclusion).Is_Nil
                        then
                           Pending
                        elsif not Is_Valid (Check (Key.Conclusion).As_UTF_8)
                        then
                           Unknown
                        else
                           Conclusions'Value
                             (Check (Key.Conclusion).As_UTF_8));
               begin
                  case Conclusion is
                     when Failure | Cancelled | Timed_Out =>
                        return Checks_Failed;
                     when Success | Skipped | Neutral =>
                        null;
                     when others =>
                        Some_Incomplete := True;
                  end case;
               end;
            end loop;

            if Some_Incomplete then
               return Checks_Ongoing;
            else
               return Checks_Passed;
            end if;
         end Checks_Status;

      begin
         return
           (Exists  => True,
            Branch  => +Info (Key.Head) (Key.Label).As_UTF_8,
            Number  => Number,
            Node_ID => +Info (Key.Node_ID).As_UTF_8,
            Title   => +Info (Key.Title).As_UTF_8,
            Status  => (if Info.Has_Field (Key.Merged) and then
                           Info (Key.Merged).As_Bool
                        then
                           Merged
                        elsif Matches (Info (Key.State).As_UTF_8, Val.Closed)
                        then
                           Rejected
                        elsif Needs_Changes then
                           Changes_Requested
                        else
                          (case Checks_Status
                                  (Info (Key.Head) (Key.SHA).As_UTF_8)
                           is
                              when Checks_Ongoing =>
                                Checks_Ongoing,
                              when Checks_Failed  =>
                                Checks_Failed,
                              when Checks_Passed =>
                               (if Info (Key.Draft).As_Bool then
                                   Checks_Passed
                                else
                                   Under_Review)))
           );
      end;
   end To_Status;

   -----------------------
   -- Find_Pull_Request --
   -----------------------

   function Find_Pull_Request (M : Milestones.Milestone) return PR_Status
   is
      List   : constant JSON_Value := GitHub.Find_Pull_Request (M);
      Target :          JSON_Value;
      --  Remains nil if no suitable one is found
   begin
      for Obj of List loop

         --  Keep the first one we see
         if Matches (Obj (Key.State).As_UTF_8, Val.Open) then
            Target := Obj;
            exit;

         elsif Target.Is_Nil
           or else
             (Target (Key.State).As_UTF_8 /= Val.Open
              and then Target (Key.Number).As_Int < Obj (Key.Number).As_Int)
         then
            --  Keep the one with the highest #number
            Target := Obj;
         end if;
      end loop;

      if Target.Is_Nil then
         return (Exists => False);
      else
         return To_Status (Target);
      end if;
   end Find_Pull_Request;

   -----------------------
   -- Find_Pull_Request --
   -----------------------

   function Find_Pull_Request (PR : Natural) return PR_Status
   is (To_Status (GitHub.Find_Pull_Request (PR)));

   ------------------------
   -- Find_Pull_Requests --
   ------------------------

   function Find_Pull_Requests return Status_Array is
      use AAA.Strings;
      Busy : constant Simple_Logging.Ongoing
        := Simple_Logging.Activity ("Retrieving user's PRs") with Unreferenced;
      List : constant JSON_Value := GitHub.Find_Pull_Requests;
      Result : Status_Array (1 .. Natural (List.Length));
      I      : Natural := Result'First;
   begin
      --  We can filter at GitHub side using the complete reference, but
      --  surprisingly not by author only. Hence we have to filter here.
      for Item of List loop
         declare
            Branch : constant String := Item (Key.Head) (Key.Label).As_UTF_8;
            --  Do this first so we don't retrieve unneeded reviews
         begin
            if Has_Prefix
              (Branch,
               Utils.User_Input.Query_Config.User_GitHub_Login & ":")
            then
               Result (I) := To_Status (Item);
               I := I + 1;
            end if;
         end;
      end loop;

      return Result (Result'First .. I - 1);
   end Find_Pull_Requests;

   -----------
   -- Color --
   -----------

   function Color (Status : Lifecycle_States) return String
   is
      use AnsiAda;
   begin
      return
        (case Status is
            when Checks_Failed | Rejected                          =>
              Foreground (Light_Red),
            when Checks_Passed | Merged                            =>
              Foreground (Light_Green),
            when Checks_Ongoing | Under_Review | Changes_Requested =>
              Foreground (Light_Yellow));
   end Color;

   ------------------
   -- Print_Status --
   ------------------

   procedure Print_Status is

      use AAA.Strings;
      use AnsiAda;

      States : constant Status_Array := Find_Pull_Requests;
      Table  : Utils.Tables.Table;

   begin
      if States'Length = 0 then
         Trace.Always ("No pending submissions found.");
         return;
      end if;

      Table.Header ("PR").Header ("Reference").Header ("Status").Header ("URL")
        .New_Row;

      for PR of States loop
         Table
           .Append (TTY.Emph (AAA.Strings.Trim (PR.Number'Image)))
           .Append (+PR.Branch)
           .Append (Color_Wrap (To_Mixed_Case (PR.Status'Image),
                                Color (PR.Status)))
           .Append (TTY.URL (Webpage (PR)))
           .New_Row;
      end loop;

      Table.Print (Always);
   end Print_Status;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (PR : Natural; Reason : String) is
      use Simple_Logging;
      Busy : constant Ongoing := Activity ("Checking PR status")
        with Unreferenced;

      Status : constant PR_Status
        := To_Status (GitHub.Find_Pull_Request (PR));

      ---------------
      -- Fail_With --
      ---------------

      procedure Fail_With (Reason : String) is
      begin
         Raise_Checked_Error
           ("Requested pull request" & TTY.Emph (PR'Image) & " " & Reason);
      end Fail_With;

      Busy_Closing : constant Ongoing := Activity ("Closing")
        with Unreferenced;

      use CLIC.User_Input;
   begin
      if not Status.Exists then
         Fail_With ("does not exist");
      end if;

      if not Status.Is_Open then
         Fail_With ("is already closed");
      end if;

      Trace.Info (""); -- New line required after busy spinner

      if Query ("Are you sure that you want to close PR"
                & PR'Image & " (" & (+Status.Title) & ") "
                & "giving as reason: """ & Reason & """?",
                (Yes | No => True, others => False), Yes) = Yes
      then
         GitHub.Close (PR, Reason);
         Put_Success ("Pull request" & TTY.Emph (PR'Image)
                      & " closed successfully");
      else
         Put_Warning ("Operation abandoned", Info);
      end if;
   end Cancel;

   --------------------
   -- Request_Review --
   --------------------

   procedure Request_Review (PR : Natural) is
      use AnsiAda;
      use AAA.Strings;
      St : constant PR_Status := Find_Pull_Request (PR);
   begin
      if not St.Exists then
         Raise_Checked_Error ("Requested pull request not found");
      end if;

      if St.Status /= Checks_Passed then
         Raise_Checked_Error
           (Errors.New_Wrapper
            .Wrap
              ("Reviews can only be requested for pull requests with status "
               & Color_Wrap (To_Mixed_Case (Checks_Passed'Image),
                             Color (Checks_Passed)))
            .Wrap
              ("PR" & TTY.Emph (PR'Image) & " has status "
               & Color_Wrap (To_Mixed_Case (St.Status'Image), Color (St.Status)
                ))
            .Get);
      end if;

      declare
         use Simple_Logging;
         Busy : constant Ongoing := Activity ("Removing draft flag")
           with Unreferenced;
      begin
         GitHub.Request_Review (PR, +St.Node_ID);
      end;

      Put_Success ("Review requested successfully");
   end Request_Review;

end Alire.Publish.States;
