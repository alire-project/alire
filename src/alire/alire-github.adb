with Ada.Calendar;
with Ada.Exceptions;

with Alire.Errors;
with Alire.OS_Lib;
with Alire.Publish;
with Alire.URI;
with Alire.Utils.TTY;
with Alire.Version;

with GNATCOLL.JSON.Utility;
with GNATCOLL.Strings;

with Minirest;

package body Alire.GitHub is

   use Minirest;
   use URI.Operators;

   Base_URL    : constant URL    := "https://api.github.com";
   Header_Rate : constant String := "X-Ratelimit-Remaining";

   -----------------
   -- JSON_Escape --
   -----------------

   function JSON_Escape (S : String) return String
   is
      use GNATCOLL;
      X : constant Strings.XString := Strings.To_XString (S);
   begin
      return +GNATCOLL.JSON.Utility.Escape_String (X);
   end JSON_Escape;

   --------------
   -- API_Call --
   --------------

   type Kinds is new Minirest.Request_Kinds;

   function API_Call (Proc  : String;
                      Args  : Minirest.Parameters := Minirest.No_Arguments;
                      Kind  : Kinds := GET;
                      Token : String := OS_Lib.Getenv (Env_GH_Token, ""))
                      return Minirest.Response
   is
      Full_URL : constant String :=
                   Base_URL
                   & (if Proc (Proc'First) /= '/' then "/" else "")
                   & Proc;
      Headers  : Minirest.Parameters :=
                   "Accept" = "Application/vnd.github.v3.full+json"
               and "X-GitHub-Api-Version" = "2022-11-28";
   begin
      if Token /= "" then
         Headers := Headers and "Authorization" = "Bearer " & Token;
      end if;

      Trace.Debug
        ("GitHub API call " & Kind'Image & " to " & Full_URL);
      Trace.Debug
        ("Headers: " & Minirest.Image (Headers, JSON_Escape'Access));
      Trace.Debug
        ("Parameters: " & Minirest.Image (Args, JSON_Escape'Access));

      return This : constant Response :=
        (case Kind is
            when GET =>
              Minirest.Get
               (Full_URL,
                Arguments => Args,
                Headers   => Headers),
            when POST | PATCH =>
              Minirest.Post
                (Full_URL,
                 Data    => Args,
                 Headers => Headers,
                 Escape  => JSON_Escape'Access,
                 Kind    => Minirest.Request_Kinds (Kind)))
      do
         Trace.Debug
           ("GitHub API response: " & This.Status_Line);
         Trace.Debug
           ("Headers: " & This.Raw_Headers.Flatten (ASCII.LF));
         Trace.Debug
           ("Data: " & This.Content.Flatten (ASCII.LF));

         if not This.Succeeded then

            --  Log info about why the API call failed

            Trace.Debug ("Failed GitHub request to API: "
                         & Utils.TTY.URL (Full_URL));

            --  Raise if API is rate-limiting to avoid misleading failures

            if This.Headers.Contains (Header_Rate) and then
              Integer'Value (This.Headers.Get (Header_Rate)) <= 0
            then
               Raise_Checked_Error
                 ("GitHub API rate limit exceeded, please wait for a while"
                  & " before retrying.");
            end if;

         end if;
      end return;
   exception
      when E : Minirest.Rest_Error =>
         Log_Exception (E);
         Raise_Checked_Error
           (Errors.Wrap
              ("Error querying GitHub API",
               Ada.Exceptions.Exception_Message (E)));
   end API_Call;

   --------------
   -- API_Call --
   --------------

   function API_Call (Proc   : String;
                      Args   : Minirest.Parameters := Minirest.No_Arguments;
                      Kind   : Kinds := GET;
                      Token  : String := OS_Lib.Getenv (Env_GH_Token, "");
                      Error  : String := "GitHub API call failed")
                      return GNATCOLL.JSON.JSON_Value
   is
      Response : constant Minirest.Response
        := API_Call (Proc   => Proc,
                     Args   => Args,
                     Kind   => Kind,
                     Token  => Token);
   begin
      if Response.Succeeded then
         return GNATCOLL.JSON.Read (Response.Content.Flatten (""));
      else
         Raise_Checked_Error
           (Errors.New_Wrapper
            .Wrap (Error)
            .Wrap ("Status line: " & Response.Status_Line)
            .Wrap ("Response body:")
            .Wrap (Response.Content.Flatten (ASCII.LF))
            .Get);
      end if;
   end API_Call;

   -------------------
   -- Branch_Exists --
   -------------------

   function Branch_Exists
     (User   : String := User_Info.User_GitHub_Login;
      Repo   : String := Index.Community_Repo_Name;
      Branch : String := Index.Community_Branch)
      return Boolean
   is (API_Call ("repos" / User / Repo / "branches" / Branch).Succeeded);

   -------------------------
   -- Create_Pull_Request --
   -------------------------

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
     )
      return Natural
   is
   begin
      return
        API_Call
          (Kind  => POST,
           Token => Token,
           Error => "Pull request could not be created",
           Proc  => "repos" / Base / Repo / "pulls",
           Args  =>
               "title" = Title
           and "body"  = Message
           and "base"  = Base_Branch
           and "head"  = User & ":" & Head_Branch
           and "draft" = Draft
           and "maintainer_can_modify" = Maintainer_Can_Modify)
        .Get ("number").Get;
   end Create_Pull_Request;

   ---------------
   -- Get_Pulls --
   ---------------

   function Get_Pulls (Args : Minirest.Parameters)
                       return GNATCOLL.JSON.JSON_Value
   is
   begin
      return
        API_Call ("repos"
                  / Index.Community_Organization
                  / Index.Community_Repo_Name
                  / "pulls",
                  Error => "Could not list pull requests",
                  Kind => GET,
                  Args => Args and "per_page" = 100);
   end Get_Pulls;

   -----------------------
   -- Find_Pull_Request --
   -----------------------

   function Find_Pull_Request (M : Milestones.Milestone)
                               return GNATCOLL.JSON.JSON_Value
   is (Get_Pulls ("state" = "all"
              and "head"  = User_Info.User_GitHub_Login & ":"
                            & Publish.Branch_Name (M)));

   -----------------------
   -- Find_Pull_Request --
   -----------------------

   function Find_Pull_Request (Number : Natural)
                               return GNATCOLL.JSON.JSON_Value
   is (API_Call
         ("repos"
          / Index.Community_Organization
          / Index.Community_Repo_Name
          / "pulls"
          / AAA.Strings.Trim (Number'Image),
          Error => "Could not retrieve pull request information",
          Kind  => GET));

   ------------------------
   -- Find_Pull_Requests --
   ------------------------

   function Find_Pull_Requests return GNATCOLL.JSON.JSON_Value
   is (Get_Pulls ("state" = "open"
              and "head"  = User_Info.User_GitHub_Login));

   -------------
   -- Comment --
   -------------

   procedure Comment (Number : Natural; Text : String) is
      Unused : constant GNATCOLL.JSON.JSON_Value
        := API_Call
          ("repos"
           / Index.Community_Organization
           / Index.Community_Repo_Name
           / "issues"
           / AAA.Strings.Trim (Number'Image)
           / "comments",
           Kind => POST,
           Args => "body" = Text);
   begin
      Trace.Debug ("Comment via GitHub REST API successful: " & Text);
   end Comment;

   -----------
   -- Close --
   -----------

   procedure Close (Number : Natural; Reason : String) is
      use AAA.Strings;
      Unused : constant GNATCOLL.JSON.JSON_Value
        := API_Call ("repos"
                     / Index.Community_Organization
                     / Index.Community_Repo_Name
                     / "pulls"
                     / Trim (Number'Image),
                     Error => "Failed to close pull request via REST API",
                     Kind  => PATCH,
                     Args  => "state" = "closed");
   begin
      Comment (Number,
               "Closed using `alr " & Version.Current & "` with reason: "
               & Reason);
   end Close;

   ----------
   -- Fork --
   ----------

   function Fork
     (User    : String := User_Info.User_GitHub_Login;
      Owner   : String;
      Repo    : String;
      Token   : String;
      Timeout : Duration := 10.0)
      return Async_Result
   is
      use Ada.Calendar;

      Start : constant Time := Clock;
      Next  : Time := Start + 1.0;

      Unused : constant GNATCOLL.JSON.JSON_Value
        := API_Call ("repos" / Owner / Repo / "forks",
                     Kind  => POST,
                     Token => Token,
                     Error =>
                       "Attempt to fork repo [" & Repo & "] owned by ["
                       & Owner & "] via " & "GitHub REST API failed"
                    );
   begin
      declare
         Wait : Trace.Ongoing := Trace.Activity ("Waiting for GitHub");
      begin
         while Clock - Start < Timeout loop
            delay until Next;
            Next := Next + 1.0;
            Wait.Step;
            if Repo_Exists (User, Repo) then
               return Completed;
            end if;
         end loop;
      end;

      return Pending;
   end Fork;

   -----------------
   -- Repo_Exists --
   -----------------

   function Repo_Exists
     (User : String := User_Info.User_GitHub_Login;
      Repo : String := Index.Community_Repo_Name)
      return Boolean
   is (API_Call ("repos" / User / Repo).Succeeded);

   -----------------
   -- User_Exists --
   -----------------

   function User_Exists
     (User : String := User_Info.User_GitHub_Login)
      return Boolean
   is (API_Call ("users" / User).Succeeded);

end Alire.GitHub;
