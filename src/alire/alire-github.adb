with Ada.Calendar;
with Ada.Exceptions;

with Alire.Errors;
with Alire.OS_Lib;
with Alire.Utils.TTY;

with Minirest;

package body Alire.GitHub is

   Base_URL    : constant URL    := "https://api.github.com";
   Header_Rate : constant String := "X-Ratelimit-Remaining";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : String) return String is (L & "/" & R);

   --------------
   -- API_Call --
   --------------

   type Kinds is (GET, POST);

   function API_Call (Proc  : String;
                      Args  : Minirest.Parameters := Minirest.No_Arguments;
                      Kind  : Kinds := GET;
                      Token : String := OS_Lib.Getenv (Env_GH_Token, ""))
                      return Minirest.Response
   is
      use Minirest;
      Full_URL : constant String :=
                   Base_URL
                   & (if Proc (Proc'First) /= '/' then "/" else "")
                   & Proc;
      Headers  : Minirest.Parameters :=
                   "Accept" = "Application/vnd.github.v3.full+json";
   begin
      if Token /= "" then
         Headers := Headers and "Authorization" = "Bearer " & Token;
      end if;

      Trace.Debug
        ("GitHub API call " & Kind'Image & " to " & Full_URL);
      Trace.Debug
        ("Headers: " & Minirest.Image (Headers));
      Trace.Debug
        ("Parameters: " & Minirest.Image (Args));

      return This : constant Response :=
        (case Kind is
            when GET =>
              Minirest.Get
               (Full_URL,
                Arguments => Args,
                Headers   => Headers),
            when POST =>
              Minirest.Post
                (Full_URL,
                 Data    => Args,
                 Headers => Headers))
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

   -------------------
   -- Branch_Exists --
   -------------------

   function Branch_Exists
     (User   : String := User_Info.User_GitHub_Login;
      Repo   : String := Index.Community_Repo_Name;
      Branch : String := Index.Community_Branch)
      return Boolean
   is (API_Call ("repos" / User / Repo / "branches" / Branch).Succeeded);

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

      Response : constant Minirest.Response
        := API_Call ("repos" / Owner / Repo / "forks",
                     Kind  => POST,
                     Token => Token);
   begin
      if not Response.Succeeded then
         Raise_Checked_Error
           ("Attempt to fork repo [" & Repo & "] owned by [" & Owner & "] via "
            & "GitHub REST API failed with code:"
            & Response.Status_Code'Image & " and status: "
            & Response.Status_Line & Response.Content.Flatten (ASCII.LF));
      end if;

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
