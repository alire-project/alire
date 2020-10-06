with Ada.Exceptions;

with Alire.Errors;
with Alire.Utils.TTY;

with Minirest;

package body Alire.GitHub is

   package TTY renames Alire.Utils.TTY;

   Base_URL    : constant URL    := "https://api.github.com";
   Header_Rate : constant String := "X-Ratelimit-Remaining";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : String) return String is (L & "/" & R);

   --------------
   -- API_Call --
   --------------

   function API_Call (Proc : String;
                      Args : Minirest.Parameters := Minirest.No_Arguments)
                      return Minirest.Response
   is
      use Minirest;
      Full_URL : constant String :=
                   Base_URL
                   & (if Proc (Proc'First) /= '/' then "/" else "")
                   & Proc;
   begin
      return This : constant Response :=
        Minirest.Get
          (Full_URL,
           Arguments => Args,
           Headers   => "Accept" = "Application/vnd.github.v3.full+json")
      do
         if not This.Succeeded then

            --  Log info about why the API call failed

            Trace.Debug ("Failed GitHub request to API: "
                         & TTY.URL (Full_URL));
            Trace.Debug ("Status: " & This.Status_Line);
            Trace.Debug ("Headers: "
                         & This.Raw_Headers.Flatten ((1 => ASCII.LF)));
            Trace.Debug ("Contents: "
                         & This.Content.Flatten ((1 => ASCII.LF)));

            --  Raise if API is rate-limiting to avoid misleading failures

            if This.Headers.Contains (Header_Rate) and then
              Integer'Value (This.Headers (Header_Rate)) <= 0
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
     (User   : String := Config.Get (Config.Keys.User_Github_Login,
                                     Default => "");
      Repo   : String := Index.Community_Repo_Name;
      Branch : String := Index.Community_Branch)
      return Boolean
   is (API_Call ("repos" / User / Repo / "branches" / Branch).Succeeded);

   -----------------
   -- Repo_Exists --
   -----------------

   function Repo_Exists
     (User : String := Config.Get (Config.Keys.User_Github_Login,
                                   Default => "");
      Repo : String := Index.Community_Repo_Name)
      return Boolean
   is (API_Call ("repos" / User / Repo).Succeeded);

   -----------------
   -- User_Exists --
   -----------------

   function User_Exists
     (User : String := Config.Get (Config.Keys.User_Github_Login,
                                   Default => ""))
      return Boolean
   is (API_Call ("users" / User).Succeeded);

end Alire.GitHub;
