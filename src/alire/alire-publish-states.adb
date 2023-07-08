with Alire.GitHub;
with Alire.Index;
with Alire.URI;
with Alire.Utils.Tables;
with Alire.Utils.User_Input.Query_Config;

with GNATCOLL.JSON;

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

   ---------
   -- Key --
   ---------

   package Key is
      Head   : constant String := "head";
      Label  : constant String := "label";
      Number : constant String := "number";
      State  : constant String := "state";
   end Key;

   package Val is
      Open   : constant String := "open";
   end Val;

   ---------------
   -- To_Status --
   ---------------

   function To_Status (Info : GNATCOLL.JSON.JSON_Value) return PR_Status
   is
   begin
      if Info.Is_Empty then
         return (Exists => False);
      end if;

      return
        (Exists  => True,
         Branch  => +Info.Get (Key.Head).Get (Key.Label),
         Number  => Info.Get (Key.Number),
         Status  => (if Info.Get (Key.State) = Val.Open
                     then Open
                     else Rejected), -- TODO: be more precise in the future
         Checks  => <> -- TODO: extract info about checks
        );
   end To_Status;

   -----------------------
   -- Find_Pull_Request --
   -----------------------

   function Find_Pull_Request (M : Milestones.Milestone) return PR_Status
   is
      use GNATCOLL.JSON;
      Result : constant JSON_Value := GitHub.Find_Pull_Request (M);
      List   : constant JSON_Array := Result.Get;
      Target :          JSON_Value := JSON_Null;
   begin
      for I in 1 .. Length (List) loop
         declare
            Obj : constant JSON_Value := Get (List, I);
         begin
            --  Keep the first one we see
            if Obj.Get (Key.State).Get = Val.Open then
               Target := Obj;
               exit;

            elsif Target.Is_Empty
              or else
                (Target.Get (Key.State) /= Val.Open
                 and then Integer'(Target.Get (Key.Number).Get) <
                   Obj.Get (Key.Number))
            then
               --  Keep the one with the highest #number
               Target := Obj;
            end if;
         end;
      end loop;

      if Target.Is_Empty then
         return (Exists => False);
      else
         return To_Status (Target);
      end if;
   end Find_Pull_Request;

   ------------------------
   -- Find_Pull_Requests --
   ------------------------

   function Find_Pull_Requests return Status_Array is
      use AAA.Strings;
      use GNATCOLL.JSON;
      List : constant JSON_Array := GitHub.Find_Pull_Requests.Get;
      Result : Status_Array (1 .. Length (List));
      I      : Natural := Result'First;
   begin
      --  We can filter at GitHub side using the complete reference, but
      --  surprisingly not by author only. Hence we have to filter here.
      for J in 1 .. Length (List) loop
         declare
            Status : constant PR_Status := To_Status (Get (List, J));
         begin
            if Has_Prefix
              (+Status.Branch,
               Utils.User_Input.Query_Config.User_GitHub_Login & ":")
            then
               Result (I) := Status;
               I := I + 1;
            end if;
         end;
      end loop;

      return Result (Result'First .. I - 1);
   end Find_Pull_Requests;

   ------------------
   -- Print_Status --
   ------------------

   procedure Print_Status is
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
           .Append (AAA.Strings.To_Mixed_Case (PR.Status'Image))
           .Append (TTY.URL (Webpage (PR)))
           .New_Row;
      end loop;

      Table.Print (Always);
   end Print_Status;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (PR : Natural; Reason : String) is
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

   begin
      if not Status.Exists then
         Fail_With ("does not exist");
      end if;

      if not Status.Is_Open then
         Fail_With ("is already closed");
      end if;

      GitHub.Close (PR, Reason);

      Put_Success ("Pull request" & TTY.Emph (PR'Image)
                   & " closed successfully");
   end Cancel;

end Alire.Publish.States;
