with Alire.GitHub;
with Alire.Index;
with Alire.URI;

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

   -----------------------
   -- Find_Pull_Request --
   -----------------------

   function Find_Pull_Request (M : Milestones.Milestone) return PR_Status
   is
      use GNATCOLL.JSON;
      Result : constant JSON_Value := GitHub.Find_Pull_Request (M);
      List   : constant JSON_Array := Result.Get;
      Target :          JSON_Value := JSON_Null;

      Key_Number : constant String := "number";
      Key_State  : constant String := "state";
      Val_Open   : constant String := "open";
   begin
      for I in 1 .. Length (List) loop
         declare
            Obj : constant JSON_Value := Get (List, I);
         begin
            --  Keep the first one we see
            if Obj.Get (Key_State).Get = Val_Open then
               Target := Obj;
               exit;

            elsif Target.Is_Empty
              or else
                (Target.Get (Key_State) /= Val_Open
                 and then Integer'(Target.Get (Key_Number).Get) <
                   Obj.Get (Key_Number))
            then
               --  Keep the one with the highest #number
               Target := Obj;
            end if;
         end;
      end loop;

      if Target.Is_Empty then
         return (Exists => False);
      else
         return
           (Exists => True,
            Number => Target.Get (Key_Number),
            Status => (if Target.Get (Key_State) = Val_Open
                       then Open
                       else Rejected), -- TODO: be more precise in the future
            others => <>); -- TODO: likewise, inform the rest of fields
      end if;
   end Find_Pull_Request;

end Alire.Publish.States;
