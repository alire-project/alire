with AAA.Debug;
with AAA.Strings;

with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.OS_Lib;
with Alire.Utils;

package body Alire.Errors is

   use AAA.Strings;

   --  Internally, an error is stored with a unique id, which is an integer.
   --  The exception message gets replaced with this integer. As long as the
   --  exception handler uses Errors.Get, the user should never get to see
   --  this meaningless Id.

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Positive, String);

   function To_Int (Id : Unique_Id) return Integer is
     (Integer'Value (AAA.Strings.Tail (String (Id), ':')));

   -----------
   -- Store --
   -----------

   protected Store is

      procedure Set (Text : String; Id : out Positive);

      function Get (Id : Unique_Id) return String;

      procedure Clear (Id : Unique_Id);

   private

      Next  : Positive := 1;
      Store : String_Maps.Map;

   end Store;

   protected body Store is

      ---------
      -- Set --
      ---------

      procedure Set (Text : String; Id : out Positive) is
      begin
         Id   := Next;
         Next := Next + 1;
         Store.Insert (Id, Text);
      end Set;

      ---------
      -- Get --
      ---------

      function Get (Id : Unique_Id) return String is
         (Store (To_Int (Id)));

      -----------
      -- Clear --
      -----------

      procedure Clear (Id : Unique_Id) is
      begin
         Store.Delete (To_Int (Id));
      end Clear;

   end Store;

   ---------
   -- Set --
   ---------

   function Set (Text : String) return String is
      Id : Positive;
   begin
      --  When we store an error, we do so with the current error stack
      Store.Set (Stack (Text), Id);

      return Id_Marker & Trim (Id'Img);
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Id : Unique_Id; Clear : Boolean := True) return String is
   begin
      return Text : constant String := Store.Get (Id) do
         if Clear then
            Store.Clear (Id);
         end if;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Ex    : Ada.Exceptions.Exception_Occurrence;
                 Clear : Boolean := True) return Outcome is
      (Outcome_Failure (Get (Ex, Clear)));

   ---------
   -- Get --
   ---------

   function Get (Ex    : Ada.Exceptions.Exception_Occurrence;
                 Clear : Boolean := True) return String
   is
      use Ada.Exceptions;
      Msg : constant String := Exception_Message (Ex);
   begin
      if Is_Error_Id (Msg) then
         return Get (Unique_Id (Msg), Clear);
      else
         return Msg;
      end if;
   end Get;

   ------------------
   -- Pretty_Print --
   ------------------

   procedure Pretty_Print (Error : String;
                           Level : Trace.Levels := Trace.Error)
   is
      Lines : constant AAA.Strings.Vector := Split (Error, ASCII.LF);
   begin
      for I in Lines.First_Index .. Lines.Last_Index loop
         declare
            Line : constant String := Trim (Lines (I));
         begin
            if Line /= "" then
               Trace.Log
                 ((if I > Lines.First_Index then "   " else "")
                  --  Indentation

                  & (if I < Lines.Last_Index and then Line (Line'Last) = '.'
                    then Line (Line'First .. Line'Last - 1)
                    else Line)
                  --  The error proper, trimming unwanted final '.'

                  & (if I < Lines.Last_Index
                    and then Line (Line'Last) /= ':'
                    then ":"
                    else ""),
                  --  Trailing ':' except for last line
                 Level);
            else
               Trace.Log (Line, Level);
            end if;
         end;
      end loop;
   end Pretty_Print;

   ----------
   -- Wrap --
   ----------

   function Wrap (Upper, Lower : String) return String
   is (Upper & ASCII.LF & Lower);

   -----------
   -- Print --
   -----------

   procedure Print (This : Wrapper) is
   begin
      Pretty_Print (This.Text);
   end Print;

   --------------------
   -- ERROR STACKING --
   --------------------

   Error_Stack : AAA.Strings.Vector;

   ----------
   -- Open --
   ----------

   function Open (Text : String) return Scope is
   begin
      Open (Text);
      return (Ada.Finalization.Limited_Controlled with null record);
   end Open;

   ----------
   -- Open --
   ----------

   procedure Open (Text : String) is
   begin
      Error_Stack.Append (Text);
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Error_Stack.Delete_Last;
   end Close;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out Scope) is
      pragma Unreferenced (This);
   begin
      Close;
   exception
      when E : others =>
         Alire.Utils.Finalize_Exception (E);
   end Finalize;

   -----------
   -- Stack --
   -----------

   function Stack (Text : String) return String
   is
      Msg : UString;
      use UStrings;
   begin
      --  Remove duplicates that may have crept in when generating the final
      --  stack:

      for I in Error_Stack.First_Index .. Error_Stack.Last_Index loop
         if I = Error_Stack.First_Index
           or else Error_Stack (I) /= Error_Stack (I - 1)
         then
            Append (Msg, Error_Stack (I) & ASCII.LF);
         end if;
      end loop;

      return +Msg & Text;
   end Stack;

   -------------------
   -- Program_Error --
   -------------------

   procedure Program_Error (Explanation  : String  := "";
                            Recoverable  : Boolean := True;
                            Stack_Trace  : String  := "";
                            Stack_Offset : Natural := 0)
   is
      Stack  : constant AAA.Strings.Vector :=
                 AAA.Strings.Split
                   ((if Stack_Trace /= ""
                    then Stack_Trace
                    else AAA.Debug.Stack_Trace),
                    ASCII.LF);

      Caller : constant Positive :=
                 5 + Stack_Offset - (if Stack_Trace /= "" then 2 else 0);
      --  The minus 2 is because in stacks obtained from an exception:
      --  1) Except name 2) exec name 3) stack start
      --  If instead we use AAA.Debug.Stack_Trace:
      --  1) Except name 2) exec name 3) AAA.Debug 4) here 5) caller

      URL    : constant String
      := "https://github.com/alire-project/alire/issues/new?title=[Bug%20box]";

      Level  : constant Trace.Levels :=
                 (if Recoverable then Warning else Error);

      ---------
      -- Put --
      ---------

      procedure Put (Msg : String) is
      begin
         Trace.Log (Msg, Level);
      end Put;

   begin
      if Stack_Trace = "" then
         Trace.Debug (AAA.Debug.Stack_Trace);
      end if;
      --  Otherwise, the exception has been logged elsewhere

      Put ("******************* BEGIN Alire bug detected *******************");
      Put ("Location  : "
           & (if Integer (Stack.Length) >= Caller
             then Stack (Caller)
             else "<unknown>"));

      if Explanation /= "" then
         Put ("Extra info: " & Explanation);
      end if;

      Put ("Report at : " & URL);

      if Log_Level < Debug or else not Log_Debug then
         Put ("Re-run with global switches `-vv -d` "
              & "for a full log and stack trace.");
      end if;

      if Recoverable then
         Put
           (TTY.Bold
              ("Alire will now continue as the error might be recoverable."));
      end if;

      Put ("******************** END Alire bug detected ********************");

      if not Recoverable then
         --  Do not re-raise as we would end repeating the -vv -d info
         OS_Lib.Bailout (1);
      end if;
   end Program_Error;

end Alire.Errors;
