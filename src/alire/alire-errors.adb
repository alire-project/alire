with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;

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

   procedure Pretty_Print (Error : String) is
      Lines : constant AAA.Strings.Vector := Split (Error, ASCII.LF);
   begin
      for I in Lines.First_Index .. Lines.Last_Index loop
         declare
            Line : constant String := Trim (Lines (I));
         begin
            Trace.Error ((if I > Lines.First_Index then "   " else "")
                         --  Indentation

                         & (if I < Lines.Last_Index and Line (Line'Last) = '.'
                           then Line (Line'First .. Line'Last - 1)
                           else Line)
                         --  The error proper, trimming unwanted final '.'

                         & (if I < Lines.Last_Index
                              and then Line (Line'Last) /= ':'
                           then ":"
                           else "")
                           --  Trailing ':' except for last line
                        );
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

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   Error_Stack : String_Lists.List;

   ----------
   -- Open --
   ----------

   function Open (Text : String) return Scope is
   begin
      Error_Stack.Append (Text);
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
   end Finalize;

   -----------
   -- Stack --
   -----------

   function Stack (Text : String) return String
   is
      Msg : UString;
      use UStrings;
   begin
      for Item of Error_Stack loop
         Append (Msg, Item & ASCII.LF);
      end loop;

      return +Msg & Text;
   end Stack;

end Alire.Errors;
