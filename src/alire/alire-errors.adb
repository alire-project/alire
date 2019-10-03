with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Utils;

package body Alire.Errors is

   --  Internally, an error is stored with a unique id, which is an integer.
   --  The exception message gets replaced with this integer. As long as the
   --  exception handler uses Errors.Get, the user should never get to see
   --  this meaningless Id.

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Positive, String);

   function To_Int (Id : Unique_Id) return Integer is
     (Integer'Value (Utils.Tail (String (Id), ':')));

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
      Store.Set (Text, Id);
      return Id_Marker & Utils.Trim (Id'Img);
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

end Alire.Errors;
