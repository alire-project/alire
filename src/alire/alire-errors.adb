with Ada.Containers.Indefinite_Ordered_Maps;

package body Alire.Errors is

   --  Internally, an error is stored with a unique id, which is an integer.
   --  The exception message gets replaced with this integer. As long as the
   --  exception handler uses Errors.Get, the user should never get to see
   --  this meaningless Id.

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Positive, String);

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
         (Store (Positive'Value (Id)));

      -----------
      -- Clear --
      -----------

      procedure Clear (Id : Unique_Id) is
      begin
         Store.Delete (Positive'Value (Id));
      end Clear;

   end Store;

   ---------
   -- Set --
   ---------

   function Set (Text : String) return Unique_Id is
      Id : Positive;
   begin
      Store.Set (Text, Id);
      return Id'Img;
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Id : Unique_Id) return String is
   begin
      return Text : constant String := Store.Get (Id) do
         Store.Clear (Id);
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Ex : Ada.Exceptions.Exception_Occurrence) return Outcome is
      (Outcome_Failure (Get (Ex)));

   ---------
   -- Get --
   ---------

   function Get (Ex : Ada.Exceptions.Exception_Occurrence) return String is
      use Ada.Exceptions;
   begin
      return Get (Exception_Message (Ex));
   exception
      when others =>
         return Exception_Message (Ex);
   end Get;

end Alire.Errors;
