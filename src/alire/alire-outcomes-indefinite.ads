with Ada.Containers.Indefinite_Holders;

with Alire.Errors;

--  See documentation in the Definites package.

generic
   type Result (<>) is private;
package Alire.Outcomes.Indefinite with Preelaborate is

   type Reference (Ptr : not null access constant Result)
   is limited null record with
     Implicit_Dereference => Ptr;

   type Outcome (<>) is new Alire.Outcome with private;

   function New_Result (R : Result) return Outcome with
     Post => New_Result'Result.Success;

   function Value (This : aliased Outcome) return Reference with
     Pre => This.Success or else
     raise Checked_Error with Errors.Set (This.Message);

   overriding
   function Outcome_Failure (Message : String) return Outcome;

   overriding
   function Outcome_Success return Outcome is
     (raise Program_Error with
        "A successful non-trivial outcome requires a result");

   overriding
   function Outcome_From_Exception
     (Ex  : Ada.Exceptions.Exception_Occurrence;
      Msg : String := "") return Outcome;

private

   package Definites is new Ada.Containers.Indefinite_Holders (Result);

   type Outcome (OK : Boolean) is new Alire.Outcome with record
      case OK is
         when True  => The_Result : Definites.Holder;
         when False => null;
      end case;
   end record;

   function New_Result (R : Result) return Outcome is
     (OK         => True,
      Success    => True,
      Message    => <>,
      The_Result => Definites.To_Holder (R));

   function Value (This : aliased Outcome) return Reference is
     (Ptr => This.The_Result.Constant_Reference.Element);

   overriding
   function Outcome_Failure (Message : String) return Outcome is
     (Alire.Outcome_Failure (Message) with OK => False);

   overriding
   function Outcome_From_Exception
     (Ex  : Ada.Exceptions.Exception_Occurrence;
      Msg : String := "") return Outcome is
     (Alire.Outcome_From_Exception (Ex, Msg) with OK => False);

end Alire.Outcomes.Indefinite;
