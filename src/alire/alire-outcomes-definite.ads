with Alire.Errors;

generic
   type Result is private;
   --  The type being returned in case of successful outcome.
package Alire.Outcomes.Definite with Preelaborate is

   type Reference (Ptr : not null access constant Result)
   is limited null record with
     Implicit_Dereference => Ptr;

   type Outcome (<>) is new Alire.Outcome with private;

   function New_Result (R : Result) return Outcome with
     Post => New_Result'Result.Success;
   --  Wrap a Result in a successful Outcome.

   function Value (This : aliased Outcome) return Reference with
     Pre => This.Success or else
     raise Checked_Error with Errors.Set (This.Message);

   overriding
   function Outcome_Failure (Message : String) return Outcome;

   overriding
   function Outcome_Success return Outcome is
     (raise Program_Error with
        "A successful non-trivial outcome requires a result value");

   overriding
   function Outcome_From_Exception
     (Ex  : Ada.Exceptions.Exception_Occurrence;
      Msg : String := "") return Outcome;
   --  Create a failed outcome with given message (or default to Ex message).
   --  The exception is logged at debug level, and to stderr if debug mode on.

private

   type Outcome (OK : Boolean) is new Alire.Outcome with record
      case OK is
         when True  => The_Result : aliased Result;
         when False => null;
      end case;
   end record;

   function New_Result (R : Result) return Outcome is
     (Alire.Outcome_Success with
      OK         => True,
      The_Result => R);

   function Value (This : aliased Outcome) return Reference is
     (Ptr => This.The_Result'Access);

   overriding
   function Outcome_Failure (Message : String) return Outcome is
     (Alire.Outcome_Failure (Message) with OK => False);

   overriding
   function Outcome_From_Exception
     (Ex  : Ada.Exceptions.Exception_Occurrence;
      Msg : String := "") return Outcome is
     (Alire.Outcome_From_Exception (Ex, Msg) with OK => False);

end Alire.Outcomes.Definite;
