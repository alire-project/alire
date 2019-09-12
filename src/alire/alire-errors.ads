with Ada.Exceptions;

package Alire.Errors with Preelaborate is

   --  This package is reentrant and thread-safe.

   --  Supporting types to ensure an exception message doesn't get truncated.
   --  Intended usage is to raise with a Set:
   --    raise My_Exception with Errors.Set ("The sky is falling!");
   --  And later report it with Get:
   --    when E : My_Exception =>
   --       Put_Line (Errors.Get (E));
   --  Or, when returning a failed Outcome:
   --    when E : My_Exception =>
   --       return Errors.Get (E);
   --  If an error for an exception is not found, the exception own message
   --  is returned. This way, using this package is transparent and opt-in:
   --  handlers will either report the proper error, when it exists, or the
   --  exception message as usual, when it doesn't.

   subtype Unique_Id is String;

   function Set (Text : String) return Unique_Id;
   --  Stores an error and receives a unique code to later retrieve it.

   function Get (Id : Unique_Id) return String;
   --  Direct retrieval from Id. The stored error is cleared.

   function Get (Ex : Ada.Exceptions.Exception_Occurrence) return Outcome with
     Post => not Get'Result.Success;
   --  Wrap the error stored for Ex into a failed Outcome.

   function Get (Ex : Ada.Exceptions.Exception_Occurrence) return String;
   --  Returns the error for Ex if it exists, or defaults to Exception_Message.
   --  The stored error is cleared.

end Alire.Errors;
