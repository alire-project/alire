with Ada.Exceptions;
private with Ada.Finalization;

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

   type Unique_Id is new String with
     Dynamic_Predicate => Is_Error_Id (String (Unique_Id));

   function Is_Error_Id (Str : String) return Boolean;
   --  Says if a string actually stores an error id. Used to double check that
   --  Get functions receive an Id and not another unrelated string by mistake,
   --  since that would silently return the same string as the error text.

   function Set (Text : String) return String with
     Post => Is_Error_Id (Set'Result);
   --  Stores an error and receives a unique code to later retrieve it.
   --  The Id is returned as String so it can be directly used as the
   --  error message in a raise, e.g.:
   --    raise Checked_Error with Errors.Set (Very_Long_Message);

   function Get (Id : Unique_Id; Clear : Boolean := True) return String;
   --  Direct retrieval from Id. If Clear the error is removed from memory.

   function Get (Ex    : Ada.Exceptions.Exception_Occurrence;
                 Clear : Boolean := True)
                 return Outcome with
     Post => not Get'Result.Success;
   --  Wrap the error stored for Ex into a failed Outcome. If there was no
   --  error stored for Ex, its exception message is used instead.

   function Get (Ex    : Ada.Exceptions.Exception_Occurrence;
                 Clear : Boolean := True)
                 return String;
   --  Returns the error for Ex if it exists, or defaults to Exception_Message.
   --  The stored error is cleared.

   procedure Pretty_Print (Error : String);
   --  Split Error at LFs to prefix each sub-error in a new line with the
   --  appropriate tracing prefix. Also, from the second line on, messages are
   --  indented. This way, several top-level errors are easier to distinguish.
   --  Finally, ':' is appended to every line but the last one if not already
   --  in the text.

   function Wrap (Upper, Lower : String) return String;
   --  Compose a more general (Upper) error with a more detailed error (Lower).
   --  These are later apt for pretty printing. Even if not pretty printed,
   --  these merely show in two lines.

   function Set (Wrapper : String;
                 Wrapped : Ada.Exceptions.Exception_Occurrence)
                 return String
   is (Set (Wrap (Wrapper, Get (Wrapped))))
   with Post => Is_Error_Id (Set'Result);
   --  Convenience to concatenate two error messages: a new wrapping text and
   --  an existing error within a exception being wrapped.

   -----------
   -- Scope --
   -----------

   type Scope (<>) is limited private;
   --  A type to create a stack of error information. When Errors.Set is used,
   --  the whole error stack is stored. Manages scope closing automatically.

   function Open (Text : String) return Scope;
   --  Push a new message into the error stack

   procedure Open (Text : String);
   --  Manually open a scope; used to blend seamlessly the TOML_Adapters.
   --  Should not be used otherwise.

   procedure Close;
   --  As for Open; don't use manually.

   function Stack (Text : String) return String;
   --  Return current error stack, plus Text as the latest error

private

   Id_Marker : constant String := "alire-stored-error:";

   type Scope is new Ada.Finalization.Limited_Controlled with null record;

   overriding
   procedure Finalize (This : in out Scope);

   -----------------
   -- Is_Error_Id --
   -----------------

   function Is_Error_Id (Str : String) return Boolean is
     (Str'Length > Id_Marker'Length and then
      Str (Str'First .. Str'First + Id_Marker'Length - 1) = Id_Marker);

end Alire.Errors;
