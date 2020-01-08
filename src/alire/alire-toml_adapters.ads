with Alire.Utils;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.TOML_Adapters with Preelaborate is

   type Key_Queue is tagged private;
   --  Helper type that simplifies keeping track of processed keys during load.
   --  Also encapsulates a context that can be used to pinpoint errors better.
   --  Note: all operations on this type use shallow copies!

   function Empty_Queue return Key_Queue;

   function From (Key     : String;
                  Value   : TOML.TOML_Value;
                  Context : String) return Key_Queue;
   --  Convert a key/value pair into a wrapped table as Key_Queue.

   function From (Value   : TOML.TOML_Value;
                  Context : String)
                  return Key_Queue;
   --  Create a new queue wrapping a TOML value.

   function Descend (Parent  : Key_Queue;
                     Value   : TOML.TOML_Value;
                     Context : String)
                     return Key_Queue;
   --  Wrap Value as a Key_Queue, using the parent context as context prefix.

   function Descend (Parent  : Key_Queue;
                     Key     : String;
                     Value   : TOML.TOML_Value;
                     Context : String)
                     return Key_Queue;
   --  Use Parent for previous context, wrapping a key = value table.

   function Message (Queue : Key_Queue; Message : String) return String;
   --  Returns Queue's context & ": " & extra message.

   function Failure (Queue : Key_Queue; Message : String) return Outcome with
     Post => not Failure'Result.Success;
   --  Return a failed Outcome, using the Context & Message as information.

   procedure Checked_Error (Queue : Key_Queue; Message : String) with
     No_Return;
   --  Raise a Checked error with Context & ": " & Message, using Alire.Errors.

   function Checked_Pop (Queue : Key_Queue;
                         Key   : String;
                         Kind  : TOML.Any_Value_Kind)
                         return TOML.TOML_Value;
   --  Return the requested Key value, checking it matches type Kind. If type
   --  mismatch or missing key raise a Checked_Error.

   function Pop (Queue : Key_Queue) return TOML.TOML_Value;
   --  Return a value discarding its key; if no values left No_TOML_Value is
   --  returned.

   function Pop (Queue : Key_Queue;
                 Value : out TOML.TOML_Value) return String with
     Post => Value.Is_Present or else Pop'Result = "";
   --  Get a Key/Value pair. The returned string is the key. The pair is
   --  removed from the queue. An empty string is returned when no more pairs
   --  are left.

   function Pop (Queue : Key_Queue;
                 Key   : String;
                 Value : out TOML.TOML_Value) return Boolean with
     Post => Value.Is_Present or else not Pop'Result;
   --  Remove Key from the given set of keys and set Value to the
   --  corresponding value in Queue. Return whether Key was present.

   function Pop_Expr (Queue  : Key_Queue;
                      Prefix : String;
                      Value  : out TOML.TOML_Value) return String;
   --  Return a entry in the underlying table which key starts with Prefix,
   --  or No_TOML_Value if not a table or does not contain such a key. The
   --  intended use is to process keys beginning with "case(" in the table.

   function Unwrap (Queue : Key_Queue) return TOML.TOML_Value;
   --  Return the internal value as-is (with any already popped keys missing).

   procedure Assert_Key (Queue : Key_Queue;
                         Key   : String;
                         Kind  : TOML.Any_Value_Kind);
   --  Ensures that Key exists with given Kind type, or raises Checked_Error.

   function Report_Extra_Keys (Queue : Key_Queue) return Outcome;
   --  If Queue still contains pending keys, consider it's an error, return
   --  false and fill error with extra keys. Just return Success otherwise.

   procedure Report_Extra_Keys (Queue : Key_Queue);
   --  As the previous function, but raise Checked_Error with the same
   --  information in case of remaining keys.

   procedure Print (Queue : Key_Queue);
   --  Debug helper.

   --  Helpers to create TOML values with ease

   function "+" (S : String) return TOML.TOML_Value is
      (TOML.Create_String (S));

   function "+" (Vect : Utils.String_Vector) return TOML.TOML_Value;

   function To_Array (V : TOML.TOML_Value) return TOML.TOML_Value with
     Pre  => V.Kind in TOML.Atom_Value_Kind or V.Kind = TOML.TOML_Array,
     Post => To_Array'Result.Kind = TOML.TOML_Array;
   --  Take an atom value and return an array of a single element
   --  If already an array, do nothing

   function To_Table (Key : String;
                      Val : TOML.TOML_Value) return TOML.TOML_Value with
     Post => To_Table'Result.Kind = TOML.TOML_Table;
   --  Create a table with a single key=val entry

   function Adafy (Key : String) return String;
   --  Take a toml key and replace every '-' and '.' with a '_';

   function Tomify (Image : String) return String;
   --  Take some enumeration image and turn it into a TOML-style key, replacing
   --  every "_" with a "-" and in lower case.

   function To_Vector (Val : TOML.TOML_Value) return Utils.String_Vector
     with
       Pre => Val.Kind = TOML.TOML_Array;
   --  Take a TOML value and turn it into a vector of strings

   generic
      type Enum is (<>);
   function Tomify_Enum (E : Enum) return TOML.TOML_Value;
   --  As the previous function, but taking enumeration values directly.

private

   type Key_Queue is tagged record
      Value   : TOML.TOML_Value;
      Context : UString;
   end record;

   -----------------
   -- Empty_Queue --
   -----------------

   function Empty_Queue return Key_Queue is
     (Value   => TOML.No_TOML_Value,
      Context => <>);

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Queue : Key_Queue) return TOML.TOML_Value is
      (Queue.Value);

   -------------
   -- Descend --
   -------------

   function Message (Queue : Key_Queue; Message : String) return String is
     (+Queue.Context & ": " & Message);

   -------------
   -- Descend --
   -------------

   function Descend (Parent  : Key_Queue;
                     Key     : String;
                     Value   : TOML.TOML_Value;
                     Context : String)
                     return Key_Queue is
      (From (Key, Value, Parent.Message (Context)));

   -------------
   -- Failure --
   -------------

   function Failure (Queue : Key_Queue; Message : String) return Outcome is
     (Outcome_Failure (Queue.Message (Message)));

   -----------
   -- Adafy --
   -----------

   function Adafy (Key : String) return String is
     (Utils.Replace
        (Utils.Replace
             (Key,
              Match => "-",
              Subst => "_"),
        Match => ".", Subst => "_"));

   ----------------------
   -- Tomify_As_String --
   ----------------------

   function Tomify (Image : String) return String is
     (Utils.Replace
        (Utils.To_Lower_Case (Image),
         Match => "_",
         Subst => "-"));

   ------------
   -- Tomify --
   ------------

   function Tomify_Enum (E : Enum) return TOML.TOML_Value is
     (TOML.Create_String
        (Utils.Replace
             (Utils.To_Lower_Case (E'Img),
              Match => "_",
              Subst => "-")));

end Alire.TOML_Adapters;
