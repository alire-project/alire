private with Ada.Finalization;

with AAA.Strings; use AAA.Strings;

private with Alire.Errors;
with Alire.Loading;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.TOML_Adapters with Preelaborate is

   function Escape (S : String) return String;
   --  Returns an unquoted string escaped for use in a doubly-quoted string

   function Create_Table (Key   : String;
                          Value : TOML.TOML_Value)
                          return TOML.TOML_Value with
     Pre  => (for all Char of Key => Char /= '.'),
     Post => Create_Table'Result.Kind in TOML.TOML_Table;
   --  Create a table with a single key and value

   type Key_Queue is tagged limited private;
   --  Helper type that simplifies keeping track of processed keys during load.
   --  Also encapsulates a context that can be used to pinpoint errors better.
   --  Note: all operations on this type use shallow copies!

   function Metadata (This : Key_Queue) return Loading.Metadata;

   function From (Key      : String;
                  Value    : TOML.TOML_Value;
                  Context  : String;
                  Metadata : Loading.Metadata := Loading.No_Metadata)
                  return Key_Queue;
   --  Convert a key/value pair into a wrapped table as Key_Queue.

   function From (Value    : TOML.TOML_Value;
                  Context  : String;
                  Metadata : Loading.Metadata := Loading.No_Metadata)
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

   function Failure (Queue : Key_Queue; Message : String) return Outcome with
     Post => not Failure'Result.Success;
   --  Return a failed Outcome, using the Context & Message as information.

   procedure Assert (Queue : Key_Queue; Condition : Boolean; Message : String);
   --  If Condition is False, call Queue.Checked_Error (Message)

   procedure Checked_Error (Queue : Key_Queue; Message : String) with
     No_Return;
   --  Raise a Checked error with Context & ": " & Message, using Alire.Errors.

   procedure Recoverable_Error (Queue   : Key_Queue;
                                Message : String;
                                Recover : Boolean := Alire.Force);
   --  As Checked_Error, but emit a warning instead when Recover is True

   function Checked_Pop (Queue : Key_Queue;
                         Key   : String;
                         Kind  : TOML.Any_Value_Kind)
                         return TOML.TOML_Value;
   --  Return the requested Key value, checking it matches type Kind. If type
   --  mismatch or missing key raise a Checked_Error.

   function Contains (Queue : Key_Queue; Key : String) return Boolean;
   --  Says if one of the keys in the wrapped table is Key

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

   function Pop (Queue : Key_Queue; Key : String) return TOML.TOML_Value;
   --  Pop a key, that must exist, without checking its type (see Checked_Pop);

   function Pop_Expr (Queue  : Key_Queue;
                      Prefix : String;
                      Value  : out TOML.TOML_Value) return String;
   --  Return a entry in the underlying table whose key starts with Prefix,
   --  or No_TOML_Value if not a table or does not contain such a key. The
   --  intended use is to process keys beginning with "case(" in the table.

   function Pop_Single_Table (Queue : Key_Queue;
                              Value : out TOML.TOML_Value)
                              return String;
   --  For constructions like [parent.child.grandchild], where only one child
   --  is allowed. Child is returned as String, and Value is set to granchild.
   --  Raises Checked_Error if Queue is not a table, or it doesn't contain
   --  exactly one key.

   function Pop_Single_Table (Queue  : Key_Queue;
                              Value  : out TOML.TOML_Value;
                              Kind   : TOML.Any_Value_Kind) return String;
   --  For constructions like [parent.child.grandchild], where we known that
   --  only one child can exist. Will raise Checked_Error if any of these
   --  happens: Queue is not a table; Queue doesn't have exactly one key; Value
   --  is not of the expected Kind. Returns the single key child. Value is set
   --  to grandchild.

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

   function "+" (Vect : AAA.Strings.Vector) return TOML.TOML_Value;

   function To_Array (V : TOML.TOML_Value) return TOML.TOML_Value with
     Post => To_Array'Result.Kind = TOML.TOML_Array;
   --  Take a value and return an array of a single element.
   --  If already an array, do nothing.

   function To_Table (Key : String;
                      Val : TOML.TOML_Value) return TOML.TOML_Value with
     Post => To_Table'Result.Kind = TOML.TOML_Table;
   --  Create a table with a single key=val entry

   function Adafy (Key : String) return String;
   --  Take a toml key and replace every '-' and '.' with a '_'; Use Title_Case
   --  unless key = "others".

   function Tomify (Image : String) return String;
   --  Take some enumeration image and turn it into a TOML-style key, replacing
   --  every "_" with a "-" and in lower case.

   generic
      type Enum is (<>);
   function Tomify_Enum (E : Enum) return TOML.TOML_Value with
     Post => Tomify_Enum'Result.Kind = TOML.TOML_String;
   --  As Tomify function, but taking enumeration values directly

   function To_Vector (Val : TOML.TOML_Value) return AAA.Strings.Vector
     with
       Pre => Val.Kind = TOML.TOML_Array;
   --  Take a TOML value and turn it into a vector of strings

   function Merge_Tables (L, R : TOML.TOML_Value) return TOML.TOML_Value with
     Pre => L.Kind in TOML.TOML_Table and then R.Kind in TOML.TOML_Table,
     Post => Merge_Tables'Result.Kind in TOML.TOML_Table;

private

   use type UString; -- Allows comparisons between strings and unbounded

   type Key_Queue is new Ada.Finalization.Limited_Controlled with record
      Value    : TOML.TOML_Value;
      Metadata : Loading.Metadata;
   end record;

   overriding
   procedure Finalize (This : in out Key_Queue);

   --------------
   -- Metadata --
   --------------

   function Metadata (This : Key_Queue) return Loading.Metadata
   is (This.Metadata);

   --------------
   -- Contains --
   --------------

   function Contains (Queue : Key_Queue; Key : String) return Boolean
   is (Queue.Unwrap.Kind in TOML.TOML_Table
       and then
         (for some Table_Key of Queue.Unwrap.Keys => Key = Table_Key));

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Queue : Key_Queue) return TOML.TOML_Value is
      (Queue.Value);

   -------------
   -- Descend --
   -------------

   function Descend (Parent  : Key_Queue;
                     Key     : String;
                     Value   : TOML.TOML_Value;
                     Context : String)
                     return Key_Queue is
      (From (Key, Value, Context, Parent.Metadata));

   -------------
   -- Failure --
   -------------

   function Failure (Queue : Key_Queue; Message : String) return Outcome is
     (Outcome_Failure (Message));

   -----------
   -- Adafy --
   -----------

   function Adafy (Key : String) return String is
     (if (for some Char of Key => Char = '_')
      then raise Alire.Checked_Error with Errors.Set
        ("TOML keys should use hyphens instead of underscores, but found key: "
         & Key)
      elsif To_Lower_Case (Key) = "others"
      then To_Lower_Case (Key)
      else
      To_Mixed_Case
        (Replace
             (Replace
                  (Key,
                   Match => "-",
                   Subst => "_"),
              Match => ".", Subst => "_")));

   ----------------------
   -- Tomify_As_String --
   ----------------------

   function Tomify (Image : String) return String is
     (Replace
        (To_Lower_Case (Image),
         Match => "_",
         Subst => "-"));

   ------------
   -- Tomify --
   ------------

   function Tomify_Enum (E : Enum) return TOML.TOML_Value is
     (TOML.Create_String
        (Replace
             (To_Lower_Case (E'Img),
              Match => "_",
              Subst => "-")));

end Alire.TOML_Adapters;
