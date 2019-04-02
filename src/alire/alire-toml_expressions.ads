private with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with TOML;
use type TOML.Any_Value_Kind;

package Alire.TOML_Expressions is

   package US renames Ada.Strings.Unbounded;

   function "+" (S : String) return US.Unbounded_String
      renames US.To_Unbounded_String;
   function "+" (S : US.Unbounded_String) return String
      renames US.To_String;

   function Is_Valid_Variable_Name (Name : String) return Boolean;
   --  Return whether Name is a valid name for an environment variable

   package Environment_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => US.Unbounded_String,
      Element_Type    => US.Unbounded_String,
      Equivalent_Keys => US."=",
      "="             => US."=",
      Hash            => US.Hash);
   --  Mapping assigning a value to each environment variable

   generic
      type Value_Type is private;
   package Computation_Result is
      type T (Success : Boolean := True) is record
         case Success is
            when False =>
               Error : US.Unbounded_String;
               --  Computation could not reach completion: this contains
               --  information about the error that occurred.

            when True =>
               Value : Value_Type;
               --  Computation reached completion: this contains the resulting
               --  value.
         end case;
      end record;
   end Computation_Result;

   --  TODO: find a way to share more code between the following Single_Values
   --  and Composite_Values generic packages.

   --  TODO: check case clauses coverage on the controlling variables

   --  TODO: check that controlling variables exist

   generic
      type Value_Type is private;
      --  Type for the expressions to handle

      with package Evaluation_Result is new Computation_Result (Value_Type);

      with procedure Parse_Literal
        (Value : TOML.TOML_Value; Result : out Evaluation_Result.T) is <>;
      --  Parse Value as a literal for Value_Type

   package Single_Values is

      --  Provides helpers to parse and evaluate expressions which yield static
      --  values.

      type Expression is limited private;
      --  Expression, whose evaluation produces a Value_Type value

      No_Expression : constant Expression;

      --  TODO: Ada rules prevent us from instantiating Computation_Result on
      --  Expression because that would be a "premature use of private type".

      type Parsing_Result (Success : Boolean := True) is limited record
         case Success is
            when False => Error : US.Unbounded_String;
            when True  => Value : Expression;
         end case;
      end record;

      function Parse (Value : TOML.TOML_Value) return Parsing_Result;
      --  Parse the expression tree encoded in the given TOML document

      function Evaluate
        (Expr : Expression;
         Env  : Environment_Maps.Map) return Evaluation_Result.T;
      --  Evaluate the given expression according to the given environment

      function Evaluate_Or_Default
        (Expr    : Expression;
         Default : Value_Type;
         Env     : Environment_Maps.Map) return Evaluation_Result.T;
      --  If Expr contains no expression, return Default. Otherwise, evaluate
      --  the given expression according to the given environment.

      procedure Move
        (Destination : out Expression; Source : in out Expression);
      --  Move the content of Source to Destination and set Source to
      --  No_Expression.

   private

      type Expression_Node_Record;
      type Expression_Node is access all Expression_Node_Record;

      package Matcher_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => US.Unbounded_String,
         Element_Type    => Expression_Node,
         Equivalent_Keys => US."=",
         Hash            => US.Hash);

      type Expression_Node_Record (Is_Case : Boolean := False) is record
         case Is_Case is
            when False =>
               Literal : Value_Type;
            when True =>
               Variable : US.Unbounded_String;
               Matchers : Matcher_Maps.Map;
               Default  : Expression_Node;
         end case;
      end record;

      package Node_Vectors is new Ada.Containers.Vectors
        (Positive, Expression_Node);
      procedure Free is new Ada.Unchecked_Deallocation
        (Expression_Node_Record, Expression_Node);

      type Expression is limited
         new Ada.Finalization.Limited_Controlled with
      record
         Root  : Expression_Node;
         --  Root node, from which evaluation must start

         Nodes : Node_Vectors.Vector;
         --  List of all expression nodes associated to this expression
      end record;

      overriding procedure Finalize (Self : in out Expression);

      No_Expression : constant Expression :=
        (Ada.Finalization.Limited_Controlled with others => <>);

   end Single_Values;

   generic
      type Value_Type is private;
      --  Type for the expressions to handle

      with package Evaluation_Result is new Computation_Result (Value_Type);

      with procedure Parse_Literal
        (Value : TOML.TOML_Value; Result : out Evaluation_Result.T) is <>;
      --  Parse Value as a literal for Value_Type

      with procedure Merge
        (Left, Right : Value_Type;
         Result      : out Evaluation_Result.T) is <>;
      --  Merge two Value_Type values into a single one

   package Composite_Values is

      --  Provides helpers to parse and evaluate expressions which yield
      --  dynamic values.

      type Expression is limited private;
      --  Expression, whose evaluation produces a Value_Type value

      No_Expression : constant Expression;

      --  TODO: Ada rules prevent us from instantiating Computation_Result on
      --  Expression because that would be a "premature use of private type".

      type Parsing_Result (Success : Boolean := True) is limited record
         case Success is
            when False => Error : US.Unbounded_String;
            when True  => Value : Expression;
         end case;
      end record;

      function Parse (Value : TOML.TOML_Value) return Parsing_Result;
      --  Parse the expression tree encoded in the given Value document

      function Evaluate
        (Expr : Expression;
         Env  : Environment_Maps.Map) return Evaluation_Result.T;
      --  Evaluate the given expression according to the given environment

      function Evaluate_Or_Default
        (Expr    : Expression;
         Default : Value_Type;
         Env     : Environment_Maps.Map) return Evaluation_Result.T;
      --  If Expr contains no expression, return Default. Otherwise, evaluate
      --  the given expression according to the given environment.

      procedure Move
        (Destination : out Expression; Source : in out Expression);
      --  Move the content of Source to Destination and set Source to
      --  No_Expression.

   private

      type Expression_Node_Record;
      type Expression_Node is access all Expression_Node_Record;

      package Matcher_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => US.Unbounded_String,
         Element_Type    => Expression_Node,
         Equivalent_Keys => US."=",
         Hash            => US.Hash);

      type Variable_Array is array (Positive range <>) of US.Unbounded_String;
      type Case_Array is array (Positive range <>) of Matcher_Maps.Map;
      type Expression_Node_Array is
         array (Positive range <>) of Expression_Node;

      type Expression_Node_Record (Case_Count : Natural) is record
         Base_Value : Value_Type;
         --  Value to which this expression evaluates before computing the
         --  "case" parts.

         Variables : Variable_Array (1 .. Case_Count);
         Cases     : Case_Array (1 .. Case_Count);
         Defaults  : Expression_Node_Array (1 .. Case_Count);
         --  Couples of variable names and corresponding case expressions and
         --  default expressions. Each case expression gives another value, and
         --  the final evaluation comes from merging Base_Value and these other
         --  values.
      end record;

      package Node_Vectors is new Ada.Containers.Vectors
        (Positive, Expression_Node);
      procedure Free is new Ada.Unchecked_Deallocation
        (Expression_Node_Record, Expression_Node);

      type Expression is limited
         new Ada.Finalization.Limited_Controlled with
      record
         Root  : Expression_Node;
         --  Root node, from which evaluation must start

         Nodes : Node_Vectors.Vector;
         --  List of all expression nodes associated to this expression
      end record;

      overriding procedure Finalize (Self : in out Expression);

      No_Expression : constant Expression :=
        (Ada.Finalization.Limited_Controlled with others => <>);

   end Composite_Values;

end Alire.TOML_Expressions;
