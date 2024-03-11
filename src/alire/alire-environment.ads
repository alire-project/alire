with Ada.Strings.Unbounded;

with Alire.Properties;
with Alire.Platforms;

private with Ada.Strings.Unbounded.Hash;
private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
private with Alire.Properties.Environment;
private with Ada.Containers.Generic_Array_Sort;

package Alire.Environment with Preelaborate is

   --  ANY NEW VARIABLES SHOULD USE "ALIRE" IF THIS IS SOMETHING AFFECTING THE
   --  BASE LIBRARY. CONSIDER USING A SETTING INSTEAD, UNLESS AN ENVIRONMENT
   --  VARIABLE MAKES ABSOLUTE SENSE.

   Config : constant String := "ALR_CONFIG";
   --  Folder where current alr will look for configuration
   --  DEPRECATED on 3.0

   Settings : constant String := "ALIRE_SETTINGS_DIR";
   --  Folder where Alire will look for configuration

   Testsuite : constant String := "ALR_TESTSUITE";
   --  If defined, we are running under the testsuite harness

   Testsuite_Allow : constant String := "ALR_TESTSUITE_ALLOW";
   --  If defined, we want to allow operations normally disabled forbidden
   --  during testsuite runs, like creating a PR in a public server.

   Traceback : constant String := "ALR_TRACEBACK_ENABLED";
   --  If set to True/1, dump unexpected exceptions to console (same as `-d`)

   function Traceback_Enabled return Boolean;

   type Context is tagged limited private;

   procedure Set (This : in out Context; Name, Value, Origin : String);
   --  Set a variable in the context

   procedure Append (This : in out Context; Name, Value, Origin : String);
   --  Append a value to a variable in the context

   procedure Prepend (This : in out Context; Name, Value, Origin : String);
   --  Prepend a value to a variable in the context

   procedure Export (This : Context);
   --  Export the environment variables built from the variables previously
   --  loaded and defined in the context to the OS.

   procedure Print_Shell (This : Context; Kind : Platforms.Shells);
   --  Print the shell commands that can be used to export the environment
   --  variables.

   procedure Print_Details (This : Context);
   --  Print details about the environment context. What are the variables
   --  definitions and their origin.

   --  Bulk export

   subtype Env_Map is AAA.Strings.Map;
   --  key --> value map

   function Get_All (This            : Context;
                     Check_Conflicts : Boolean := False)
                     return Env_Map;
   --  Build a map for all variables in the solution (both GPR and
   --  environment). Since this is used during hash computation, we must
   --  skip conflict checks at this time as definitive paths aren't yet known.

private

   type Var is record
      Key : Ada.Strings.Unbounded.Unbounded_String;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function "<" (Left, Right : Var) return Boolean
   is (Ada.Strings.Unbounded."<" (Left.Key, Right.Key));

   type Var_Array is array (Natural range <>) of Var;

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Natural,
      Element_Type => Var,
      Array_Type   => Var_Array);

   function Compile (This            : Context;
                     Check_Conflicts : Boolean)
                     return Var_Array;
   --  Return an array of environment variable key/value built from the
   --  variables previously loaded and defined in the context. During
   --  hashing, we know some paths will conflict with the definitive ones,
   --  so Check_Conflicts allows to skip those checks.

   type Env_Action is record
      Kind   : Alire.Properties.Environment.Actions;
      Value  : Ada.Strings.Unbounded.Unbounded_String;
      Origin : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Action_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Env_Action);

   package Action_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Action_Vectors.Vector,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => Action_Vectors."=");

   type Context is tagged limited record
      Actions : Action_Maps.Map;
   end record;

   procedure Add (This : in out Context; Name : String; Action : Env_Action);

end Alire.Environment;
