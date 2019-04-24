private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Ordered_Sets;
private with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Hash;

private with TOML;
private with Semantic_Versioning;

private with Alire.Actions;
with Alire.Containers;
with Alire.TOML_Expressions;
private with Alire.Licensing;
with Alire.Platforms;
with Alire.Releases;

package Alire.TOML_Index is

   function Valid_Package_Name (Name : String) return Boolean;
   --  Return whether the given name is a valid package name

   type Load_Result (Success : Boolean := True) is record
      case Success is
         when True  => null;
         when False => Message : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function Error_Message (Result : Load_Result) return String
      with Pre => not Result.Success;
   --  Return the error message in Result

   subtype Environment_Variables is
      Alire.TOML_Expressions.Environment_Variables;

   procedure Set_Environment
     (Env      : in out Environment_Variables;
      Distrib  : Platforms.Distributions;
      OS       : Platforms.Operating_Systems;
      Compiler : Platforms.Compilers);
   --  Initialize Env according to the given distribution and OS

   procedure Load_Catalog
     (Catalog_Dir : String;
      Environment : Environment_Variables;
      Result      : out Load_Result);
   --  Load the whole TOML catalog from the given directory and using the given
   --  environment variables.

   procedure Load_From_Catalog
     (Catalog_Dir, Package_Name : String;
      Environment               : Environment_Variables;
      Result                    : out Load_Result);
   --  Load a specific package from the TOML catalog in the given directory
   --  using the given environment variables.  This does nothing if the package
   --  is already loaded.

   procedure Load_Release_From_File
     (Filename    : String;
      Environment : Environment_Variables;
      Release     : out Containers.Release_Holders.Holder;
      Result      : out Load_Result) with
     Post => Result.Success = not Release.Is_Empty;
   --  Load a file that must contain a single release.

private

   package US renames Ada.Strings.Unbounded;

   function "+" (S : String) return US.Unbounded_String
      renames US.To_Unbounded_String;
   function "+" (S : US.Unbounded_String) return String
      renames US.To_String;

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => US.Unbounded_String,
      "="          => US."=");

   package String_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => US.Unbounded_String,
      "="          => US."=",
      "<"          => US."<");

   --  Intermediate representation for decoded TOML packages

   --------------
   -- Licenses --
   --------------

   --  A license is either one "natively handled", or a custom one, so
   --  including the text for the whole license.

   type License_Type (Custom : Boolean := False) is record
      case Custom is
         when False => License : Licensing.Licenses;
         when True  => Text    : US.Unbounded_String;
      end case;
   end record;

   package License_Vectors is new Ada.Containers.Vectors
     (Positive, License_Type);

   ------------------------------
   -- Dependencies expressions --
   ------------------------------

   --  Dependencies: a dependency is a name, for the package on which to
   --  depend, plus an optional version constraint, to represent the set of
   --  versions that can satisfy this dependency.

   type Dependency (Any : Boolean := False) is record
      Name : US.Unbounded_String;

      case Any is
         when False => Versions : Semantic_Versioning.Version_Set;
         when True  => null;
      end case;
   end record;

   package Dependency_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => US.Unbounded_String,
      Element_Type    => Dependency,
      Equivalent_Keys => US."=",
      Hash            => US.Hash);

   package Dependencies_Result is new Alire.TOML_Expressions.Computation_Result
     (Dependency_Maps.Map);

   procedure Add_Dependency
     (D : Dependency; Result : in out Dependencies_Result.T);
   --  If Result.Success is false, do nothing. Otherwise, try to add D to
   --  Result.Value. In case of error, replace Result with the corresponding
   --  error information.

   procedure Parse_Literal
     (Value : TOML.TOML_Value; Result : out Dependencies_Result.T);
   --  Turn Value into a vector of dependencies

   procedure Merge
     (Left, Right : Dependency_Maps.Map;
      Result      : out Dependencies_Result.T);
   --  Merge two dependency vectors into one. This turns duplicate entries into
   --  errors.

   package Dependencies_Expressions is
      new Alire.TOML_Expressions.Composite_Values
        (Value_Type        => Dependency_Maps.Map,
         Evaluation_Result => Dependencies_Result);

   -------------------------
   -- Boolean expressions --
   -------------------------

   package Boolean_Result is new Alire.TOML_Expressions.Computation_Result
     (Boolean);

   procedure Parse_Literal
     (Value : TOML.TOML_Value; Result : out Boolean_Result.T);
   --  Turn Value into a boolean, or an error if Value is not a TOML boolean
   --  literal node.

   package Boolean_Expressions is new Alire.TOML_Expressions.Single_Values
     (Value_Type        => Boolean,
      Evaluation_Result => Boolean_Result);

   --  package Dependencies_Expressions is new Expressions
   --    (Dependency_Vectors.Vector);

   ------------------------
   -- String expressions --
   ------------------------

   package String_Result is new Alire.TOML_Expressions.Computation_Result
     (US.Unbounded_String);

   procedure Parse_Literal
     (Value : TOML.TOML_Value; Result : out String_Result.T);
   --  Turn Value into a string, or an error if Value is not a TOML string
   --  literal node.

   package String_Expressions is new Alire.TOML_Expressions.Single_Values
     (Value_Type        => US.Unbounded_String,
      Evaluation_Result => String_Result);

   ------------------------
   -- Origin expressions --
   ------------------------

   type Origin_Kind is (Git, Mercurial, SVN, Source_Archive, Native_Package);
   subtype VCS_Origin_Kind is Origin_Kind range Git .. SVN;

   type Origin_Type (Kind : Origin_Kind := Git) is record
      case Kind is
         when VCS_Origin_Kind =>
            Repo_URL : US.Unbounded_String;
            Revision : US.Unbounded_String;

         when Source_Archive =>
            Archive_URL : US.Unbounded_String;

         when Native_Package =>
            Package_Name : US.Unbounded_String;
      end case;
   end record;

   package Origin_Result is new Alire.TOML_Expressions.Computation_Result
     (Origin_Type);

   procedure Parse_Literal
     (Value : TOML.TOML_Value; Result : out Origin_Result.T);
   --  Turn Value into an origin, or an error if Value is not a valid TOML
   --  origin representation.

   package Origin_Expressions is new Alire.TOML_Expressions.Single_Values
     (Value_Type        => Origin_Type,
      Evaluation_Result => Origin_Result);

   -------------------
   -- GPR externals --
   -------------------

   type GPR_Externals_Values is access all String_Sets.Set;
   type GPR_Set_Externals_Expr is access all String_Expressions.Expression;

   package GPR_Externals_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => US.Unbounded_String,
      Element_Type    => GPR_Externals_Values,
      Equivalent_Keys => US."=",
      Hash            => US.Hash);
   --  Associate GPR external variables to lists of possible values (or empty
   --  sets if all possible values are accepted).

   package GPR_Set_Externals_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => US.Unbounded_String,
      Element_Type    => GPR_Set_Externals_Expr,
      Equivalent_Keys => US."=",
      Hash            => US.Hash);
   --  Associate GPR external variables to expressions that provide values for
   --  these variables.

   -------------
   -- Actions --
   -------------

   type Action is record
      Kind    : Actions.Moments;
      Command : US.Unbounded_String;
   end record;

   package Action_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Action);

   type Common_Data is record
      Notes : US.Unbounded_String;

      Dependencies : Dependencies_Expressions.Expression;

      Available : Boolean_Expressions.Expression;

      Project_Files : String_Vectors.Vector;

      GPR_Externals     : GPR_Externals_Maps.Map;
      GPR_Set_Externals : GPR_Set_Externals_Maps.Map;

      Executables : String_Vectors.Vector;

      Actions : Action_Vectors.Vector;
   end record;

   --------------
   -- Releases --
   --------------

   type Release_Record is record
      Version      : Semantic_Versioning.Version;
      Origin       : Origin_Expressions.Expression;
      Archive_Name : US.Unbounded_String;

      Common : Common_Data;
   end record;

   type Release is access all Release_Record;

   function Hash
     (Version : Semantic_Versioning.Version) return Ada.Containers.Hash_Type
   is (US.Hash (+Semantic_Versioning.Image (Version)));

   package Release_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Semantic_Versioning.Version,
      Element_Type    => Release,
      Equivalent_Keys => Semantic_Versioning."=",
      Hash            => Hash);

   type Package_Type is record
      Name        : US.Unbounded_String;
      Description : US.Unbounded_String;
      Website     : US.Unbounded_String;

      Authors, Maintainers : String_Vectors.Vector;
      Licenses             : License_Vectors.Vector;

      Common : Common_Data;

      Releases : Release_Maps.Map;
   end record;

   --  TODO: automate memory management

   procedure Decode_TOML_Package
     (Filename, Package_Name : String;
      Environment            : Environment_Variables;
      Value                  : TOML.TOML_Value;
      Pkg                    : out Package_Type;
      Result                 : out Load_Result);
   --  Load data from Value and create a package out of it in Pkg, setting
   --  Result.Success to true. If the package description is invalid, set it to
   --  false and produce an error message.

   procedure Decode_TOML_Package_As_Releases
     (Pkg         : Package_Type;
      Environment : Environment_Variables;
      Releases    : out Containers.Release_Sets.Set);
   --  Generate one fully populated release per version loaded in Pkg

   procedure Index_Releases
     (Pkg      : Package_Type;
      Releases : Containers.Release_Sets.Set);
   --  Add the given releases to the internal index

end Alire.TOML_Index;
