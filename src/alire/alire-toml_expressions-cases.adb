with Alire.Conditional_Trees.Cases;
with Alire.Platforms;
with Alire.Requisites.Booleans;
with Alire.Requisites.Platform;
with Alire.TOML_Keys;

package body Alire.TOML_Expressions.Cases is

   procedure Set_Up_Loaders;
   --  MUST be called before attempting to load anything.
   --  Needed to keep the package in Preelaborable.

   package Compilers         is new Enum_Cases (Platforms.Compilers);
   package Distributions     is new Enum_Cases (Platforms.Distributions);
   package Operating_Systems is new Enum_Cases (Platforms.Operating_Systems);
   package Word_Sizes        is new Enum_Cases (Platforms.Word_Sizes);

   ------------------------------------------------
   --  COMMON REQUISITES/PROPERTIES SCAFFOLDING  --
   ------------------------------------------------

   --  This generic saves duplication of instances for requisites and
   --  properties.

   generic
      with package Condtrees is new Conditional_Trees (<>);
   package Common_Cases is

      package Compilers is
        new Condtrees.Cases (Requisites.Platform.Compiler_TOML_Cases);

      package Distributions is
         new Condtrees.Cases (Requisites.Platform.Distro_Cases);

      package Operating_Systems is
        new Condtrees.Cases (Requisites.Platform.OS_Cases);

      package Word_Sizes is
        new Condtrees.Cases (Requisites.Platform.Word_Size_Cases);

   end Common_Cases;

   --  The following packages create the actual Case_Node classes for the
   --  respective conditional trees of properties and dependencies.
   --  These instances are later used in TOML loading of expressions.

   package Cases_Deps  is new Common_Cases (Conditional.For_Dependencies);
   package Cases_Props is new Common_Cases (Conditional.For_Properties);

   -----------------------------------
   --  CASE REQUISITES SCAFFOLDING  --
   -----------------------------------

   package Reqs is

      --  Requisites have differences in regard to Conditional_Trees that
      --  make a common generic impossible (or not evident).

      package Trees is new Enum_Trees
        (Tree    => Requisites.Tree,
         "and"   => Requisites.Trees."and",
         Default => Requisites.Booleans.Always_True);

      Loaders : array (Case_Loader_Keys) of Trees.Recursive_Case_Loader :=
                  (others => null);

      function Loader (Key : Case_Loader_Keys)
                       return Trees.Recursive_Case_Loader is (Loaders (Key));

      function Load_Instance is new Trees.Load
        (Case_Keys => Case_Loader_Keys,
         Loaders   => Loader);

      --  Requisite loader instances:
      package Compiler_Loader is new Tree_Builders
        (Trees      => Trees,
         Cases      => Compilers,
         Enum_Array => Requisites.Platform.Compiler_TOML_Cases.Cases_Array,
         New_Leaf   => Requisites.Platform.Compiler_TOML_Cases.New_Case,
         Load       => Load_Instance);
      package Distro_Loader is new Tree_Builders
        (Trees      => Trees,
         Cases      => Distributions,
         Enum_Array => Requisites.Platform.Distro_Cases.Cases_Array,
         New_Leaf   => Requisites.Platform.Distro_Cases.New_Case,
         Load       => Load_Instance);
      package OS_Loader is new Tree_Builders
        (Trees      => Trees,
         Cases      => Operating_Systems,
         Enum_Array => Requisites.Platform.OS_Cases.Cases_Array,
         New_Leaf   => Requisites.Platform.OS_Cases.New_Case,
         Load       => Load_Instance);
      package WS_Loader is new Tree_Builders
        (Trees      => Trees,
         Cases      => Word_Sizes,
         Enum_Array => Requisites.Platform.Word_Size_Cases.Cases_Array,
         New_Leaf   => Requisites.Platform.Word_Size_Cases.New_Case,
         Load       => Load_Instance);

   end Reqs;

   ---------------------
   -- Load_Requisites --
   ---------------------

   function Load_Requisites (From : TOML_Adapters.Key_Queue)
                             return Requisites.Tree is
   begin
      Set_Up_Loaders;
      return Reqs.Load_Instance (TOML_Keys.Available,
                                 From,
                                 Requisites.Booleans.From_TOML'Access);
   end Load_Requisites;

   -------------------------------------------
   --  CASE CONDITIONAL COMMON SCAFFOLDING  --
   -------------------------------------------

   --  The following actually provides all that is necessary to load cases of
   --  dependencies or properties.

   generic
      with package Condtrees is new Conditional_Trees (<>);
      with package Condcases is new Common_Cases (Condtrees);
   package Conditional_Instances is

      --  Given the common base of Dependencies and Properties, we can reuse
      --  some structure here.

      package Trees is new Enum_Trees (Tree  => Condtrees.Tree,
                                       "and" => Condtrees."and",
                                       Default => Condtrees.Empty);
      --  I.e., Conditional.Dependencies & Conditional.Properties

      Loaders : array (Case_Loader_Keys) of Trees.Recursive_Case_Loader :=
                  (others => null);

      function Loader (Key : Case_Loader_Keys)
                       return Trees.Recursive_Case_Loader is (Loaders (Key));

      function Load_Instance is new Trees.Load
        (Case_Keys => Case_Loader_Keys,
         Loaders   => Loader);

      --  Requisite loader instances:
      package Compiler_Loader is new Tree_Builders
        (Trees      => Trees,
         Cases      => Compilers,
         Enum_Array => Condcases.Compilers.Cases_Array,
         New_Leaf   => Condcases.Compilers.New_Case,
         Load       => Load_Instance);
      package Distro_Loader is new Tree_Builders
        (Trees      => Trees,
         Cases      => Distributions,
         Enum_Array => Condcases.Distributions.Cases_Array,
         New_Leaf   => Condcases.Distributions.New_Case,
         Load       => Load_Instance);
      package OS_Loader is new Tree_Builders
        (Trees      => Trees,
         Cases      => Operating_Systems,
         Enum_Array => Condcases.Operating_Systems.Cases_Array,
         New_Leaf   => Condcases.Operating_Systems.New_Case,
         Load       => Load_Instance);
      package WS_Loader is new Tree_Builders
        (Trees      => Trees,
         Cases      => Word_Sizes,
         Enum_Array => Condcases.Word_Sizes.Cases_Array,
         New_Leaf   => Condcases.Word_Sizes.New_Case,
         Load       => Load_Instance);

      procedure Set_Up_Loaders;

   end Conditional_Instances;

   package body Conditional_Instances is

      procedure Set_Up_Loaders is
      begin
         Loaders := (Compiler     => Compiler_Loader.Load_Cases'Access,
                     Distribution => Distro_Loader.Load_Cases'Access,
                     OS           => OS_Loader.Load_Cases'Access,
                     Word_Size    => WS_Loader.Load_Cases'Access);
      end Set_Up_Loaders;

   end Conditional_Instances;

   -------------------------------------
   --  CASE DEPENDENCIES SCAFFOLDING  --
   -------------------------------------

   package Deps is new Conditional_Instances (Conditional.For_Dependencies,
                                              Cases_Deps);

   -----------------------
   -- Load_Dependencies --
   -----------------------

   function Load_Dependencies (From : TOML_Adapters.Key_Queue)
                               return Conditional.Dependencies
   is
   begin
      Set_Up_Loaders;
      return Deps.Load_Instance (TOML_Keys.Depends_On,
                                 From,
                                 Conditional.Deps_From_TOML'Access);
   end Load_Dependencies;

   -------------------------------------
   --  CASE PROPERTIES SCAFFOLDING  --
   -------------------------------------

   package Props is new Conditional_Instances (Conditional.For_Properties,
                                               Cases_Props);

   -------------------
   -- Load_Property --
   -------------------

   function Load_Property (Key    : String;
                           From   : TOML_Adapters.Key_Queue;
                           Loader : Static_Loader)
                           return Conditional.Properties
   is
      use all type TOML.Any_Value_Kind;
   begin
      Set_Up_Loaders;
      return Prop : constant Conditional.Properties :=
        Props.Load_Instance (Key, From, Loader.all'Access)
        --  The Loader type is structurally equivalent, so we can circunvent
        --  the type check without consequences. This is merely for clarity.
      do
         if From.Unwrap.Kind = TOML.TOML_Table then
            From.Report_Extra_Keys;
         end if;
      end return;
   end Load_Property;

   --------------------
   -- Set_Up_Loaders --
   --------------------

   procedure Set_Up_Loaders is
   begin
      Deps.Set_Up_Loaders;
      Props.Set_Up_Loaders;
      Reqs.Loaders := (Compiler     => Reqs.Compiler_Loader.Load_Cases'Access,
                       Distribution => Reqs.Distro_Loader.Load_Cases'Access,
                       OS           => Reqs.OS_Loader.Load_Cases'Access,
                       Word_Size    => Reqs.WS_Loader.Load_Cases'Access);
   end Set_Up_Loaders;

end Alire.TOML_Expressions.Cases;
