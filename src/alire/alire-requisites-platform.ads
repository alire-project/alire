with Alire.Conditional;
with Alire.Platforms;
with Alire.Properties.Platform;

with Alire.Requisites.Comparables;

package Alire.Requisites.Platform with Preelaborate is

   package Ps   renames Platforms;
   package PrPl renames Properties.Platform;

   use all type Ps.Compilers;
   use all type Tree;

   package Op_Systems is new Comparables
     (Ps.Operating_Systems, Ps."<", Ps.Operating_Systems'Image,
      PrPl.Operating_Systems.Property,
      PrPl.Operating_Systems.Element,
      "OS");

   package Op_System_Cases is new Conditional.For_Properties.Case_Statements
     (Ps.Operating_Systems, Op_Systems.Is_Equal_To);

   package Compilers is new Comparables
     (Ps.Compilers, Ps."<", Ps.Compilers'Image,
      PrPl.Compilers.Property,
      PrPl.Compilers.Element,
      "Compiler");

   use all type Compilers.Comparable;
   function Compiler is new Compilers.Factory;

   function Compiler_Is_Native return Tree is
     (Compiler >= GNAT_FSF_Old and Compiler < GNAT_GPL_Old);

   package Compiler_Cases is new Conditional.For_Properties.Case_Statements
     (Ps.Compilers, Compilers.Is_Equal_To);

   package Distributions is new Comparables
     (Ps.Distributions, Ps."<", Ps.Distributions'Image,
      PrPl.Distributions.Property,
      PrPl.Distributions.Element,
      "Distribution");

   package Distribution_Cases_Deps is new Conditional.For_Dependencies.Case_Statements
     (Ps.Distributions, Distributions.Is_Equal_To);

   package Distribution_Cases_Props is new Conditional.For_Properties.Case_Statements
     (Ps.Distributions, Distributions.Is_Equal_To);

   package Targets is new Comparables
     (Ps.Targets, Ps."<", Ps.Targets'Image,
      PrPl.Targets.Property,
      PrPl.Targets.Element,
      "Target");

   package Versions is new Comparables
     (Ps.Versions, Ps."<", Ps.Versions'Image,
      PrPl.Versions.Property,
      PrPl.Versions.Element,
      "Version");

   package Word_Sizes is new Comparables
     (Ps.Word_Sizes, Ps."<", Ps.Word_Sizes'Image,
      PrPl.Word_Sizes.Property,
      PrPl.Word_Sizes.Element,
      "Word_Size");

end Alire.Requisites.Platform;
