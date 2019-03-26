with Alire.Dependencies.Vectors;
with Alire.Properties;

package Alire.Properties.Dependencies with Preelaborate is

   type Availability_Checker is new Property with private;

   type Checker_Function is access function (Dependencies : Alire.Dependencies.Vectors.Vector)
                                             return Boolean;
   --  The platform-bound function that says if a dependency is available
   --  Currently, Alr.Query.Is_Resolvable

   function New_Property (Checker : Checker_Function;
                          Props   : Properties.Vector) return Vector;

   function Checker (This : Availability_Checker) return Checker_Function;

   function Properties (This : Availability_Checker) return Properties.Vector;
   --  Contained platform properties

private

   type Availability_Checker is new Property with record
      Checker    : Checker_Function;   -- Needed to check resolution
      Properties : Alire.Properties.Vector;  -- Needed to materialize the conditional dependencies
   end record;

   overriding function Image (This : Availability_Checker) return String is
     ("Availability_Resolver");

   function New_Property (Checker : Checker_Function;
                          Props   : Alire.Properties.Vector) return Vector is
     (+Availability_Checker'(Checker    => Checker,
                             Properties => Props));

   function Checker (This : Availability_Checker) return Checker_Function is
     (This.Checker);

   function Properties (This : Availability_Checker) return Alire.Properties.Vector is
      (This.Properties);

   -- FIXME currently there is no recursivity detection so mutually dependent packages will enter infinite loop

end Alire.Properties.Dependencies;
