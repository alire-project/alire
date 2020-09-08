package Alire.Roots.Optional is

   type States is
     (Outside,
      --  There is no alire metadata at all

      Broken,
      --  There is metadata that cannot be loaded, root is unusable

      Valid
      --  There is loadable metadata and the root is usable
     );

   --  Hit a GNAT bug trying to use Outcomes.Indefinite... using custom impl

   type Root (<>) is new Outcome with private;

   type Reference (Ptr : not null access constant Roots.Root)
   is limited null record with
     Implicit_Dereference => Ptr;

   function Detect_Root (Path : Any_Path) return Optional.Root;
   --  Try to detect a root at the given Path

   function Search_Root (From : Any_Path) return Optional.Root;
   --  Try to detect a root at From or any ancestor folder

   function Status (This : Root) return States;

   function Is_Broken (This : Root) return Boolean;

   function Is_Valid (This : Root) return Boolean;

   function Outside (This : Root) return Boolean;
   --  True when there is no root at all, broken or valid

   function Value (This : aliased Root) return Reference with
     Pre => This.Is_Valid;

   function Outcome_Failure (Message : String;
                             Status  : States;
                             Report  : Boolean)
                             return Root
     with Pre => Status in Outside | Broken;

   function Outcome_Success (This : Roots.Root) return Optional.Root;

private

   type Root (Status : States) is new Outcome with record
      case Status is
         when Valid =>
            Value : aliased Roots.Root;
         when others =>
            null;
      end case;
   end record;

   overriding
   function Outcome_Failure (Unused_Message : String;
                             Unused_Report  : Boolean := True)
                             return Root
   is (raise Program_Error with "Status must be provided");

   overriding
   function Outcome_Success return Root
   is (raise Program_Error with
        "A successful non-trivial outcome requires a result");

   overriding
   function Outcome_From_Exception
     (Unused_Ex  : Ada.Exceptions.Exception_Occurrence;
      Unused_Msg : String := "") return Root
   is (raise Program_Error with "Status must be provided");

end Alire.Roots.Optional;
