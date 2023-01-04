with Ada.Strings.Unbounded;

generic
package GPR_Generator
is
   -------------
   --  Types  --
   -------------

   subtype GPR_File_Name   is String;
   subtype Project_Name    is String;
   subtype Package_Name    is String;
   subtype For_Clause_Name is String;

   type Expr
     is new Ada.Strings.Unbounded.Unbounded_String;
   type Expr_List
     is array (Positive range <>)
     of Expr;

   type Project_Kind
     is (Library     -- library project (ordinary)
        , Abstrakt   -- abstract project
        , Aggregate  -- aggregate project
        );

   --------------
   --  Config  --
   --------------

   LF_Before_IS : Boolean := True;
   Show_LIBRARY : Boolean := False;

   --------------------
   --  Sub-programs  --
   --------------------

   procedure Project_Begin (Name : Project_Name;
                            Kind : Project_Kind);
   procedure Project_End;
   --  Begin/end PROJECT.

   procedure Package_Begin (Name : Package_Name);
   procedure Package_End;
   --  Begin/end PACKAGE.

   procedure For_Use (Name  : For_Clause_Name;
                      Value : Expr);
   procedure For_Use (Name : For_Clause_Name;
                      List : Expr_List);
   --  Insert FOR .. USE clause.

   procedure With_Clause (Name : GPR_File_Name);
   --  Insert WITH clause.

   function Quote (Item : String)
                   return Expr;
   --  Quote Item.

end GPR_Generator;
