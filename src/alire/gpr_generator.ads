with Ada.Strings.Unbounded;

generic
   with procedure Put (Item : String) is <>;
   with procedure New_Line is <>;
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

   NL_Before_IS  : Boolean := True;
   NL_Before_USE : Boolean := True;
   Show_LIBRARY  : Boolean := False;
   Indent_Level  : Natural := 0;
   Space_Count   : Natural := 3;

   --------------------
   --  Sub-programs  --
   --------------------

   procedure Project_Begin (Name : Project_Name;
                            Kind : Project_Kind);
   procedure Project_End (Name : Project_Name);
   --  Begin/end PROJECT.

   procedure Package_Begin (Name : Package_Name);
   procedure Package_End (Name : Project_Name);
   --  Begin/end PACKAGE.

   procedure For_Use (Name  : For_Clause_Name;
                      Value : Expr);
   procedure For_Use (Name : For_Clause_Name;
                      List : Expr_List);
   --  Insert FOR .. USE clause.

   procedure With_Clause (Name : GPR_File_Name);
   --  Insert WITH clause.

   procedure Comment (Item : String);
   --  Insert stand alone comment.

   procedure Free (Item : String);
   --  Insert free form text.

   function Quote (Item : String)
                   return Expr;
   --  Quote Item.

   package Shortcuts
   is
      procedure WC (Name : GPR_File_Name)
        renames With_Clause;

      procedure FU (Name  : For_Clause_Name;
                    Value : Expr)
        renames For_Use;

      procedure FU (Name : For_Clause_Name;
                    List : Expr_List)
        renames For_Use;

   end Shortcuts;

end GPR_Generator;
