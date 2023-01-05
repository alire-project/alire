with Ada.Strings.Unbounded;

generic
   with procedure Put (Item : String) is <>;
   with procedure New_Line is <>;
package GPR_Generator
is
   -----------
   -- Types --
   -----------

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

   ------------
   -- Config --
   ------------

   NL_Before_IS      : Boolean := True;
   NL_Before_USE     : Boolean := True;
   NL_After_FOR_List : Boolean := True;
   NL_After_FOR_Expr : Boolean := False;
   NL_After_PACKAGE  : Boolean := True;
   Show_LIBRARY      : Boolean := False;

   -----------
   -- State --
   -----------

   Indent_Level  : Natural := 0;
   Space_Count   : Natural := 3;

   ------------------
   -- Sub-programs --
   ------------------

   procedure Project_Begin (Name : Project_Name;
                            Kind : Project_Kind);
   procedure Project_End (Name : Project_Name);
   --  Begin/end PROJECT.

   procedure Package_Begin (Name : Package_Name);
   procedure Package_End (Name : Project_Name);
   --  Begin/end PACKAGE.

   procedure For_Use
     (Name    : For_Clause_Name;
      Value   : Expr;
      Oneline : Boolean := False);
   --  Insert FOR .. USE expression.

   procedure For_Use
     (Name    : For_Clause_Name;
      List    : Expr_List;
      Oneline : Boolean  := True;
      Compact : Boolean  := True);
   --  Insert FOR .. USE expression list.

   procedure With_Clause (Name : GPR_File_Name);
   --  Insert WITH clause.

   procedure Comment (Item : String);
   --  Insert stand alone comment.

   procedure Free (Item : String);
   --  Insert free form text.

   function Expression_List
     (List : Expr)
      return Expr_List;
   --  Expression list.

   function Expression_List_One
     (Item : String)
      return Expr_List;
   --  Expression list of one element.

   function Expression
     (Item : String)
      return Expr;
   --  Expression.

   function Quoted_Expression
     (Item : String)
      return Expr;
   --  Quote expression.

   package Shortcuts
   is
      subtype Project_Kind
        is GPR_Generator.Project_Kind;

      procedure RB (Name : Project_Name;
                    Kind : Project_Kind)
        renames Project_Begin;

      procedure RE (Name : Project_Name)
        renames Project_End;

      procedure PB (Name : Package_Name)
        renames Package_Begin;

      procedure PE (Name : Project_Name)
        renames Package_End;

      procedure WC (Name : GPR_File_Name)
        renames With_Clause;

      procedure FU
        (Name    : For_Clause_Name;
         Value   : Expr;
         Oneline : Boolean := True)
        renames For_Use;

      procedure FU
        (Name    : For_Clause_Name;
         List    : Expr_List;
         Oneline : Boolean := True;
         Compact : Boolean := True)
        renames For_Use;

      function E (Item : String) return Expr
        renames Expression;

      function Q (Item : String) return Expr
        renames Quoted_Expression;

      function L (List : Expr) return Expr_List
        renames Expression_List;

      function A (Item : String) return Expr_List
        renames Expression_List_One;

      procedure CO (Item : String)
        renames Comment;

      procedure FR (Item : String)
        renames Free;

   end Shortcuts;

end GPR_Generator;
