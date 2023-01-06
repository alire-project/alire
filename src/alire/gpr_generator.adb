package body GPR_Generator
is
   procedure Gnüf;
   --  Emit '"'.

   procedure Indent;
   --  Emit indention.

   procedure For_Use_Skeleton
     (Name    : String;
      Oneline : Boolean);
   --  Emit FOR and USE.

   -------------------
   -- Project_Begin --
   -------------------

   procedure Project_Begin (Name : Project_Name;
                            Kind : Project_Kind)
   is
      L : Positive renames Indent_Level;
   begin
      Indent;
      case Kind is
      when Library =>
         if Show_LIBRARY then
            Put ("library ");
         end if;
      when Abstrakt =>
         Put ("abstract ");
      when Aggregate =>
         Put ("aggregate ");
      end case;
      Put ("project ");
      Put (Name);
      if NL_Before_IS then
         New_Line;
         Put ("is");
      else
         Put (" is");
      end if;
      New_Line;
      L := L + 1;
   end Project_Begin;

   -----------------
   -- Project_End --
   -----------------

   procedure Project_End (Name : String)
   is
      L : Positive renames Indent_Level;
   begin
      L := L - 1;
      Indent;
      Put ("end ");
      Put (Name);
      Put (";");
      New_Line;
   end Project_End;

   -------------------
   -- Package_Begin --
   -------------------

   procedure Package_Begin (Name : Package_Name)
   is
      L : Positive renames Indent_Level;
   begin
      --  Indent
      Indent;

      --  Package
      Put ("package ");

      --  Name
      Put (Name);

      --  Is
      if NL_Before_IS then
         --  NL, Is
         New_Line;
         Indent;
         Put ("is ");
      else
         --  Is, Inline
         Put (" is");
      end if;

      --  New line
      New_Line;
      L := L + 1;
   end Package_Begin;

   -----------------
   -- Package_End --
   -----------------

   procedure Package_End (Name : String)
   is
      L : Positive renames Indent_Level;
   begin
      L := L - 1;
      Indent;
      Put ("end ");
      Put (Name);
      Put (";");
      New_Line;
   end Package_End;

   -------------
   -- For_Use --
   -------------

   procedure For_Use
     (Name    : For_Clause_Name;
      Value   : Expr;
      Oneline : Boolean := False) is
   begin
      --  For, Use
      For_Use_Skeleton (Name, Oneline);

      --  Value
      Put (To_String (Value));

      --  Semicolon
      Put (";");

      --  NL
      New_Line;

      --  Space
      if NL_After_FOR_Expr then
         New_Line;
      end if;
   end For_Use;

   -------------
   -- For_Use --
   -------------

   procedure For_Use
     (Name    : For_Clause_Name;
      List    : Expr_List;
      Oneline : Boolean  := True;
      Compact : Boolean  := True)
   is
      L : Natural renames Indent_Level;
   begin
      --  For, Use
      For_Use_Skeleton (Name, Oneline => Oneline);

      --  Indent
      if not Compact then
         New_Line;
         L := L + 1;
         Indent;
      end if;

      --  List
      Put ("(");
      if Compact then
         --  Compact list
         for A in List'Range loop
            Put (To_String (List (A)));
            if A /= List'Last then
               Put (", ");
            end if;
         end loop;
      else
         --  Expanded list
         for A in List'Range loop
            if A = List'First then
               Put (" ");
            else
               Put (", ");
            end if;
            Put (To_String (List (A)));
            New_Line;
            Indent;
         end loop;
      end if;
      Put (");");

      --  New_Line
      New_Line;
      if NL_After_FOR_List then
         New_Line;
      end if;
      if not Compact then
         L := L - 1;
      end if;
   end For_Use;

   ----------------------
   -- For_Use_Skeleton --
   ----------------------

   procedure For_Use_Skeleton
     (Name    : String;
      Oneline : Boolean) is
   begin
      --  Indent
      Indent;

      --  For
      Put ("for ");
      Put (Name);

      --  Use
      if Oneline then
         --  Inline Use
         Put (" use ");
      else
         --  NL Use
         if NL_Before_USE then
            New_Line;
            Indent;
            Put ("use ");
         else
            Put (" use ");
         end if;
      end if;
   end For_Use_Skeleton;

   -----------------
   -- With_Clause --
   -----------------

   procedure With_Clause
     (Name : GPR_File_Name) is
   begin
      --  Indent
      Indent;

      --  With
      Put ("with ");

      --  Name in quotes
      Gnüf;
      Put (Name);
      Gnüf;

      --  Semicolon
      Put (";");
      New_Line;
   end With_Clause;

   -------------------------
   -- Expression_List_One --
   -------------------------

   function Expression_List_One
     (Item : String)
      return Expr_List is
   begin
      return
        Expression_List
          (Quoted_Expression (Item));
   end Expression_List_One;

   ----------------
   -- Expression --
   ----------------

   function Expression
     (Item : String)
      return Expr is
   begin
      return To_Unbounded_String (Item);
   end Expression;

   ---------------------
   -- Expression_List --
   ---------------------

   function Expression_List
     (List : Expr)
      return Expr_List is
   begin
      return Expr_List'(1 => List);
   end Expression_List;

   -----------------------
   -- Quoted_Expression --
   -----------------------

   function Quoted_Expression
     (Item : String)
      return Expr
   is
      use Ada.Strings.Unbounded;
   begin
      return '"' & To_Unbounded_String (Item) & '"';
   end Quoted_Expression;

   -------------
   -- Comment --
   -------------

   procedure Comment (Item : String) is
   begin
      Indent;
      Put ("--  ");
      Put (Item);
      New_Line;
   end Comment;

   ----------
   -- Free --
   ----------

   procedure Free (Item : String) is
   begin
      Indent;
      Put (Item);
      New_Line;
   end Free;

   ----------
   -- Gnüf --
   ----------

   procedure Gnüf
   is
   begin
      Put ("""");
   end Gnüf;

   ------------
   -- Indent --
   ------------

   procedure Indent
   is
   begin
      for Level in 1 .. Indent_Level loop
         for Space in 1 .. Space_Count loop
            Put (" ");
         end loop;
      end loop;
   end Indent;

end GPR_Generator;
