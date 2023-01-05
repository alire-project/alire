package body GPR_Generator
is
   procedure Gnüf;
   procedure Indent;
   procedure For_Use_Skeleton (Name : String);

   ---------------------
   --  Project_Begin  --
   ---------------------

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

   -------------------
   --  Project_End  --
   -------------------

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

   ---------------------
   --  Package_Begin  --
   ---------------------

   procedure Package_Begin (Name : Package_Name)
   is
      L : Positive renames Indent_Level;
   begin
      Indent;
      Put ("package ");
      Put (Name);
      if NL_Before_IS then
         New_Line;
         Indent;
         Put ("is ");
      else
         Put (" is");
      end if;
      L := L + 1;
   end Package_Begin;

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

   ---------------
   --  For_Use  --
   ---------------

   procedure For_Use (Name  : For_Clause_Name;
                      Value : Expr)
   is
--      use Ada.Strings.Unbounded;
   begin
      For_Use_Skeleton (Name);
      Put (To_String (Value));
      New_Line;
   end For_Use;

   ---------------
   --  For_Use  --
   ---------------

   procedure For_Use (Name : For_Clause_Name;
                      List : Expr_List)
   is
--      use Ada.Strings.Unbounded;
      L : Natural renames Indent_Level;
   begin
      For_Use_Skeleton (Name);
      New_Line;
      L := L + 1;
      Indent;
      Put ("(");
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
      Put (");");
      L := L - 1;
   end For_Use;

   -----------------------
   --  For_Use_Skeleton  --
   ------------------------

   procedure For_Use_Skeleton (Name : String)
   is
   begin
      Indent;
      Put ("for ");
      Put (Name);
      if NL_Before_USE then
         New_Line;
         Put ("use ");
      else
         Put (" use ");
      end if;
   end For_Use_Skeleton;

   ------------------
   -- With_Clause  --
   ------------------

   procedure With_Clause (Name : GPR_File_Name)
   is
   begin
      Indent;
      Put ("with ");
      Gnüf;
      Put (Name);
      Gnüf;
      Put (";");
      New_Line;
   end With_Clause;

   -------------
   --  Quote  --
   -------------

   function Quote (Item : String)
                   return Expr
   is
      use Ada.Strings.Unbounded;
   begin
      return '"' & To_Unbounded_String (Item) & '"';
   end Quote;

   ---------------
   --  Comment  --
   ---------------

   procedure Comment (Item : String)
   is
   begin
      Indent;
      Put ("--  ");
      Put (Item);
      New_Line;
   end Comment;

   ------------
   --  Free  --
   ------------

   procedure Free (Item : String)
   is
   begin
      Indent;
      Put (Item);
      New_Line;
   end Free;

   ------------
   --  Gnüf  --
   ------------

   procedure Gnüf
   is
   begin
      Put ("""");
   end Gnüf;

   --------------
   --  Indent  --
   --------------

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
