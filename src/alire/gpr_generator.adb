package body GPR_Generator
is
   -------------------------
   --  Projecg_Begin/End  --
   -------------------------

   procedure Project_Begin (Name : Project_Name;
                            Kind : Project_Kind)
   is null;

   procedure Project_End is null;

   -------------------------
   --  Package_Begin/End  --
   -------------------------

   procedure Package_Begin (Name : Package_Name)
   is null;

   procedure Package_End
   is null;

   ---------------
   --  For_Use  --
   ---------------

   procedure For_Use (Name  : For_Clause_Name;
                      Value : Expr)
   is null;

   procedure For_Use (Name : For_Clause_Name;
                      List : Expr_List)
   is null;

   ------------------
   -- With_Clause  --
   ------------------

   procedure With_Clause (Name : GPR_File_Name)
   is null;
   --

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
end GPR_Generator;
