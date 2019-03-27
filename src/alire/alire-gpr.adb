package body Alire.GPR is

   -----------
   -- Image --
   -----------

   function Image (V : Variable) return String is

      function Listify (Vals : Value_Vector) return String is
         Head : constant String := Vals.First_Element;
         Tail : Value_Vector := Vals;
      begin
         Tail.Delete_First;

         return Head &
         (if Tail.Is_Empty
          then ""
          else " | " & Listify (Tail));
      end Listify;

   begin
      case V.Kind is
         when Free_String =>
            return V.Name & " := <string>";
         when Enumeration =>
            return V.Name & " := " & Listify (V.Values);
         when External =>
            return V.Name & " := " & V.Value.First_Element;
      end case;
   end Image;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument (S : in out Scenario; Var : String; Val : String) is
   begin
      S.Append (Var);
      S.Append (Val);
   end Add_Argument;

   ---------------------
   -- As_Command_Line --
   ---------------------

   function As_Command_Line (S : Scenario) return String is

   -------------
   -- Listify --
   -------------

      function Listify (S : Scenario) return String is
         Var : constant String := S (1);
         Val : constant String := S (2);
         Cdr : Scenario := S;
      begin
         Cdr.Delete_First;
         Cdr.Delete_First;
         return "-X" & Var & "=" & Val &
           (if Cdr.Is_Empty then "" else " " & Listify (Cdr));
      end Listify;

   begin
      return (if S.Is_Empty then "" else Listify (S));
   end As_Command_Line;

end Alire.GPR;
