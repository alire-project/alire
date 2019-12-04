package body Alire.GPR is

   -----------
   -- Image --
   -----------

   function Image (V : Variable) return String is

      function Listify (Vals : Value_Vector) return String;

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

   function As_Command_Line (S : Scenario) return Utils.String_Vector is
      use Alire.Utils;

      Result : String_Vector := Empty_Vector;

      Cdr : Scenario := S;
   begin
      while not Cdr.Is_Empty loop
         Result.Append (String'("-X" & Cdr (1) & "=" & Cdr (2)));
         Cdr.Delete_First;
            Cdr.Delete_First;
      end loop;
      return Result;
   end As_Command_Line;

end Alire.GPR;
