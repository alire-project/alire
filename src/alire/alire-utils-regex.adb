with GNAT.Regpat;

package body Alire.Utils.Regex is

   ------------
   -- Escape --
   ------------

   function Escape (Literal : String) return String is
      Targets  : constant String := "\()[].*+?^";
      Result   : UString;
      use UStrings;
   begin
      for Char of Literal loop
         if (for some Nono of Targets => Char = Nono) then
            Append (Result, "\" & Char);
         else
            Append (Result, Char);
         end if;
      end loop;

      return +Result;
   end Escape;

   -----------------
   -- First_Match --
   -----------------

   function First_Match (Regex : String; Text : String) return String is

      -----------------------
      -- Count_Parentheses --
      -----------------------

      function Count_Parentheses return Positive is
         Count : Natural := 0;
      begin
         for Char of Regex loop
            if Char = '(' then
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end Count_Parentheses;

      use GNAT.Regpat;
      Matches : Match_Array (1 .. Count_Parentheses);
      --  This is a safe estimation, as some '(' may not be part of a capture

   begin
      Match (Regex, Text, Matches);

      for I in Matches'Range loop
         if Matches (I) /= No_Match then
            return Text (Matches (I).First .. Matches (I).Last);
         end if;
      end loop;

      return "";
   end First_Match;
end Alire.Utils.Regex;
