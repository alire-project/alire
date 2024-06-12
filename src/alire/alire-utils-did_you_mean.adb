with Alire.TOML_Adapters;

package body Alire.Utils.Did_You_Mean with Preelaborate is

   -------------------------------
   -- Levenshtein_Edit_Distance --
   -------------------------------

   function Levenshtein_Edit_Distance (S, T : String) return Natural is
      D : array (0 .. S'Last, 0 .. T'Last) of Natural;
   begin
      for I in D'Range (1) loop
         D (I, 0) := I;
      end loop;

      for J in D'Range (2) loop
         D (0, J) := J;
      end loop;

      for I in S'Range loop
         for J in T'Range loop
            declare
               Cost : constant Natural :=
                 (if S (I) = T (J)
                  then 0
                  else 1);

               A : constant Natural := D (I - 1, J) + 1;
               B : constant Natural := D (I, J - 1) + 1;
               C : constant Natural := D (I - 1, J - 1) + Cost;
            begin

               D (I, J) := Natural'Min (Natural'Min (A, B), C);
            end;
         end loop;
      end loop;

      return D (D'Last (1), D'Last (2));
   end Levenshtein_Edit_Distance;

   ----------------
   -- Suggestion --
   ----------------

   function Suggestion (Input           : String;
                        Possible_Values : AAA.Strings.Vector)
                        return String
   is
      Min_Dist : Natural := Natural'Last;
      Dist : Natural;
      Closest : Positive := Possible_Values.First_Index;
   begin
      for Index in Possible_Values.First_Index .. Possible_Values.Last_Index
      loop
         Dist := Levenshtein_Edit_Distance (Input, Possible_Values (Index));
         if Dist < Min_Dist then
            Min_Dist := Dist;
            Closest := Index;
         end if;
      end loop;

      declare
         Relevant : constant Boolean := Min_Dist < Input'Length / 2;
         --  Heuristic for relevance of suggestion
      begin
         if Relevant then
            return " Did you mean '" & Possible_Values (Closest) & "'?";
         else
            return " Can be: " & Possible_Values.Flatten (", ") & ".";
         end if;
      end;
   end Suggestion;

   ---------------------
   -- Enum_Suggestion --
   ---------------------

   function Enum_Suggestion (Input : String) return String is
      Possible_Values : AAA.Strings.Vector;
   begin
      for V in Enum loop
         Possible_Values.Append
           (case Transform is
               when None => V'Img,
               when Lower_Case => AAA.Strings.To_Lower_Case (V'Img),
               when Upper_Case => AAA.Strings.To_Lower_Case (V'Img),
               when Tomify => TOML_Adapters.Tomify (V'Img));
      end loop;

      return Suggestion (Input, Possible_Values);
   end Enum_Suggestion;

end Alire.Utils.Did_You_Mean;
