with Ada.Characters.Handling;

package body Alire.Versions is

   ---------------------
   -- From_Identifier --
   ---------------------

   function From_Identifier (S : String) return Semantic_Versioning.Version is

      -----------
      -- Error --
      -----------

      procedure Error is
      begin
         Trace.Error ("Given identifier cannot be a version: " & S);
         raise Constraint_Error;
      end Error;

      use Ada.Characters.Handling;
      Img   : String   := S;
      First : Positive := Img'First;
      Seen  : Natural  := 0;
   begin
      if Img'Length < 2 then
         Error;
      end if;

      --  Skip leading text
      while First <= Img'Last and then Is_Letter (Img (First)) loop
         First := First + 1;
      end loop;

      if First > Img'Last then
         Error;
      end if;

      --  Skip optional underscore
      if Img (First) = '_' then
         First := First + 1;
      end if;

      --  Convert remaining underscores to proper symbol:
      for I in First + 1 .. Img'Last loop
         if S (I) = '_' then
            Seen := Seen + 1;
            case Seen is
               when 1 .. 2 => Img (I) := '.';
               when 3      => Img (I) := '-';
               when 4      => Img (I) := '+';
               when others => exit;
            end case;
         end if;
      end loop;

      return Semantic_Versioning.Relaxed (Img (First .. S'Last));
   end From_Identifier;

end Alire.Versions;
