with Ada.Characters.Latin_1;

package body Alire.Utils.YAML is

   -------------
   -- To_YAML --
   -------------

   function To_YAML (V : Vector) return String is

      package Latin_1 renames Ada.Characters.Latin_1;
      use all type Vectors.Index_Type;

      function Image (V : Vector; Pos : Vectors.Index_Type) return String is
        (T'Class (V.Element (Pos)).To_YAML &
         (if Pos = V.Last_Index
          then ""
          else "," & Latin_1.LF & Image (V, Pos + 1)));

   begin
      if V.Is_Empty then
         return "[]";
      else
         return "[" & Image (V, V.First_Index) & "]";
      end if;
   end To_YAML;

   --------------------
   -- YAML_Stringify --
   --------------------

   function YAML_Stringify (Input : String) return String is
      package Latin_1 renames Ada.Characters.Latin_1;

      --  Inspired by AdaYaml, (c) 2017 Felix Krause

      Result : String (1 .. Input'Length * 4 + 2);
      --  Worst case is all input characters are escaped to hexadecimal, e.g.
      --  \xff. We also add leading and trailing double quotes.

      Last : Positive := Result'First;
      --  Index of the last character of result string

      ------------
      -- Escape --
      ------------

      procedure Escape (C : Character) is
      begin
         Last := Last + 2;
         Result (Last - 1) := '\';
         Result (Last) := C;
      end Escape;

      -------------------
      -- Escape_To_Hex --
      -------------------

      procedure Escape_To_Hex (C : Character) is

         function To_Hex (X : Natural) return Character
         is (case X is
                when 0  .. 9  => Character'Val (Character'Pos ('0') + X),
                when 10 .. 15 => Character'Val (Character'Pos ('a') + X - 10),
                when others   => 'x')
              with Pre  => X <= 15;

      begin
         Escape ('x');
         Last := Last + 2;
         Result (Last - 1) := To_Hex (Character'Pos (C) / 16);
         Result (Last)     := To_Hex (Character'Pos (C) mod 16);
      end Escape_To_Hex;

   begin

      Result (Last) := '"';
      for C of Input loop
         case C is
            when Latin_1.LF              => Escape ('l');
            when Latin_1.CR              => Escape ('c');
            when '"' | '\'               => Escape (C);
            when Latin_1.HT              => Escape ('t');
            when Latin_1.NUL .. Latin_1.BS
               | Latin_1.VT  .. Latin_1.FF
               | Latin_1.SO  .. Latin_1.US => Escape_To_Hex (C);

            when others =>
               Last := Last + 1;
               Result (Last) := C;
         end case;
      end loop;
      Last := Last + 1;
      Result (Last) := '"';
      return Result (Result'First .. Last);
   end YAML_Stringify;

end Alire.Utils.YAML;
