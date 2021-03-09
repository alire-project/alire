package body Alire.Expressions.Maps is

   ------------
   -- Insert --
   ------------

   procedure Insert (M : in out Map; V : String; E : Elements)
   is
   begin
      if V = TOML_Keys.Case_Others or else V = "others" then
         M.Set_Others (E);
      else
         if not M.Base.Is_Valid (V) and then Force then
            Trace.Debug ("Storing unknown value '" & V & "' for enumeration '"
                         & Key (M.Base) & "'");
         end if;
         M.Entries.Insert (V, E);
      end if;
   end Insert;

   ----------
   -- Keys --
   ----------

   function Keys (M              : Map;
                  Ada_Like       : Boolean;
                  Exclude_Others : Boolean)
                  return Key_Array
   is
      Pos    : Positive := 1;
      Result : Key_Array (1 .. Natural (M.Entries.Length) + 1);
   begin
      for I in M.Entries.Iterate loop
         Result (Pos) := +Maps.Key (I);
         Pos          := Pos + 1;
      end loop;

      if M.Has_Others then
         if Ada_Like then
            Result (Result'Last) := +"others";
         else
            Result (Result'Last) := +TOML_Keys.Case_Others;
         end if;
      end if;

      if Exclude_Others or else not M.Has_Others then
         return Result (Result'First .. Result'Last - 1);
      else
         return Result;
      end if;
   end Keys;

   ----------------
   -- Set_Others --
   ----------------

   procedure Set_Others (M : in out Map; E : Elements) is
   begin
      M.Other.Insert (TOML_Keys.Case_Others, E);
   end Set_Others;

   ----------
   -- Size --
   ----------

   function Size (M     : Map;
                  Count : access function (E : Elements) return Natural)
                  return Natural
   is
   begin
      return Total : Natural := 0 do
         for Elem of M.Entries loop
            Total := Total + Count (Elem);
         end loop;
      end return;
   end Size;

end Alire.Expressions.Maps;
