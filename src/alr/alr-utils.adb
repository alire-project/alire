with Ada.Text_IO;

package body Alr.Utils is

   Indicator : constant String := ".oOo";

   -------------------
   -- Busy_Activity --
   -------------------

   function Busy_Activity (Activity : String) return Busy_Prompt is
   begin
      return Busy : Busy_Prompt (Activity'Length) do
         Busy.Activity := Activity;
         Busy.Step;
      end return;
   end Busy_Activity;

   --------------
   -- Contains --
   --------------

   function Contains (V : String_Vector; Subst : String) return Boolean is
   begin
      for Str of V loop
         if Contains (Str, Subst) then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Busy_Prompt) is
   begin
      if Trace.Level = Info then
         Ada.Text_IO.Put
           (ASCII.CR & (1 .. This.Activity'Length + 1 => ' ') & ASCII.CR);
         Ada.Text_IO.Flush;
      end if;
   exception
      when others =>
         null;
   end Finalize;

   ----------
   -- Step --
   ----------

   procedure Step (This : in out Busy_Prompt) is
      use Ada.Calendar;
   begin
      if Trace.Level = Info and then Clock - This.Last >= 0.1 then
         Ada.Text_IO.Put
           (ASCII.CR & This.Activity & " " & Indicator (This.Pos));
         Ada.Text_IO.Flush;

         This.Last := Clock;
         This.Pos  := This.Pos + 1;
         if This.Pos > Indicator'Last then
            This.Pos := Indicator'First;
         end if;
      end if;
   end Step;

end Alr.Utils;
