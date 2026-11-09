with AAA.Strings;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.OS_Lib;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alire.Errors;
with Alire.Properties.Environment; use Alire.Properties.Environment;
with Alire.OS_Lib;

with Dirty_Booleans;

package body Alire.Environment is

   ---------------------
   -- Already_Defines --
   ---------------------

   function Already_Defines (Existing, Value : String) return Boolean
   --  Check that Value is a path-delimited identical value in Existing
   is (for some Part of AAA.Strings.Vector'
         (AAA.Strings.Split (Existing, GNAT.OS_Lib.Path_Separator)) =>
          Part = Value);

   ---------
   -- Add --
   ---------

   procedure Add (This : in out Context; Name : String; Action : Env_Action) is
   begin
      if not This.Actions.Contains (+Name) then
         declare
            Empty_Vect : Action_Vectors.Vector;
         begin
            This.Actions.Include (+Name, Empty_Vect);
         end;
      end if;

      This.Actions.Reference (+Name).Append (Action);
   end Add;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Context; Name, Value, Origin : String) is
      Action : constant Env_Action := (Set, +Value, +Origin);
   begin
      This.Add (Name, Action);
   end Set;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Context; Name, Value, Origin : String) is
      Action : constant Env_Action := (Append, +Value, +Origin);
   begin
      This.Add (Name, Action);
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (This : in out Context; Name, Value, Origin : String) is
      Action : constant Env_Action := (Prepend, +Value, +Origin);
   begin
      This.Add (Name, Action);
   end Prepend;

   -----------------
   -- Print_Shell --
   -----------------

   procedure Print_Shell (This : Context; Kind : Platforms.Shells) is
   begin
      --  TODO: PowerShell or CMD version for Windows. Is it possible to detect
      --  the kind of shell we are running in?
      for Elt of This.Compile (Check_Conflicts => True) loop
         Trace.Always (Shell_Command (Kind,
                                      Name  => To_String (Elt.Key),
                                      Value => To_String (Elt.Value)));
      end loop;
   end Print_Shell;

   -------------------
   -- Shell_Command --
   -------------------

   function Shell_Command (Kind  : Platforms.Shells;
                           Name  : String;
                           Value : String) return String
   is
      use AAA.Strings;

      Error : constant String := Error_In_Variable_Name (Name);

      ---------
      -- Hex --
      ---------

      Hex_Digits : constant String := "0123456789ABCDEF";

      function Hex (Value : Natural; Width : Natural := 4) return String
      is ((if Value < 16 and then Width <= 1
           then ""
           else Hex (Value / 16, Natural'Max (Width, 1) - 1))
          & Hex_Digits (Value mod 16 + 1));
      --  Uppercase hexadecimal image, with at least Width digits

      ---------------------------
      -- PowerShell_Expression --
      ---------------------------

      function PowerShell_Expression (Text : String) return String is
         --  Single-quoted runs of printable ASCII, in which PowerShell takes
         --  everything literally but single quotes (doubled), joined with
         --  [char] codes for anything else. The result is plain ASCII, so it
         --  means the same whatever encoding PowerShell reads it with (non-
         --  ASCII bytes may otherwise decode to the typographic quotes that
         --  PowerShell also accepts, U+2018 .. U+201B).

         Result : Unbounded_String := +"'";
         In_Run : Boolean := True;
         --  Whether Result ends inside a quoted run. It always begins with
         --  one, so that PowerShell concatenates strings and not chars.

         procedure Add (Point : Natural) is
         begin
            if Point in 16#20# .. 16#7E# then
               if not In_Run then
                  Append (Result, " + '");
                  In_Run := True;
               end if;
               Append (Result, (if Point = Character'Pos (''')
                                then "''"
                                else (1 => Character'Val (Point))));
            else
               Append (Result,
                       (if In_Run then "'" else "") & " + "
                       & (if Point > 16#FFFF#
                          then "[char]::ConvertFromUtf32(0x" & Hex (Point)
                               & ")"
                          else "[char]0x" & Hex (Point)));
               In_Run := False;
            end if;
         end Add;

      begin
         declare
            Points : constant Wide_Wide_String :=
                       Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
                         (Text);
         begin
            for Point of Points loop
               Add (Wide_Wide_Character'Pos (Point));
            end loop;
         end;

         return +Result & (if In_Run then "'" else "");
      exception
         when Ada.Strings.UTF_Encoding.Encoding_Error =>
            --  Not UTF-8 (it may come from the user's environment), so keep
            --  its bytes as they are
            Result := +"'";
            In_Run := True;
            for Char of Text loop
               Add (Character'Pos (Char));
            end loop;
            return +Result & (if In_Run then "'" else "");
      end PowerShell_Expression;

   begin
      if Error /= "" then
         Raise_Checked_Error (Error);
      end if;

      case Kind is
         when Platforms.Unix =>
            --  In single quotes, everything is literal but single quotes,
            --  which are closed, escaped and reopened.
            return "export " & Name & "='" & Replace (Value, "'", "'\''")
              & "'";

         when Platforms.PowerShell =>
            return "$env:" & Name & " = " & PowerShell_Expression (Value);

         when Platforms.WinCmd =>
            --  In set "NAME=VALUE", cmd takes metacharacters (& | < > ^)
            --  literally, but there is no reliable escape for these:
            if (for some C of Value =>
                  C in '"' | '%' | '!' | Latin_1.CR | Latin_1.LF)
            then
               Raise_Checked_Error
                 ("the value of " & Name & " cannot be given literally to "
                  & "cmd.exe, as it contains '""', '%', '!' or a line break; "
                  & "use PowerShell instead");
            end if;
            return "set """ & Name & "=" & Value & """";
      end case;
   end Shell_Command;

   -------------------
   -- Print_Details --
   -------------------

   procedure Print_Details (This : Context) is
   begin
      for C in This.Actions.Iterate loop
         declare
            Key : constant String := To_String (Action_Maps.Key (C));
         begin
            Trace.Always (" - variable: '" & Key & "'");
            for Act of This.Actions (C) loop
               case Act.Kind is
               when Properties.Environment.Set =>
                  Trace.Always ("   - Set to '" & To_String (Act.Value) &
                                  "' by '" & To_String (Act.Origin)  & "'");

               when Properties.Environment.Append =>
                  Trace.Always ("   - Appended with '" &
                                  To_String (Act.Value) &
                                  "' by '" & To_String (Act.Origin)  & "'");

               when Properties.Environment.Prepend =>
                  Trace.Always ("   - Prepended with '" &
                                  To_String (Act.Value) &
                                  "' by '" & To_String (Act.Origin)  & "'");

               end case;
            end loop;
         end;
      end loop;
   end Print_Details;

   -------------
   -- Compile --
   -------------

   function Compile (Key             : Unbounded_String;
                     Vect            : Action_Vectors.Vector;
                     Check_Conflicts : Boolean)
                     return Var
   is
      Separator : constant Character := GNAT.OS_Lib.Path_Separator;

      Value : Unbounded_String := +OS_Lib.Getenv (+Key);
      --  Pre-existing value, and new value when no conflict
   begin

      for Act of Vect loop

         --  Print some helpful details to inspect a conflict
         case Act.Kind is
            when Properties.Environment.Set =>
               Trace.Detail
                 (+("Env: " & Act.Origin &
                    " sets '" & Act.Value & "' to '" & Key & "'"));

            when Properties.Environment.Append =>

               Trace.Detail
                 (+("Env: " & Act.Origin &
                    " appends '" & Act.Value & "' to '" & Key & "'"));

            when Properties.Environment.Prepend =>
               Trace.Detail
                 (+("Env: " & Act.Origin &
                    " prepends '" & Act.Value & "' to '" & Key & "'"));
         end case;

         if Length (Value) = 0 then
            Value := Act.Value;
         else
            case Act.Kind is

            when Properties.Environment.Set =>
               if Value = Act.Value then
                  Trace.Debug ("Skipping identical key value: "
                               & (+Key) & "=" & (+Value));
                  --  We can silently ignore the attempt to set to the same
                  --  value. This is not ideal, but is more flexible for the
                  --  cases where we may end exporting the same environment
                  --  twice. Long-term, something like Boost.Process would be
                  --  more robust to call subprocesses without pilfering our
                  --  own environment.
               elsif Check_Conflicts then
                  Raise_Checked_Error
                    (Errors.Wrap
                       ("Trying to set an already defined environment "
                        & "variable",
                        (+Key) & " is already defined as '" & (+Value)
                        & "' but new value is '" & (+Act.Value) & "'"));
               end if;

            when Properties.Environment.Append =>
               if Already_Defines (+Value, +Act.Value) then
                  Trace.Debug ("Skipping identical key value: "
                               & (+Key) & "=" & (+Value));
               else
                  Value := Value & Separator & Act.Value;
               end if;

            when Properties.Environment.Prepend =>
               if Already_Defines (+Value, +Act.Value) then
                  Trace.Debug ("Skipping identical key value: "
                               & (+Key) & "=" & (+Value));
               else
                  Value := Act.Value & Separator & Value;
               end if;
            end case;
         end if;
      end loop;

      return (Key => Key, Value => Value);
   end Compile;

   -------------
   -- Compile --
   -------------

   function Compile (This            : Context;
                     Check_Conflicts : Boolean)
                     return Var_Array is
      Result : Var_Array (1 .. Natural (This.Actions.Length));
      Index  : Natural := Result'First;
   begin
      for C in This.Actions.Iterate loop
         Result (Index) := Compile (Action_Maps.Key (C),
                                    This.Actions (C),
                                    Check_Conflicts);
         Index := Index + 1;
      end loop;

      --  Sort results for predictable output
      Sort (Result);

      return Result;
   end Compile;

   ------------
   -- Export --
   ------------

   procedure Export (This : Context) is
   begin
      for Var of This.Compile (Check_Conflicts => True) loop
         OS_Lib.Setenv (+Var.Key, +Var.Value);
      end loop;
   end Export;

   -------------
   -- Get_All --
   -------------

   function Get_All (This            : Context;
                     Check_Conflicts : Boolean := False)
                     return Env_Map is
   begin
      return Result : Env_Map do
         for Var of This.Compile (Check_Conflicts) loop
            Result.Insert (+Var.Key, +Var.Value);
         end loop;
      end return;
   end Get_All;

   -----------------------
   -- Traceback_Enabled --
   -----------------------

   function Traceback_Enabled return Boolean
   is
      package Dirty is new Dirty_Booleans;
      use type Dirty.Boolean;
   begin
      return Dirty.Value (OS_Lib.Getenv (Traceback, "false")) = True;
   end Traceback_Enabled;

end Alire.Environment;
