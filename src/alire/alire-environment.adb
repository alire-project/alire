with GNAT.OS_Lib;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alire.Properties.Environment; use Alire.Properties.Environment;
with Alire.Properties.Scenarios;
with Alire.OS_Lib;
with Alire.GPR;
with Alire.Utils;
with Alire.Environment.Formatting;

package body Alire.Environment is

   function "+" (Str : String) return Unbounded_String
                 renames To_Unbounded_String;

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

   ----------
   -- Load --
   ----------

   procedure Load (This            : in out Context;
                   Rel             : Alire.Releases.Release;
                   Prop            : Alire.Properties.Vector;
                   Is_Root_Release : Boolean)
   is
      Origin : constant String := Rel.Name_Str;
   begin

      --  Enviromemnt variables defined in the crate manifest
      for Act of Rel.Environment (Prop) loop
         begin
            declare
               Value : constant String :=
                 Formatting.Format (Rel, Act.Value, Is_Root_Release);
            begin
               case Act.Action is

               when Properties.Environment.Set =>

                  This.Set (Act.Name, Value, Origin & " (env)");

               when Properties.Environment.Append =>

                  This.Append (Act.Name, Value, Origin & " (env)");

               when Properties.Environment.Prepend =>

                  This.Prepend (Act.Name, Value, Origin & " (env)");

               end case;
            end;
         exception
            when Formatting.Unknown_Formatting_Key =>
               Raise_Checked_Error
                 ("Unknown environment variable formatting key in var '" &
                    Act.Name & " of '" & Origin & "'");
         end;
      end loop;

      --  Environment variables for GPR external scenario variables
      for Property of Rel.On_Platform_Properties (Prop) loop
         if Property in Alire.Properties.Scenarios.Property'Class then
            declare
               use all type Alire.GPR.Variable_Kinds;
               Variable : constant Alire.GPR.Variable :=
                 Alire.Properties.Scenarios.Property (Property).Value;
            begin
               if Variable.Kind = External then
                  This.Set (Variable.Name, Variable.External_Value,
                           Origin & " (gpr ext)");
               end if;
            end;
         end if;
      end loop;
   end Load;

   -----------------
   -- Print_Shell --
   -----------------

   procedure Print_Shell (This : Context; Kind : Platforms.Shells) is
   begin
      --  TODO: PowerShell or CMD version for Windows. Is it possible to detect
      --  the kind of shell we are runnning in?
      for Elt of This.Compile loop
         case Kind is
         when Platforms.Unix =>
            Trace.Always (To_String ("export " & Elt.Key & "=""" &
                            Elt.Value & """"));
         when Platforms.PowerShell =>
            Trace.Always (To_String ("$env:" & Elt.Key & " = """ &
                            Elt.Value & """"));
         when Platforms.WinCmd =>
            Trace.Always (To_String ("set " & Elt.Key & "=" & Elt.Value));
         end case;
      end loop;
   end Print_Shell;

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

   function Compile (Key  : Unbounded_String;
                     Vect : Action_Vectors.Vector)
                     return Var
   is
      Existing : constant String := OS_Lib.Getenv (+Key);

      Separator : constant Character := GNAT.OS_Lib.Path_Separator;

      Value : Unbounded_String := +Existing;
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
               Raise_Checked_Error
                 ("Trying to set an alredy defined environment variable");

            when Properties.Environment.Append =>
               Value := Value & Separator & Act.Value;

            when Properties.Environment.Prepend =>
               Value := Act.Value & Separator & Value;
            end case;
         end if;
      end loop;

      return (Key => Key, Value => Value);
   end Compile;

   -------------
   -- Compile --
   -------------

   function Compile (This : Context) return Var_Array is
      Result : Var_Array (1 .. Natural (This.Actions.Length));
      Index  : Natural := Result'First;
   begin
      for C in This.Actions.Iterate loop
         Result (Index) := Compile (Action_Maps.Key (C), This.Actions (C));
         Index := Index + 1;
      end loop;

      return Result;
   end Compile;

   ------------
   -- Export --
   ------------

   procedure Export (This : Context) is
   begin
      for Var of This.Compile loop
         OS_Lib.Setenv (+Var.Key, +Var.Value);
      end loop;
   end Export;

end Alire.Environment;
