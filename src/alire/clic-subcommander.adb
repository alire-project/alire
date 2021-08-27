with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body CLIC.Subcommander is

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG")
   is
   begin
      GNAT.Command_Line.Define_Switch (Config.GNAT_Cfg,
                                       Switch      => Switch,
                                       Long_Switch => Long_Switch,
                                       Help        => Help,
                                       Section     => Section,
                                       Argument    => Argument);
      Add (Config.Info, Switch, Long_Switch, Help, Argument);
   end Define_Switch;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Output      : access Boolean;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Value       : Boolean := True)
   is
   begin
      GNAT.Command_Line.Define_Switch (Config.GNAT_Cfg,
                                       Output      => Output,
                                       Switch      => Switch,
                                       Long_Switch => Long_Switch,
                                       Help        => Help,
                                       Section     => Section,
                                       Value       => Value);
      Add (Config.Info, Switch, Long_Switch, Help, "");
   end Define_Switch;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Output      : access Integer;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Initial     : Integer := 0;
      Default     : Integer := 1;
      Argument    : String := "ARG")
   is
   begin
      GNAT.Command_Line.Define_Switch (Config.GNAT_Cfg,
                                       Switch      => Switch,
                                       Output      => Output,
                                       Long_Switch => Long_Switch,
                                       Help        => Help,
                                       Section     => Section,
                                       Initial     => Initial,
                                       Default     => Default,
                                       Argument    => Argument);
      Add (Config.Info, Switch, Long_Switch, Help, Argument);
   end Define_Switch;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Output      : access GNAT.Strings.String_Access;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG")
   is
   begin
      GNAT.Command_Line.Define_Switch (Config.GNAT_Cfg,
                                       Output      => Output,
                                       Switch      => Switch,
                                       Long_Switch => Long_Switch,
                                       Help        => Help,
                                       Section     => Section,
                                       Argument    => Argument);
      Add (Config.Info, Switch, Long_Switch, Help, Argument);
   end Define_Switch;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Callback    : not null GNAT.Command_Line.Value_Callback;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG")
   is
   begin
      GNAT.Command_Line.Define_Switch (Config.GNAT_Cfg,
                                       Callback    => Callback,
                                       Switch      => Switch,
                                       Long_Switch => Long_Switch,
                                       Help        => Help,
                                       Section     => Section,
                                       Argument    => Argument);
      Add (Config.Info, Switch, Long_Switch, Help, Argument);
   end Define_Switch;

   ---------
   -- Add --
   ---------

   procedure Add (Vect : in out Switch_Info_Vectors.Vector;
                  Switch, Long_Switch, Help, Argument : String)
   is
   begin
      Vect.Append (Switch_Info'(To_Unbounded_String (Switch),
                                To_Unbounded_String (Long_Switch),
                                To_Unbounded_String (Help),
                                To_Unbounded_String (Argument)));
   end Add;

   --------------------------
   -- Verify_No_Duplicates --
   --------------------------

   function Verify_No_Duplicates (A, B : Switches_Configuration)
                                  return Boolean
   is
      Seen : AAA.Strings.Set;
      --  We track already set switches in this set; any re-appearance is
      --  reported.

      ------------
      -- Insert --
      ------------

      function Insert (Switch : String) return Boolean is
         --  Return True if OK; False otherwise.
      begin
         if Seen.Contains (Switch) then
            return False;
         else
            Seen.Insert (Switch);
            return True;
         end if;
      end Insert;

      ------------
      -- Insert --
      ------------

      function Insert (Switch, Long_Switch : String) return Boolean is
      begin
         --  Short version
         if Switch /= "" and then
           not Insert (Switch)
         then
            return False;
         end if;
         --  Long version
         if Long_Switch /= "" and then
           not Insert (Long_Switch)
         then
            return False;
         end if;

         return True;
      end Insert;
   begin
      for Elt of A.Info loop
         if not Insert (To_String (Elt.Switch), To_String (Elt.Long_Switch))
         then
            return False;
         end if;
      end loop;

      for Elt of B.Info loop
         if not Insert (To_String (Elt.Switch), To_String (Elt.Long_Switch))
         then
            return False;
         end if;
      end loop;

      --  No duplication detected
      return True;
   end Verify_No_Duplicates;
end CLIC.Subcommander;
