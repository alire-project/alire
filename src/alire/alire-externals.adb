with AAA.Enum_Tools;

with Alire.Crates;
with Alire.Externals.From_Output;
with Alire.Externals.From_System;
with Alire.Externals.Softlinks;
with Alire.Externals.Unindexed;
with Alire.TOML_Keys;
with Alire.TOML_Load;
with Alire.User_Pins.Maps;

with TOML;

package body Alire.Externals is

   ---------------
   -- Available --
   ---------------

   function Available (This : External'Class) return Conditional.Availability
   is (This.Available);

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From   : TOML_Adapters.Key_Queue;
                       Strict : Boolean)
                       return External'Class is

      ---------------
      -- From_TOML --
      ---------------

      function From_TOML (Kind : Kinds) return External'Class is
        (case Kind is
            when Hint           => Unindexed.External'
                                     (External with null record),
            when Softlink       => Softlinks.From_TOML (From),
            when System         => From_System.From_TOML (From),
            when Version_Output => From_Output.From_TOML (From));

      Kind : TOML.TOML_Value;
      OK   : constant Boolean := From.Pop (TOML_Keys.External_Kind, Kind);

      --------------
      -- Validate --
      --------------

      procedure Validate is
         function Is_Valid is new AAA.Enum_Tools.Is_Valid (Kinds);
      begin
         if not OK then
            From.Checked_Error ("missing external kind field");
         end if;

         if Kind.Kind not in TOML.TOML_String then
            From.Checked_Error ("external kind must be a string, but got a "
                                & Kind.Kind'Img);
         elsif not Is_Valid (TOML_Adapters.Adafy (Kind.As_String)) then
            From.Checked_Error ("external kind is invalid: " & Kind.As_String);
         end if;
      end Validate;

      Unused_Deps : Conditional.Dependencies;
      Unused_Pins : User_Pins.Maps.Map;

   begin

      --  Check TOML types

      Validate;

      --  Load specific external part

      return Ext : External'Class :=
        From_TOML (Kinds'Value (TOML_Adapters.Adafy (Kind.As_String)))
      do

         --  Load common external fields

         TOML_Load.Load_Crate_Section
           (Strict  => Strict,
            Section => Crates.External_Private_Section,
            From    => From,
            Props   => Ext.Properties,
            Deps    => Unused_Deps,
            Pins    => Unused_Pins,
            Avail   => Ext.Available);

         Assert (Unused_Deps.Is_Empty,
                 "Unexpected dependencies in external definition");
         Assert (Unused_Pins.Is_Empty,
                 "Unexpected pins in external definition");

         From.Report_Extra_Keys; -- Table must be exhausted at this point
      end return;

   exception
      when Checked_Error =>
         raise;
      when E : others =>
         Log_Exception (E);
         From.Checked_Error
           ("invalid external description (see details with -d)");
   end From_TOML;

   -----------------
   -- On_Platform --
   -----------------

   function On_Platform (This : External'Class;
                         Env  : Properties.Vector) return External'Class is
   begin
      return Ext : External'Class := This do
         Ext.Available  := Ext.Available.Evaluate (Env);
         Ext.Properties := Ext.Properties.Evaluate (Env);
      end return;
   end On_Platform;

end Alire.Externals;
