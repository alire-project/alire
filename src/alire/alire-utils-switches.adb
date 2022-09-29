with Ada.Strings.Unbounded;

with Alire.Utils.GNAT_Switches; use Alire.Utils.GNAT_Switches;

package body Alire.Utils.Switches is

   ------------
   -- Append --
   ------------

   function Append (L : Switch_List;
                    S : Switch) return Switch_List
   is
   begin
      return R : Switch_List := L do
         R.Append (S);
      end return;
   end Append;

   ------------
   -- Append --
   ------------

   function Append (L : Switch_List;
                    S : not null Switch_Access) return Switch_List
   is
   begin
      return L.Append (S.all);
   end Append;

   ------------
   -- Append --
   ------------

   function Append (A, B : Switch_List) return Switch_List is
   begin
      return R : Switch_List := A.Copy do
         for Elt of B loop
            R.Append (Elt);
         end loop;
      end return;
   end Append;

   -------------
   -- Flatten --
   -------------

   function Flatten (L         : Switch_List;
                     Separator : String := " ")
                     return String
   is
      use Ada.Strings.Unbounded;
      First : Boolean := True;
      Result : Unbounded_String;
   begin
      for Elt of L loop
         if First then
            Append (Result, Elt);
            First := False;
         else
            Append (Result, Separator & Elt);
         end if;
      end loop;
      return To_String (Result);
   end Flatten;

   --------------
   -- Get_List --
   --------------

   function Get_List (S : Optimization_Switches) return Switch_List
   is (case S.Kind is
          when Performance => Empty_List
                              .Append (GNAT_Optimize_Performance)
                              .Append (GNAT_Enable_Inlining)
                              .Append (GNAT_Function_Sections)
                              .Append (GNAT_Data_Sections),
          when Size        => Empty_List
                              .Append (GNAT_Optimize_Size)
                              .Append (GNAT_Enable_Inlining)
                              .Append (GNAT_Function_Sections)
                              .Append (GNAT_Data_Sections),
          when Debug       => Empty_List
                              .Append (GNAT_Optimize_Debug)
                              .Append (GNAT_Function_Sections)
                              .Append (GNAT_Data_Sections),
          when Custom      => S.List);

   --------------
   -- Get_List --
   --------------

   function Get_List (S : Debug_Info_Switches) return Switch_List
   is (case S.Kind is
          when No     => Empty_List,
          when Yes    => Empty_List.Append (GNAT_Debug_Info),
          when Custom => S.List);

   --------------
   -- Get_List --
   --------------

   function Get_List (S : Contracts_Switches) return Switch_List
   is (case S.Kind is
          when No         => Empty_List,
          when Yes        => Empty_List.Append (GNAT_Asserts_And_Contracts),
          when Custom     => S.List);

   --------------
   -- Get_List --
   --------------

   function Get_List (S : Runtime_Checks_Switches) return Switch_List
   is (case S.Kind is
          when None       => Empty_List.Append (GNAT_Suppress_Runtime_Check),
          when Default    => Empty_List,
          when Overflow   => Empty_List
                             .Append (GNAT_Suppress_Runtime_Check)
                             .Append (GNAT_Enable_Overflow_Check),
          when Everything => Empty_List.Append (GNAT_Enable_Overflow_Check),
          when Custom     => S.List);

   --------------
   -- Get_List --
   --------------

   function Get_List (S : Compile_Checks_Switches) return Switch_List
   is (case S.Kind is
          when None     => Empty_List,
          when Warnings => Empty_List
                           .Append (GNAT_All_Warnings)
                           .Append (GNAT_Disable_Warn_No_Exception_Propagation)
                           .Append (GNAT_All_Validity_Checks),
          when Errors   => Empty_List
                           .Append (GNAT_All_Warnings)
                           .Append (GNAT_Disable_Warn_No_Exception_Propagation)
                           .Append (GNAT_All_Validity_Checks)
                           .Append (GNAT_Warnings_As_Errors),
          when Custom   => S.List);

   --------------
   -- Get_List --
   --------------

   function Get_List (S : Style_Checks_Switches) return Switch_List
   is (case S.Kind is
          when No     => Empty_List,
          when Yes    => Empty_List
                         .Append ("-gnaty3")
                         .Append ("-gnatya")
                         .Append ("-gnatyA")
                         .Append ("-gnatyB")
                         .Append ("-gnatyb")
                         .Append ("-gnatyc")
                         .Append ("-gnaty-d")
                         --  -gnatyD is not available in GNAT 9
                         --  .Append ("-gnatyD")
                         .Append ("-gnatye")
                         .Append ("-gnatyf")
                         .Append ("-gnatyh")
                         .Append ("-gnatyi")
                         .Append ("-gnatyI")
                         .Append ("-gnatyk")
                         .Append ("-gnatyl")
                         .Append ("-gnatym")
                         .Append ("-gnatyn")
                         .Append ("-gnatyO")
                         .Append ("-gnatyp")
                         .Append ("-gnatyr")
                         .Append ("-gnatyS")
                         .Append ("-gnatyt")
                         .Append ("-gnatyu")
                         .Append ("-gnatyx"),
          when Custom => S.List);

   --------------
   -- Get_List --
   --------------

   function Get_List (S : Ada_Version_Switches) return Switch_List
   is (case S.Kind is
          when Compiler_Default => Empty_List,
          when Ada83            => Empty_List.Append (GNAT_Ada83),
          when Ada95            => Empty_List.Append (GNAT_Ada95),
          when Ada05            => Empty_List.Append (GNAT_Ada05),
          when Ada12            => Empty_List.Append (GNAT_Ada12),
          when Ada2022          => Empty_List.Append (GNAT_Ada2022),
          when GNAT_Extensions  => Empty_List.Append (GNAT_Ada_Extensions),
          when Custom           => S.List);

   --------------
   -- Get_List --
   --------------

   function Get_List (C : Switches_Configuration) return Switch_List is
   begin
      return Empty_List
             --  Unconditional switches
             .Append (GNAT_UTF8_Encoding) --

             .Append (Get_List (C.Optimization))
             .Append (Get_List (C.Debug_Info))
             .Append (Get_List (C.Runtime_Checks))
             .Append (Get_List (C.Compile_Checks))
             .Append (Get_List (C.Contracts))
             .Append (Get_List (C.Style_Checks))
             .Append (Get_List (C.Ada_Version));
   end Get_List;

end Alire.Utils.Switches;
