with Alire.Utils.GNAT_Switches; use Alire.Utils.GNAT_Switches;

package body Alire.Utils.Switches.Knowledge is

   Builtin_Done : Boolean := False;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (Sw : Switch) return String
   is
   begin
      if DB.Contains (Sw) then
         return DB.Element (Sw);
      else
         return "";
      end if;
   end Get_Info;

   --------------
   -- Register --
   --------------

   procedure Register (Sw   : Switch;
                       Info : String)
   is
   begin
      DB.Insert (Sw, Info);
   end Register;

   --------------
   -- Populate --
   --------------

   procedure Populate is
   begin
      if Builtin_Done then
         return;
      else
         Builtin_Done := True;
      end if;

      --  Register GNAT switches in the Switches knowledge database

      pragma Style_Checks ("M200");

      Register (GNAT_Optimize_Performance, "Optimize for performance");
      Register (GNAT_Optimize_Debug, "Optimize for debug");
      Register (GNAT_Optimize_Size, "Optimize for code size");
      Register (GNAT_Enable_Inlining, "Enable inlining");
      Register (GNAT_Asserts_And_Contracts, "Enable assertions and contracts");
      Register (GNAT_Debug_Info, "Generate debug info");
      Register (GNAT_Suppress_Runtime_Check, "Suppress run-time checks");
      Register (GNAT_Enable_Overflow_Check, "Enable numeric overflow checking");
      Register (GNAT_Disable_Warn_No_Exception_Propagation, "Disable warnings for No_Exception_Propagation");
      Register (GNAT_Dont_Quit, "Don't quit. Generate ALI and tree files even if illegalities");
      Register (GNAT_All_Warnings, "Enable all warnings");
      Register (GNAT_All_Validity_Checks, "All validity checks");
      Register (GNAT_Warnings_As_Errors, "Warnings as errors");
      Register (GNAT_Function_Sections, "Separate ELF section for each function");
      Register (GNAT_Data_Sections, "Separate ELF section for each variable");
      Register (GNAT_Extra_Exception_Info, "Extra information in exception messages");

      Register (GNAT_Ada83, "Ada 83 Compatibility Mode");
      Register (GNAT_Ada95, "Ada 95 Mode");
      Register (GNAT_Ada05, "Ada 2005 Mode");
      Register (GNAT_Ada12, "Ada 2012 Mode");
      Register (GNAT_Ada2022, "Ada 2022 Mode");
      Register (GNAT_Ada_Extensions, "Enable GNAT Extensions");

      Register (GNAT_UTF8_Encoding, "UTF-8 encoding for wide characters");

      Register ("-gnaty3", "Specify indentation level of 3");
      Register ("-gnatya", "Check attribute casing");
      Register ("-gnatyA", "Use of array index numbers in array attributes");
      Register ("-gnatyB", "Check Boolean operators");
      Register ("-gnatyb", "Blanks not allowed at statement end");
      Register ("-gnatyc", "Check comments");
      Register ("-gnaty-d", "Disable check no DOS line terminators present");
      Register ("-gnatyD", "Check declared identifiers in mixed case");
      Register ("-gnatye", "Check end/exit labels");
      Register ("-gnatyf", "No form feeds or vertical tabs");
      Register ("-gnatyh", "No horizontal tabs");
      Register ("-gnatyi", "Check if-then layout");
      Register ("-gnatyI", "check mode IN keywords");
      Register ("-gnatyk", "Check keyword casing");
      Register ("-gnatyl", "Check layout");
      Register ("-gnatym", "Check maximum line length");
      Register ("-gnatyn", "Check casing of entities in Standard");
      Register ("-gnatyO", "Check that overriding subprograms are explicitly marked as such");
      Register ("-gnatyp", "Check pragma casing");
      Register ("-gnatyr", "Check identifier references casing");
      Register ("-gnatyS", "Check no statements after THEN/ELSE");
      Register ("-gnatyt", "Check token spacing");
      Register ("-gnatyu", "Check unnecessary blank lines");
      Register ("-gnatyx", "Check extra parentheses");

   end Populate;

end Alire.Utils.Switches.Knowledge;
