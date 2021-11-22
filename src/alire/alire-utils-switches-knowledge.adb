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

      Register (GNAT_Optimize_Performance, "Optimize for performance");
      Register (GNAT_Optimize_Debug, "Optimize for debug");
      Register (GNAT_Optimize_Size, "Optimize for code size");
      Register (GNAT_Enable_Inlining, "Enable inlining");
      Register (GNAT_Asserts_And_Contracts, "Enable assertions and contracts");
      Register (GNAT_Debug_Info, "Generate debug info");
      Register (GNAT_Supress_Runtime_Check, "Supress run-time checks");
      Register (GNAT_Enable_Overflow_Check,
                "Enable numeric overflow checking");
      Register (GNAT_Disable_Warn_No_Exception_Propagation,
                "Disable warnings for No_Exception_Propagation");
      Register (GNAT_Dont_Quit,
                "Don't quit. Generate ALI and tree files" &
                  " even if illegalities");
      Register (GNAT_All_Warnings, "Enable all warnings");
      Register (GNAT_All_Validity_Checks, "All validity checks");
      Register (GNAT_Warnings_As_Errors, "Warnings as errors");

      Trace.Always ("GNAT switches registered");

   end Populate;

end Alire.Utils.Switches.Knowledge;
