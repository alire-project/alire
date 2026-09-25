with Ada.Environment_Variables;

with Alire.Errors;
with Alire.Warnings;

package body Alire.Gated_Delivery is

   --------------------------
   -- Environment_Variable --
   --------------------------

   function Environment_Variable (Feature : Gated_Feature) return String
   is (case Feature is
         when Package_Features => "ALIRE_GATE_FEATURES");

   -------------
   -- Enabled --
   -------------

   function Enabled (Feature : Gated_Feature) return Boolean
   is (Ada.Environment_Variables.Exists (Environment_Variable (Feature)));

   -------------
   -- Require --
   -------------

   procedure Require (Feature : Gated_Feature; Context : String) is
   begin
      if not Enabled (Feature) then
         raise Feature_Disabled with Errors.Set
           (Context & " is experimental; set "
            & Environment_Variable (Feature)
            & " to enable this gated feature");
      end if;
   end Require;

   ----------------------
   -- Warn_If_Disabled --
   ----------------------

   procedure Warn_If_Disabled
     (Feature : Gated_Feature;
      Context : String)
   is
   begin
      if not Enabled (Feature) then
         Warnings.Warn_Once
           (Context & "; set " & Environment_Variable (Feature)
            & " to enable this gated feature");
      end if;
   end Warn_If_Disabled;

end Alire.Gated_Delivery;
