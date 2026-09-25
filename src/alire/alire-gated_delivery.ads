package Alire.Gated_Delivery with Preelaborate is

   --  Registry for features being delivered behind temporary gates. Callers
   --  name the feature, while this package privately owns how each gate is
   --  configured so the delivery mechanism does not leak into feature code.

   type Gated_Feature is
     (Package_Features);

   Feature_Disabled : exception;
   --  A user-facing denial that must not be mistaken for malformed data.

   function Enabled (Feature : Gated_Feature) return Boolean;
   --  True when the delivery gate for Feature is enabled.

   procedure Require (Feature : Gated_Feature; Context : String);
   --  Raise Feature_Disabled with an actionable message unless Feature is
   --  enabled.

   procedure Warn_If_Disabled (Feature : Gated_Feature; Context : String);
   --  Warn once that Context is being limited by a disabled gate, including
   --  the private configuration needed to enable it.

end Alire.Gated_Delivery;
