with AAA.Strings;

package Alire.Settings.Version_Output is

   --  Values of the externals.version_output.policy setting, which says which
   --  version commands of externals detected from their output are run. Kept
   --  apart from Alire.Externals.From_Output, which depends on the settings.

   type Policies is (Run_All, Run_Allowlist, Run_None);

   Prefix : constant String := "Run_";
   --  The setting text is the literal without this prefix, in lower case, as
   --  "all" cannot be a literal in Ada.

   function Image (Policy : Policies) return String
   is (AAA.Strings.To_Lower_Case (AAA.Strings.Tail (Policy'Image, '_')));
   --  The setting text for each value

end Alire.Settings.Version_Output;
