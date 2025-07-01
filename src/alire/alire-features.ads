with Semantic_Versioning;

package Alire.Features is

   --  For easier lockstep updates, we keep track of features that we will
   --  enable in future index versions.

   subtype Min_Version is Semantic_Versioning.Version;
   subtype On_Version is Min_Version;

   use type Min_Version;

   Config_Deprecated : constant On_Version := +"3.0";
   --  We migrate ALR_CONFIG to ALIRE_SETTINGS_DIR, but allow the use of the
   --  former with a warning during our next major release to ease transition.
   --  Likewise for the -c/--config switch

   Self_Update_Cmd : constant On_Version := +"3.0.0-dev";
   --  Used to warn when users downgrade alr using the `self-update` command

   package Index is

      --  Features referring to the index version

      Explicit_Binary_Origin : constant Min_Version := +"1.3.0";
      --  Require that binary origins are explicitly marked as such instead of
      --  relying on dynamic expressions.

   end Index;

end Alire.Features;
