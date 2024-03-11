with Semantic_Versioning;

package Alire.Version.Semver is

   --  Convenience to be able to use the current version directly as a
   --  comparable proper semantic version version.

   package Semver renames Semantic_Versioning;

   subtype Version is Semver.Version;

   Current : constant Version := Semver.New_Version (Alire.Version.Current);

end Alire.Version.Semver;
