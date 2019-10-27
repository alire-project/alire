with Alire.Actions;
with Alire.Conditional;
with Alire.Projects;
with Alire.Properties.Labeled;
with Alire.Properties.Licenses;
with Alire.Properties.Scenarios;
with Alire.TOML_Adapters;

package Alire.Properties.From_TOML with Preelaborate is

   subtype Property_Loader is Conditional.Property_Loader;

   type Property_Keys is (Actions,
                          Authors,
                          Description,
                          Executables,
                          GPR_Externals,
                          GPR_Set_Externals,
                          Licenses,
                          Maintainers,
                          Maintainers_Logins,
                          Notes,
                          Project_Files,
                          Website);
   --  These enum values must match the toml key they represent with '-' => '_'

   type Loader_Array is array (Property_Keys range <>) of Property_Loader;

   --  We use these arrays to determine which properties may appear when
   --  loading a [general] section or a proper release.

   General_Loaders : constant Loader_Array (Property_Keys) :=
                       (Actions  => Alire.Actions.From_TOML'Access,
                        GPR_Externals ..
                        GPR_Set_Externals
                                 => Scenarios.From_TOML'Access,
                        Licenses => Properties.Licenses.From_TOML'Access,
                        others   => Labeled.From_TOML'Access);

   Release_Loaders : constant Loader_Array (Property_Keys) :=
                       (Actions       => Alire.Actions.From_TOML'Access,
                        Executables   => Labeled.From_TOML'Access,
                        GPR_Externals ..
                        GPR_Set_Externals
                                      => Scenarios.From_TOML'Access,
                        Notes         => Labeled.From_TOML'Access,
                        Project_Files => Labeled.From_TOML'Access,
                        others        => null);

   --  The following array determines which properties accept dynamic
   --  expressions, per index semantics. All other properties must be static.

   Loaders_During_Case : constant array (Property_Keys) of Property_Loader
     := (Actions           => Alire.Actions.From_TOML'Access,
         Executables       => Labeled.From_TOML_Executable_Cases'Access,
         GPR_Set_Externals => Scenarios.From_TOML_Cases'Access,
         Project_Files     => Labeled.From_TOML_Project_File_Cases'Access,
         others            => null);

   function Loader (From    : TOML_Adapters.Key_Queue;
                    Loaders : Loader_Array;
                    Section : Projects.Sections)
                    return Conditional.Properties;
   --  Takes a table of mixed properties and dispatches to each concrete
   --  property loader. Takes into account dynamic properties.

   --  Following functions are wrappers on Loader that conform to the signature
   --  expected by the dynamic expression loaders.

   use all type Projects.Sections;

   function General_Loader (From : TOML_Adapters.Key_Queue)
                            return Conditional.Properties is
     (Loader (From, General_Loaders, General_Section));

   function Release_Loader (From : TOML_Adapters.Key_Queue)
                            return Conditional.Properties is
     (Loader (From, Release_Loaders, Release_Section));

   Section_Loaders : constant
        array (Projects.Sections) of access
        function (From : TOML_Adapters.Key_Queue)
        return Conditional.Properties
        := (General_Section => General_Loader'Access,
            Release_Section => Release_Loader'Access);

end Alire.Properties.From_TOML;
