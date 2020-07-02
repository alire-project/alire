with Alire.Conditional;
with Alire.Crates;
with Alire.Properties.Actions;
with Alire.Properties.Environment;
with Alire.Properties.Labeled;
with Alire.Properties.Licenses;
with Alire.Properties.Scenarios;
with Alire.Properties.Bool;
with Alire.TOML_Adapters;

package Alire.Properties.From_TOML with Preelaborate is

   subtype Property_Loader is Conditional.Property_Loader;

   type Property_Keys is (Actions,
                          Authors,
                          Auto_GPR_With,
                          Description,
                          Environment,
                          Executables,
                          GPR_Externals,
                          GPR_Set_Externals,
                          Hint,
                          Licenses,
                          Long_Description,
                          Maintainers,
                          Maintainers_Logins,
                          Notes,
                          Project_Files,
                          Website,
                          Tags);
   --  These enum values must match the toml key they represent with '-' => '_'

   type Loader_Array is array (Property_Keys range <>) of Property_Loader;

   --  We use these arrays to determine which properties may appear in each
   --  crate section. These loaders will always receive a table of the form
   --  prop_name = prop_value. Dynamic expressions are resolved prior to
   --  dispatching to these loaders, so they need not to care.

   External_Loaders : constant Loader_Array (Property_Keys) :=
                        (Hint   => Labeled.From_TOML'Access,
                         others => null);
   --  This loader is used for properties common to all external classes

   General_Loaders : constant Loader_Array (Property_Keys) :=
                       (Actions     => Properties.Actions.From_TOML'Access,
                        Environment => Properties.Environment.From_TOML'Access,
                        GPR_Externals ..
                        GPR_Set_Externals
                                    => Scenarios.From_TOML'Access,
                        Hint        => null, -- Only apply to externals
                        Licenses    => Properties.Licenses.From_TOML'Access,
                        Auto_GPR_With => Bool.From_TOML'Access,
                        others      => Labeled.From_TOML'Access);
   --  This loader is used in the [general] crate section

   Release_Loaders : constant Loader_Array (Property_Keys) :=
                       (Actions       => Properties.Actions.From_TOML'Access,
                        Environment   =>
                          Properties.Environment.From_TOML'Access,
                        Executables   => Labeled.From_TOML'Access,
                        GPR_Externals ..
                        GPR_Set_Externals
                                      => Scenarios.From_TOML'Access,
                        Notes         => Labeled.From_TOML'Access,
                        Project_Files => Labeled.From_TOML'Access,
                        Auto_GPR_With => Bool.From_TOML'Access,
                        others        => null);
   --  This loader applies to release sections

   --  The following array determines which properties accept dynamic
   --  expressions, per index semantics. All other properties must be static.

   Loaders_During_Case : constant array (Property_Keys) of Property_Loader
     := (Actions           => Properties.Actions.From_TOML'Access,
         Environment       => Properties.Environment.From_TOML'Access,
         Executables       => Labeled.From_TOML_Executable_Cases'Access,
         GPR_Set_Externals => Scenarios.From_TOML_Cases'Access,
         Hint              => Labeled.From_TOML_Hint_Cases'Access,
         Project_Files     => Labeled.From_TOML_Project_File_Cases'Access,
         others            => null);

   function Loader (From    : TOML_Adapters.Key_Queue;
                    Loaders : Loader_Array;
                    Section : Crates.Sections)
                    return Conditional.Properties;
   --  Takes a table of mixed properties and dispatches to each concrete
   --  property loader. Takes into account dynamic properties.

   --  Following functions are wrappers on Loader that conform to the signature
   --  expected by the dynamic expression loaders.

   use all type Crates.Sections;

   function External_Loader (From : TOML_Adapters.Key_Queue)
                             return Conditional.Properties is
     (Loader (From, External_Loaders, External_Section));

   function General_Loader (From : TOML_Adapters.Key_Queue)
                            return Conditional.Properties is
     (Loader (From, General_Loaders, General_Section));

   function Release_Loader (From : TOML_Adapters.Key_Queue)
                            return Conditional.Properties is
     (Loader (From, Release_Loaders, Release_Section));

   Section_Loaders : constant
     array (Crates.Sections) of access
     function (From : TOML_Adapters.Key_Queue)
     return Conditional.Properties
     := (External_Section => External_Loader'Access,
         General_Section  => General_Loader'Access,
         Release_Section  => Release_Loader'Access);

end Alire.Properties.From_TOML;
