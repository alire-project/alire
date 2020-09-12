with Alire.Conditional;
with Alire.Crates;
with Alire.Properties.Actions;
with Alire.Properties.Environment;
with Alire.Properties.Labeled;
with Alire.Properties.Licenses;
with Alire.Properties.Scenarios;
with Alire.Properties.Bool;
with Alire.TOML_Adapters;

package Alire.Properties.From_TOML is

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
                          Name,
                          Notes,
                          Project_Files,
                          Tags,
                          Version,
                          Website);
   --  These enum values must match the toml key they represent with '-' => '_'

   --  The following array describes which properties are mandatory, depending
   --  on what we are loading.

   Mandatory : array (Crates.Sections, Property_Keys) of Boolean :=

                 (Crates.External_Private_Section =>
                    (others => False),

                  Crates.External_Shared_Section  =>
                    (Description        |
                     Maintainers        |
                     Maintainers_Logins |
                     Name   => True,
                     others => False),

                  Crates.Index_Release           =>
                    (Description        |
                     Maintainers        |
                     Maintainers_Logins |
                     Name               |
                     Version => True,
                     others  => False),

                  Crates.Local_Release           =>
                    (Description        |
                     Name               |
                     Version => True,
                     others  => False)
                 );

   Recommended : array (Property_Keys) of Boolean :=
                   (Authors  |
                    Licenses |
                    Tags     |
                    Website => True,
                    others  => False);

   type Loader_Array is array (Property_Keys range <>) of Property_Loader;

   --  We use the following arrays to determine which properties may appear
   --  in a manifest section. These loaders will always receive a table of the
   --  form prop_name = prop_value. Dynamic expressions are resolved prior to
   --  dispatching to these loaders, so they need not to care.

   External_Private_Loaders : constant Loader_Array (Property_Keys) :=
                                (Hint   => Labeled.From_TOML'Access,
                                 others => null);
   --  This loader is used for properties common to all external classes, found
   --  in their private [[external]] sections

   External_Shared_Loaders : constant Loader_Array (Property_Keys) :=
                               (Authors            |
                                Description => Labeled.From_TOML'Access,
                                Licenses    =>
                                  Properties.Licenses.From_TOML'Access,
                                Long_Description   |
                                Maintainers        |
                                Maintainers_Logins |
                                Name               |
                                Tags               |
                                Website => Labeled.From_TOML'Access,
                                others  => null);
   --  This loader is used for properties a manifest containing externals may
   --  provide, shared by all external definitions found therein

   Release_Loaders : constant Loader_Array (Property_Keys) :=
                       (Actions       => Properties.Actions.From_TOML'Access,
                        Authors       => Labeled.From_TOML'Access,
                        Auto_GPR_With => Bool.From_TOML'Access,
                        Description   => Labeled.From_TOML'Access,
                        Environment   =>
                          Properties.Environment.From_TOML'Access,
                        Executables   => Labeled.From_TOML'Access,
                        GPR_Externals |
                        GPR_Set_Externals
                                      => Scenarios.From_TOML'Access,
                        Hint          => null,
                        Licenses      => Properties.Licenses.From_TOML'Access,
                        Long_Description   |
                        Maintainers        |
                        Maintainers_Logins |
                        Name               |
                        Notes              |
                        Project_Files      |
                        Tags               |
                        Version            |
                        Website       => Labeled.From_TOML'Access);
   --  This loader applies to a normal release manifest

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

   function External_Private_Loader (From : TOML_Adapters.Key_Queue)
                                     return Conditional.Properties is
     (Loader (From, External_Private_Loaders, External_Private_Section));

   function External_Shared_Loader (From : TOML_Adapters.Key_Queue)
                            return Conditional.Properties is
     (Loader (From, External_Shared_Loaders, External_Shared_Section));

   function Index_Release_Loader (From : TOML_Adapters.Key_Queue)
                                  return Conditional.Properties is
     (Loader (From, Release_Loaders, Index_Release));

   function Local_Release_Loader (From : TOML_Adapters.Key_Queue)
                                  return Conditional.Properties is
     (Loader (From, Release_Loaders, Local_Release));

   Section_Loaders : constant
     array (Crates.Sections) of access
     function (From : TOML_Adapters.Key_Queue)
     return Conditional.Properties
     := (External_Private_Section => External_Private_Loader'Access,
         External_Shared_Section  => External_Shared_Loader'Access,
         Index_Release            => Index_Release_Loader'Access,
         Local_Release            => Local_Release_Loader'Access);

end Alire.Properties.From_TOML;
