with Alire.Conditional;
with Alire.TOML_Adapters;
private with Alire.TOML_Keys;
private with Alire.Utils.YAML;

package Alire.Properties.Labeled with Preelaborate is

   --  Properties that have a string atomic/array value and a name.
   --  Note that string arrays are internally stored as several individual
   --  properties with the same label at present.

   type Labels is
     (Author,
      --  VIP

      Description,
      --  Free-form but short description

      Executable,
      --  A resulting executable built by the release

      Hint,
      --  A text to display to the user for externals that fail detection

      Long_Description,
      --  Unlimited-length crate description

      Maintainer,
      --  Info about the maintainer of the alr-packaged crate

      Maintainers_Logins,
      --  E-mails used by the maintainers of a crate to log in to GitHub

      Name,
      --  Crate name

      Notes,
      --  Specific information about a release

      Path,
      --  Extra path for PATH to add to build (prepended)

      Project_File,
      --  Buildable project files in the release, with full relative path

      Version,
      --  Semantic version of the release

      Website,
      --  A website other than the repository

      Tag
      --  One word that indentify one of the topic convered by a crate
     );

   type Cardinalities is (Unique, Multiple); -- Are they atoms or arrays?
   --  This information is used during loading to enforce index correctness,
   --  and during exporting to ensure the proper type (atom/array) is created.

   Cardinality : array (Labels) of Cardinalities :=
                   (Author             => Multiple,
                    Description        => Unique,
                    Executable         => Multiple,
                    Hint               => Unique,
                    Long_Description   => Unique,
                    Maintainer         => Multiple,
                    Maintainers_Logins => Multiple,
                    Name               => Unique,
                    Notes              => Unique,
                    Path               => Multiple,
                    Project_File       => Multiple,
                    Version            => Unique,
                    Website            => Unique,
                    Tag                => Multiple);

   type Label (<>) is new
     Properties.Property and
     Interfaces.Tomifiable
   with private;

   function New_Label (Name  : Labels;
                       Value : String) return Label;

   function Name (L : Label) return Labels;

   function Value (L : Label) return String;

   function Filter (LV : Vector; Name : Labels) return Vector;
   --  Return only Label'Class with matching name

   function Filter (PV   : Conditional.Properties;
                    Name : Labels) return Vector;
   --  Version that takes a conditional tree, enumerates it and filters it.

   overriding
   function Image (L : Label) return String;

   overriding
   function To_YAML (L : Label) return String;

   function Key (L : Labels) return String;

   overriding function Key (L : Label) return String;

   overriding function To_TOML (L : Label) return TOML.TOML_Value with
     Post => To_TOML'Result.Kind = TOML.TOML_String;
   --  Returns only the value, not the name (TOML key)

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;
   --  Loads any labeled property. May raise Checked_Error.

   generic
      Name : Labels;
   function Cond_New_Label (Value : String) return Conditional.Properties;

   generic
      Name : Labels;
   function Cond_New_Path_Label (Value : Any_Path)
                                 return Conditional.Properties;

private

   type Label (Length : Natural) is new
     Properties.Property and
     Interfaces.Tomifiable with
   record
      Name  : Labels;
      Value : String (1 .. Length);
   end record;

   not overriding
   procedure Validate (L    : Label;
                       From : TOML_Adapters.Key_Queue);
   --  Check a just loaded label for validity (descr. length, email format...).
   --  If invalid, raise Checked_Error with appropriate context and message.

   function New_Label (Name : Labels; Value : String) return Label is
     (Properties.Property with Value'Length, Name, Value);

   function Name (L : Label) return Labels is (L.Name);

   function Value (L : Label) return String is (L.Value);

   function Cond_New_Label (Value : String) return Conditional.Properties is
     (Conditional.For_Properties.New_Value (New_Label (Name, Value)));

   function Cond_New_Path_Label (Value : Any_Path)
                                 return Conditional.Properties
   is (Conditional.For_Properties.New_Value
       (New_Label (Name, Utils.To_Native (Value))));

   overriding
   function Image (L : Label) return String
   is (Utils.To_Mixed_Case (L.Name'Img) & ": " & L.Value);

   overriding
   function To_YAML (L : Label) return String
   is (Utils.YAML.YAML_Stringify (L.Value));

   function Key (L : Labels) return String
   is (case L is
          when Author             => TOML_Keys.Author,
          when Description        => TOML_Keys.Description,
          when Executable         => TOML_Keys.Executable,
          when Hint               => TOML_Keys.Hint,
          when Long_Description   => TOML_Keys.Long_Descr,
          when Maintainer         => TOML_Keys.Maintainer,
          when Maintainers_Logins => TOML_Keys.Maint_Logins,
          when Name               => TOML_Keys.Name,
          when Notes              => TOML_Keys.Notes,
          when Path               => TOML_Keys.Path,
          when Project_File       => TOML_Keys.Project_File,
          when Tag                => TOML_Keys.Tag,
          when Version            => TOML_Keys.Version,
          when Website            => TOML_Keys.Website);

   overriding
   function Key (L : Label) return String is (Key (L.Name));

   overriding
   function To_TOML (L : Label) return TOML.TOML_Value
   is (TOML.Create_String (L.Value));

end Alire.Properties.Labeled;
