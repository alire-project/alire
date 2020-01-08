private with Alire.Platforms;
private with Alire.Utils;

package Alire.Externals.From_Native is

   --  A natively-provided package, installed via a platform-specific method
   --  such as apt, yum, etc. that can also inform us about the version.

   type External is new Externals.External with private;

   overriding
   function Detect (This : External;
                    Name : Crate_Name) return Containers.Release_Set;

   overriding
   function Image (This : External) return String;

   overriding
   function Detail (This   : External;
                    Distro : Platforms.Distributions)
                    return Utils.String_Vector;

   overriding
   function Kind (This : External) return String is ("Native package");

   function From_TOML (From : TOML_Adapters.Key_Queue) return External;
   --  From must point to the table with the keys that describe the external.

private

   subtype Package_Vector is Utils.String_Vector;

   type Candidates is array (Platforms.Known_Distributions) of Package_Vector;

   type Packages (Is_Case : Boolean := False) is record
      case Is_Case is
         when False =>
            Common_Candidates : Package_Vector;
         when True  =>
            Distro_Candidates : Candidates;
      end case;
   end record;

   function Candidate_Count (This : Packages) return Natural;

   type External is new Externals.External with record
      Origin : Packages;
   end record;

   function Native_Candidates (This   : External;
                               Distro : Platforms.Distributions)
                               return Package_Vector
   is
     (if This.Origin.Is_Case
      then This.Origin.Distro_Candidates (Distro)
      else This.Origin.Common_Candidates);

end Alire.Externals.From_Native;
