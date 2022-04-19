with Alire.Conditional_Trees.TOML_Load;
with Alire.Index;
with Alire.Origins.Deployers.System;
with Alire.Platforms.Current;
with Alire.Properties.Platform;
with Alire.Releases;
with Alire.Root;
with Alire.TOML_Adapters;

package body Alire.Externals.From_System is

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : External;
                    Name : Crate_Name) return Releases.Containers.Release_Set
   is
      package System renames Origins.Deployers.System;
   begin
      --  No need to look for anything if the distro is unknown:
      if not Platforms.Current.Distribution_Is_Known then
         Trace.Detail ("Cannot look for system packages for crate " & (+Name)
                       & " in unknown distribution");
         return (Releases.Containers.Release_Sets.Empty_Set with null record);
      end if;

      Trace.Debug ("Looking for system packages that provide crate: "
                   & (+Name));

      return Releases : Alire.Releases.Containers.Release_Set do
         declare
            Origin : constant Conditional_Packages.Tree :=
                       This.Origin.Evaluate (Root.Platform_Properties);
         begin
            if Origin.Is_Empty then
               Trace.Debug ("No system packages for current platform");
            else
               for Candidate of Origin.Value.Packages loop
                  Trace.Detail ("Looking for system package: " & Candidate);
                  declare
                     Detector : constant System.Deployer'Class :=
                                  System.Platform_Deployer (Candidate);
                     Result   : constant System.Version_Outcomes.Outcome :=
                                  Detector.Detect;
                  begin
                     if Result.Success then
                        Trace.Detail ("Success with system package: "
                                      & Candidate);

                        Releases.Insert
                          (Index.Crate (Name, Index.Query_Mem_Only).Base
                           .Retagging (Result.Value)
                           .Providing (This.Provides)
                           .Replacing (Origins.New_System (Candidate))
                           .Replacing (Notes => "Provided by system package: "
                                       & Candidate));
                     end if;
                  end;
               end loop;
            end if;
         end;
      end return;
   end Detect;

   ----------------------
   -- From_TOML_Static --
   ----------------------
   --  Loads a simple origin = [] origin, for reuse in the dynamic expr loader
   function From_TOML_Static (From : TOML_Adapters.Key_Queue)
                              return Conditional_Packages.Tree
   is

      ----------------
      -- From_Array --
      ----------------

      function From_Array (Values : TOML.TOML_Value) return Package_Vector
      is (Packages => TOML_Adapters.To_Vector (Values));

      Value : TOML.TOML_Value;
   begin
      if not From.Pop (TOML_Keys.Origin, Value) then
         From.Checked_Error ("mandatory origin missing");
      elsif Value.Kind in TOML.TOML_Array then
         --  List of possible packages
         return Conditional_Packages.New_Value (From_Array (Value));
      else
         From.Checked_Error ("origin: expected array of candidate packages");
      end if;
   end From_TOML_Static;

   ---------------
   -- From_TOML --
   ---------------

   package Loader is new Conditional_Packages.TOML_Load;

   function From_TOML (From : TOML_Adapters.Key_Queue) return External is
   begin
      return (Externals.External with
                Origin => Loader.Load
                  (From    =>
                   --  We detach the 'origin' entry by itself to avoid the
                   --  expression parser to complain about too many entries.
                     From.Descend (Key     => TOML_Keys.Origin,
                                   Value   => From.Pop (TOML_Keys.Origin),
                                   Context => TOML_Keys.Origin),
                   Loader  => From_TOML_Static'Access,
                   Resolve => True,
                   Strict  => False));
   end From_TOML;

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : External) return String is
      Candidates : Natural := 0;
   begin
      for Packages of This.Origin.As_List loop
         Candidates := Candidates + Natural (Packages.Packages.Length);
      end loop;

      return AAA.Strings.Trim (Candidates'Image)
        & (if Candidates = 1
           then " candidate system package"
           else " candidate system packages");
   end Image;

   ------------
   -- Detail --
   ------------

   overriding
   function Detail (This   : External;
                    Distro : Platforms.Distributions)
                    return AAA.Strings.Vector
   is
      Result : AAA.Strings.Vector;
      use Alire.Properties;
      use type Platforms.Distributions;
   begin
      for Concrete_Distro in Platforms.Known_Distributions loop

         --  We show either the requested Distro only, or all distros, which is
         --  signaled by Distro = Unknown.

         if Concrete_Distro = Distro or else Distro = Platforms.Distro_Unknown
         then
            declare
               On_Distro : constant Conditional_Packages.Tree :=
                             This.Origin.Evaluate
                               (To_Vector
                                  (Properties.Platform.Distributions
                                   .New_Property (Concrete_Distro)));
            begin
               if not On_Distro.Is_Empty then
                  Result.Append
                    (TOML_Adapters.Adafy (Concrete_Distro'Image) &
                       ": " & On_Distro.Image_One_Line);
               end if;
            end;
         end if;
      end loop;
      Result.Append ("others: unavailable");
      return Result;
   end Detail;

   -----------
   -- Image --
   -----------

   function Image (This : Package_Vector) return String
   is (This.Packages.Flatten (", "));

end Alire.Externals.From_System;
