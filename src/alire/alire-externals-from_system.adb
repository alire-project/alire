with Alire.Index;
with Alire.Origins.Deployers.System;
with Alire.Platform;
with Alire.Releases;
with Alire.TOML_Adapters;
with Alire.TOML_Expressions;
with Alire.TOML_Keys;

with TOML;

package body Alire.Externals.From_System is

   ---------------------
   -- Candidate_Count --
   ---------------------

   function Candidate_Count (This : Packages) return Natural is
   begin
      if This.Is_Case then
         return Total : Natural := 0 do
            for Distro of This.Distro_Candidates loop
               Total := Total + Natural (Distro.Length);
            end loop;
         end return;
      else
         return Natural (This.Common_Candidates.Length);
      end if;
   end Candidate_Count;

   ------------
   -- Detect --
   ------------

   overriding
   function Detect (This : External;
                    Name : Crate_Name) return Containers.Release_Set
   is
      package System renames Origins.Deployers.System;
   begin
      --  No need to look for anything if the distro is unknown:
      if not Platform.Distribution_Is_Known then
         Trace.Detail ("Cannot look for system packages for crate " & (+Name)
                       & "in unknown distribution");
         return Containers.Release_Sets.Empty_Set;
      end if;

      Trace.Debug ("Looking for system packages that provide crate: "
                   & (+Name));

      return Releases : Containers.Release_Set do
         for Candidate of This.System_Candidates (Platform.Distribution) loop
            Trace.Detail ("Looking for system package: " & Candidate);
            declare
               Detector : constant System.Deployer'Class :=
                            System.Platform_Deployer (Candidate);
               Result   : constant System.Version_Outcomes.Outcome :=
                            Detector.Detect;
            begin
               if Result.Success then
                  Trace.Detail ("Success with system package: " & Candidate);

                  Releases.Insert
                    (Index.Crate (Name).Base
                     .Retagging (Result.Value)
                     .Replacing (Origins.New_System (Candidate))
                     .Replacing (Notes => "Provided by system package: "
                                 & Candidate));
               end if;
            end;
         end loop;
      end return;
   end Detect;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return External is

      ----------------
      -- From_Array --
      ----------------

      function From_Array (Values : TOML.TOML_Value) return External is
        (Externals.External with
         Origin =>
           (Is_Case           => False,
            Common_Candidates => TOML_Adapters.To_Vector (Values)));

      ---------------
      -- From_Case --
      ---------------

      function From_Case (Case_From : TOML.TOML_Value) return External is
         package Distros is new TOML_Expressions.Enum_Cases
           (Platforms.Known_Distributions);

         Result : External := (Externals.External with
                               Origin => (Is_Case => True,
                                          others  => <>));
      begin
         if Case_From.Keys'Length /= 1 or else
         +Case_From.Keys (1) /= "case(distribution)"
         then
            From.Checked_Error
              ("system origins can only be distribution-specific");
         end if;

         --  Get an array of TOML values that will each point to a distribution
         --  specific array of candidate packages:

         declare
            use type TOML.TOML_Value;
            Distro_Origins : constant Distros.TOML_Array :=
                               Distros.Load_Cases
                                 (TOML_Adapters.From
                                    (Case_From.Get (Case_From.Keys (1)),
                                     From.Message ("case")));
         begin
            for Distro in Distro_Origins'Range loop
               if Distro_Origins (Distro) /= TOML.No_TOML_Value then

                  if Distro_Origins (Distro).Kind not in TOML.TOML_Array then
                     From.Checked_Error
                       ("case(distribution): "
                        & "array of candidate packages expected, but got: "
                        & Distro_Origins (Distro).Kind'Img);
                  end if;

                  Result.Origin.Distro_Candidates (Distro) :=
                    TOML_Adapters.To_Vector (Distro_Origins (Distro));
               end if;
            end loop;

            return Result;
         end;
      end From_Case;

      Value : TOML.TOML_Value;
   begin
      if not From.Pop (TOML_Keys.Origin, Value) then
         From.Checked_Error ("mandatory origin missing");

      elsif Value.Kind in TOML.TOML_Table then
         --  A table: a case origin.
         return From_Case (Value);

      elsif Value.Kind in TOML.TOML_Array then
         --  List of possible packages
         return From_Array (Value);
      else
         From.Checked_Error ("origin: expected array or case table");
      end if;
   end From_TOML;

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : External) return String is
     (Utils.Trim (Candidate_Count (This.Origin)'Img)
      & " candidate system packages");

   ------------
   -- Detail --
   ------------

   overriding
   function Detail (This   : External;
                    Distro : Platforms.Distributions)
                    return Utils.String_Vector
   is
      use all type Platforms.Distributions;
   begin
      if This.Origin.Is_Case then
         return Result : Utils.String_Vector do
            for I in This.Origin.Distro_Candidates'Range loop
               if Distro = I or else Distro = Distro_Unknown then
                  Result.Append
                    (Utils.To_Mixed_Case (I'Img) & ": "
                     & This.Origin.Distro_Candidates (I).Flatten (", "));
               end if;
            end loop;
         end return;
      else
         return Utils.Empty_Vector.Append
           (This.Origin.Common_Candidates.Flatten (", "));
      end if;
   end Detail;

end Alire.Externals.From_System;
