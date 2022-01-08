with Alire.Errors;
with Alire.Warnings;

package body Alire.Properties.Licenses is

   function Legacy_To_SPDX (Legacy : Licensing.Licenses) return String
   is (case Legacy is
          when AFL_3_0            => "AFL-3.0",
          when AGPL_3_0           => "AGPL-3.0-only",
          when Apache_2_0         => "Apache-2.0",
          when Artistic_2_0       => "Artistic-2.0",
          when BSD_2_Clause       => "BSD-2-Clause",
          when BSD_3_Clause_Clear => "BSD-3-Clause-Clear",
          when BSD_3_Clause       => "BSD-3-Clause",
          when BSL_1_0            => "BSL-1.0",
          when CC0_1_0            => "CC0-1.0",
          when CC_BY_4_0          => "CC-BY-4.0",
          when CC_BY_SA_4_0       => "CC-BY-SA-4.0",
          when ECL_2_0            => "ECL-2.0",
          when EPL_1_0            => "EPL-1.0",
          when EPL_2_0            => "EPL-2.0",
          when EUPL_1_1           => "EUPL-1.1",
          when EUPL_1_2           => "EUPL-1.2",
          when GPL_2_0            => "GPL-2.0-only",
          when GPL_3_0            => "GPL-3.0-only",
          when ISC                => "ISC",
          when LGPL_2_1           => "LGPL-2.1-only",
          when LGPL_3_0           => "LGPL-3.0-only",
          when LPPL_1_3c          => "LPPL-1.3c",
          when MIT                => "MIT",
          when MPL_2_0            => "MPL-2.0",
          when MS_PL              => "MS-PL",
          when MS_RL              => "MS-RL",
          when NCSA               => "NCSA",
          when OFL_1_1            => "OFL-1.1",
          when OSL_3_0            => "OSL-3.0",
          when PostgreSQL         => "PostgreSQL",
          when Unlicense          => "Unlicense",
          when WTFPL              => "WTFPL",
          when Zlib               => "Zlib",
          when GMGPL_2_0          => "GPL-2.0-only WITH GCC-exception-2.0",
          when GMGPL_3_0          => "GPL-3.0-only WITH GCC-exception-3.1",
          when Public_Domain      => "custom-public-domain",
          when Custom             => "custom-not-specified",
          when Unknown            =>
             raise Checked_Error
               with Errors.Set ("Invalid legacy license conversion for '" &
                                  Legacy'Img & "'"));

   -----------------
   -- New_License --
   -----------------

   function New_License (From  : String) return License is
      Legacy   : constant Licensing.Licenses := Licensing.From_String (From);
      SPDX_Exp : constant SPDX.Expression := SPDX.Parse (From,
                                                         Allow_Custom => True);
   begin

      if From'Length > Max_SPDX_Expression_Length then
         Raise_Checked_Error
           ("License expression too long (must be no more than" &
            Max_SPDX_Expression_Length'Img & " chars)");
      end if;

      if not SPDX.Valid (SPDX_Exp) then
         if Legacy = Licensing.Unknown then
            raise Checked_Error
              with Errors.Set
                ("Invalid license expression '" & From &
                   "': " & SPDX.Error (SPDX_Exp) &
                   " . SPDX expression expected (https://spdx.org/licenses/)");
         else
            Trace.Warning ("Deprecated license identifier '" & From &
                             "'. Please replace with an SPDX expression " &
                             "(https://spdx.org/licenses/)");

            --  Try again with a translation of Legacy identifiers to SPDX
            return New_License (Legacy_To_SPDX (Legacy));
         end if;
      end if;

      return License'(Holder => SPDX_Holder.To_Holder (SPDX_Exp));
   end New_License;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      use TOML;
      use all type Conditional.Properties;
      Props : Conditional.Properties;
      Value : constant TOML.TOML_Value := From.Pop;
   begin
      if Value.Kind = TOML_Array then
         Warnings.Warn_Once
           (Errors.Stack
              ("Array of license in manifest is deprecated. " &
                 "License should be a single string containing a " &
                 "valid SPDX expression (https://spdx.org/licenses/)"));

         for I in 1 .. Value.Length loop
            if Value.Item (I).Kind = TOML_String then
               Props := Props and
                 Conditional.For_Properties.New_Value
                   (New_License (Value.Item (I).As_String));
            else
               raise Checked_Error with "licenses must be strings";
            end if;
         end loop;

      elsif Value.Kind = TOML_String then
         Props := Props and
           Conditional.For_Properties.New_Value
             (New_License (Value.As_String));
      else
         raise Checked_Error with "licenses must be a string";
      end if;

      return Props;
   exception
      when E : Checked_Error => -- May happen on unknown non-custom license.
         From.Checked_Error (Errors.Get (E));
         --  Re-raise with full context of From.
   end From_TOML;

end Alire.Properties.Licenses;
