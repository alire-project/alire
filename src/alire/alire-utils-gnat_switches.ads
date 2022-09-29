with AAA.Strings;

with Alire.Utils.Switches; use Alire.Utils.Switches;

package Alire.Utils.GNAT_Switches
with Preelaborate
is

   pragma Style_Checks ("M120");

   GNAT_Optimize_Performance                  : constant Switch := "-O3";
   GNAT_Optimize_Debug                        : constant Switch := "-Og";
   GNAT_Optimize_Size                         : constant Switch := "-Os";
   GNAT_Enable_Inlining                       : constant Switch := "-gnatn";
   GNAT_Asserts_And_Contracts                 : constant Switch := "-gnata";
   GNAT_Debug_Info                            : constant Switch := "-g";
   GNAT_Extra_Exception_Info                  : constant Switch := "-gnateE";
   GNAT_Suppress_Runtime_Check                : constant Switch := "-gnatp";
   GNAT_Enable_Overflow_Check                 : constant Switch := "-gnato";
   GNAT_Disable_Warn_No_Exception_Propagation : constant Switch := "-gnatw.X";
   GNAT_Dont_Quit                             : constant Switch := "-gnatQ";
   GNAT_All_Warnings                          : constant Switch := "-gnatwa";
   GNAT_All_Validity_Checks                   : constant Switch := "-gnatVa";
   GNAT_Warnings_As_Errors                    : constant Switch := "-gnatwe";
   GNAT_Function_Sections                     : constant Switch := "-ffunction-sections";
   GNAT_Data_Sections                         : constant Switch := "-fdata-sections";
   GNAT_Ada83                                 : constant Switch := "-gnat83";
   GNAT_Ada95                                 : constant Switch := "-gnat95";
   GNAT_Ada05                                 : constant Switch := "-gnat05";
   GNAT_Ada12                                 : constant Switch := "-gnat12";
   GNAT_Ada2022                               : constant Switch := "-gnat2022";
   GNAT_Ada_Extensions                        : constant Switch := "-gnatX";
   GNAT_UTF8_Encoding                         : constant Switch := "-gnatW8";

end Alire.Utils.GNAT_Switches;
