with AAA.Enum_Tools;
with GNAT.OS_Lib;

with Alire.OS_Lib.Subprocess;

with System;

package body Alire.Platforms.Common is

   Arch_Detected : Boolean       := False;
   Arch_Value    : Architectures := Architecture_Unknown;

   ---------------------------
   -- Machine_Hardware_Name --
   ---------------------------

   function Machine_Hardware_Name return Architectures is
   begin
      if Arch_Detected then
         return Arch_Value;
      end if;

      --  Custom detection for Windows without requiring msys2

      if On_Windows then
         Arch_Detected := True;
         case Standard.System.Word_Size is
            when 64 =>
               Arch_Value := X86_64;
            when others =>
               Arch_Value := Architecture_Unknown;
         end case;

         return Arch_Value;
      end if;

      --  Detection in unix-like platforms

      Detect : declare
         function Is_Known_Arch is
           new AAA.Enum_Tools.Is_Valid (Extended_Architectures);

         Output : AAA.Strings.Vector;

         --  uname is part of coreutils, so it should be available in
         --  linuxes/msys2.

         Code   : constant Integer :=
                    OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
                      (Command             => "uname",
                       Arguments           => AAA.Strings.To_Vector ("-m"),
                       Output              => Output);

         Name   : constant String := Output.Flatten;
      begin
         Arch_Detected := True;

         if Code /= 0 then
            Trace.Debug ("uname failed with code: "
                         & AAA.Strings.Trim (Code'Image));
         else
            if Is_Known_Arch (Name) then
               Trace.Debug ("uname known machine string is: " & Name);
               Arch_Value :=
                 Arch_Mapping (Extended_Architectures'Value (Name));
            else
               Trace.Debug ("uname unknown machine string is: " & Name);
            end if;
         end if;

         return Arch_Value;
      end Detect;
   end Machine_Hardware_Name;

   ----------------------
   -- Unix_Home_Folder --
   ----------------------

   function Unix_Home_Folder return String
   is
      Home_Var : constant String := OS_Lib.Getenv ("HOME", "unset");
      Maybe_Windows : constant Boolean := Home_Var = "unset"
         and then GNAT.OS_Lib.Directory_Separator = '\';
   begin
      if Maybe_Windows then
         raise Checked_Error with
            "$HOME is not set, you might be running an"
            & " `alr` built for a non-Windows OS";
      elsif Home_Var = "unset" or else
       not GNAT.OS_Lib.Is_Write_Accessible_File (Home_Var)
      then
         return "/tmp";
      else
         return Home_Var;
      end if;
   end Unix_Home_Folder;

end Alire.Platforms.Common;
