with Ada.Directories;

with AAA.Strings;

with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.Utils.Text_Files;

package body Alire.Roots.Probes is

   use Directories.Operators;

   Probe_Unit : constant String := "alire_toolchain_probe";
   --  Name of the empty unit, and of the folder that holds its spec

   Probe_Spec : constant Simple_File := Probe_Unit & ".ads";

   ----------------
   -- Object_Dir --
   ----------------

   function Object_Dir (Project_File : Absolute_File) return String
   --  The object dir of the root project in Project_File, or "" if unknown.
   --  gprls gives it first in its object search path.
   is
      use AAA.Strings;
      Output : AAA.Strings.Vector;
      Code   : constant Integer :=
                 OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
                   ("gprls",
                    Empty_Vector & "-v" & "-P" & Project_File,
                    Output,
                    Err_To_Out => True);
   begin
      Trace.Debug ("gprls exited with code" & Code'Image);

      for I in Output.First_Index .. Output.Last_Index - 1 loop
         if Trim (Output (I)) = "Object Search Path:" then
            declare
               Dir : constant String := Trim (Output (I + 1));
            begin
               --  Without the trailing separator that gprls adds

               if Dir'Length > 1 and then Dir (Dir'Last) in '/' | '\' then
                  return Dir (Dir'First .. Dir'Last - 1);
               else
                  return Dir;
               end if;
            end;
         end if;
      end loop;

      return "";
   end Object_Dir;

   ------------------
   -- Ada_Compiler --
   ------------------

   function Ada_Compiler (This         : in out Root;
                          Project_File : Relative_File)
                          return Ada_Compiler_Probe
   is
      use AAA.Strings;

      ---------------
      -- Not_Found --
      ---------------

      function Not_Found (Reason : String) return Ada_Compiler_Probe
      is (Found => False, Reason => +Reason);

      Project : constant Absolute_File := Roots.Path (This) / Project_File;

   begin
      for Tool of To_Vector ("gprbuild").Append ("gprls") loop
         if OS_Lib.Subprocess.Locate_In_Path (Tool) = "" then
            return Not_Found (Tool & " not found in PATH");
         end if;
      end loop;

      declare
         Obj : constant String := Object_Dir (Project);
      begin
         if Obj = "" or else not Check_Absolute_Path (Obj) then
            return Not_Found ("gprls did not give the object directory of "
                              & Project_File);
         end if;

         declare
            Probe_Dir : constant Absolute_Path := Obj / Probe_Unit;

            --------------
            -- Clean_Up --
            --------------

            procedure Clean_Up is
               --  Delete the probe, its compilation output, and the folders
               --  of the object dir left empty, as gprls may create them.

               Probe_Items : AAA.Strings.Vector;

               -------------
               -- Collect --
               -------------

               procedure Collect (Item : Any_Path; Stop : in out Boolean) is
                  Name : constant String := Ada.Directories.Simple_Name (Item);
               begin
                  Stop := False;
                  if Name = Probe_Unit
                    or else Has_Prefix (Name, Probe_Unit & ".")
                  then
                     Probe_Items.Append (Directories.Full_Name (Item));
                  end if;
               end Collect;

               --------------
               -- Is_Empty --
               --------------

               function Is_Empty (Dir : Absolute_Path) return Boolean is
                  Empty : Boolean := True;

                  procedure Check (Unused_Item : Any_Path;
                                   Stop        : in out Boolean) is
                  begin
                     Empty := False;
                     Stop  := True;
                  end Check;

               begin
                  Directories.Traverse_Tree (Dir, Check'Access);
                  return Empty;
               end Is_Empty;

               ---------------------
               -- Common_Ancestor --
               ---------------------

               function Common_Ancestor (Path : Absolute_Path)
                                         return Absolute_Path
               --  The closest folder containing both Path and Obj, which may
               --  be outside the root (e.g., test crates use the object dir
               --  of their parent crate).
               is (if Has_Prefix (Obj, Path & GNAT.OS_Lib.Directory_Separator)
                     or else Directories.Parent (Path) = Path
                   then Path
                   else Common_Ancestor (Directories.Parent (Path)));

               Limit : constant Absolute_Path :=
                         Common_Ancestor (Roots.Path (This));
               Dir   : UString := +Obj;

            begin
               if not Directories.Is_Directory (Obj) then
                  return;
               end if;

               Directories.Traverse_Tree (Obj, Collect'Access);
               for Item of Probe_Items loop
                  Directories.Delete_Tree (Item);
               end loop;

               while Has_Prefix (+Dir, Limit & GNAT.OS_Lib.Directory_Separator)
                 and then Is_Empty (+Dir)
               loop
                  Directories.Delete_Tree (+Dir);
                  Dir := +Directories.Parent (+Dir);
               end loop;
            end Clean_Up;

            ------------------
            -- Compile_Only --
            ------------------

            function Compile_Only (Source : Simple_File;
                                   Extra  : AAA.Strings.Vector)
                                   return Ada_Compiler_Probe
            --  Have gprbuild compile only Source, and find the compiler in its
            --  output, or else gprbuild's reasons.
            is
               Output : AAA.Strings.Vector;
               Code   : constant Integer :=
                          OS_Lib.Subprocess.Unchecked_Spawn_And_Capture
                            ("gprbuild",
                             Empty_Vector
                             & "-P" & Project
                             & "-c" -- Compile only
                             & "-u" -- Only the given source
                             & "-v" -- Show the command lines
                             & Extra
                             & Source,
                             Output,
                             Err_To_Out => True);
               Messages : AAA.Strings.Vector;
            begin

               --  The compiler is the first word of the command line that
               --  compiles Source. The path may contain spaces, but not
               --  " -c ".

               for Line of Output loop
                  if Contains (Line, Source) and then Contains (Line, " -c ")
                  then
                     declare
                        Driver   : constant String := Head (Line, " -c ");
                        Absolute : constant String :=
                                     (if Check_Absolute_Path (Driver)
                                      then Driver
                                      else OS_Lib.Subprocess.Locate_In_Path
                                        (Driver));
                     begin
                        if Absolute = "" then
                           return Not_Found ("gprbuild runs " & Driver
                                             & ", which is not in PATH");
                        else
                           return (Found => True, Driver => +Absolute);
                        end if;
                     end;
                  end if;
               end loop;

               --  No compilation happened, and gprbuild says why in its own
               --  messages, which include those of the configuration step.

               for Line of Output loop
                  if Has_Prefix (Line, "gprbuild:")
                    or else Has_Prefix (Line, "kb:")
                    or else Contains (Line, ">>>")
                  then
                     Messages.Append (Trim (Line));
                  end if;
               end loop;

               if Messages.Is_Empty then
                  return Not_Found ("gprbuild exited with code" & Code'Image
                                    & " without stating a reason");
               else
                  return Not_Found (Messages.Flatten ("; "));
               end if;
            end Compile_Only;

            ------------------------
            -- Check_Config_Spec --
            ------------------------

            function Check_Config_Spec return Ada_Compiler_Probe
            --  Have gprbuild check the syntax of the configuration spec that
            --  Alire generates for the root, which writes no object files.
            --  Only the captures of the compiler output are rewritten, so
            --  their previous contents are restored.
            is
               Spec     : constant Simple_File :=
                            This.Name.As_String & "_config.ads";
               Captures : constant AAA.Strings.Vector :=
                            Empty_Vector
                            & String'(Obj / Spec & ".stdout")
                            & String'(Obj / Spec & ".stderr");
               Saved    : constant String := "." & Probe_Unit;
            begin
               for Capture of Captures loop
                  if Directories.Is_File (Capture) then
                     Ada.Directories.Copy_File (Capture, Capture & Saved);
                  end if;
               end loop;

               return Result : constant Ada_Compiler_Probe :=
                 Compile_Only (Spec,
                               Extra => Empty_Vector
                                        & "-f" -- Even if up to date
                                        & "-cargs:Ada"
                                        & "-gnats") -- Syntax only
               do
                  for Capture of Captures loop
                     if Directories.Is_File (Capture & Saved) then
                        Ada.Directories.Copy_File (Capture & Saved, Capture);
                        Ada.Directories.Delete_File (Capture & Saved);
                     elsif Directories.Is_File (Capture) then
                        Ada.Directories.Delete_File (Capture);
                     end if;
                  end loop;
               end return;
            end Check_Config_Spec;

         begin
            Directories.Create_Tree (Probe_Dir);
            Utils.Text_Files.Write_Lines
              (Probe_Dir / Probe_Spec,
               To_Vector ("package " & Probe_Unit & " is end " & Probe_Unit
                          & ";"));

            declare
               Src_Subdirs : constant String := "--src-subdirs=" & Probe_Unit;
               Result      : constant Ada_Compiler_Probe :=
                               Compile_Only (Probe_Spec,
                                             Extra => To_Vector (Src_Subdirs));
            begin
               Clean_Up;

               --  When another project of the tree shares the object dir, as
               --  test crates do with their parent crate, the probe would be a
               --  source of both, which gprbuild rejects. The configuration
               --  spec of the root belongs only to it, so it can be used then.

               if not Result.Found
                 and then Contains (+Result.Reason,
                                    "cannot belong to several projects")
               then
                  declare
                     Fallback : constant Ada_Compiler_Probe :=
                                  Check_Config_Spec;
                  begin
                     Clean_Up;
                     return Fallback;
                  end;
               end if;

               return Result;
            end;
         exception
            when others =>
               Clean_Up;
               raise;
         end;
      end;
   end Ada_Compiler;

end Alire.Roots.Probes;
