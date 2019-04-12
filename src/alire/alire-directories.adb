with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Alire.Paths;

package body Alire.Directories is
   
   ----------------------
   -- Detect_Root_Path --
   ----------------------

   function Detect_Root_Path (Starting_At : Absolute_Path := Current) return String is
      use Ada.Directories;

      function Is_Candidate_Folder return Boolean is         
      begin
         return Exists (Current / Paths.Working_Folder_Inside_Root) and then
           Find_Single_File (Current / Paths.Working_Folder_Inside_Root, Paths.Crate_File_Extension_With_Dot) /= "";
      end Is_Candidate_Folder;
      
      G : Guard (Enter (Starting_At)) with Unreferenced;
   begin
      Trace.Debug ("Starting root search at " & Current);
      loop
         if Is_Candidate_Folder then
            return Current;
         else
            Set_Directory (Containing_Directory (Current));
            Trace.Debug ("Going up to " & Current);
         end if;
      end loop;
   exception
      when Use_Error =>
         return ""; -- There's no containing folder (hence we're at root)
   end Detect_Root_Path;
   
   ----------------------
   -- Find_Single_File --
   ----------------------

   function Find_Single_File (Path : String; Extension : String) return Absolute_Path is
      use Ada.Directories;
      Search : Search_Type;
      File   : Directory_Entry_Type;
   begin
      Start_Search (Search    => Search,
                    Directory => Path,
                    Pattern   => "*" & Extension,
                    Filter    => (Ordinary_File => True, others => False));
      if More_Entries (Search) then
         Get_Next_Entry (Search, File);
         return Name : constant String := 
           (if More_Entries (Search) 
            then "" 
            else Full_Name (File))
         do    
            End_Search (Search);
         end return;
      else
         End_Search (Search);
         return "";
      end if;
   end Find_Single_File;
   
   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Guard) is
      use Ada.Strings.Unbounded;
   begin
      This.Original := To_Unbounded_String (Current);
      if This.Enter /= null and then
         This.Enter.all /= Ada.Directories.Current_Directory and then
         This.Enter.all /= ""
      then
         Trace.Debug ("Entering folder: " & This.Enter.all);
         Ada.Directories.Set_Directory (This.Enter.all);
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Guard) is
      use Ada.Directories;
      use Ada.Exceptions;
      use Ada.Strings.Unbounded;
      procedure Free is new Ada.Unchecked_Deallocation (String, Destination);
      Freeable : Destination := This.Enter;
   begin
      if This.Enter /= null and then Current_Directory /= To_String (This.Original) then
         Log ("Going back to folder: " & To_String (This.Original), Debug);
         Ada.Directories.Set_Directory (To_String (This.Original));
      end if;
      Free (Freeable);
   exception
      when E : others =>
         Trace.Debug ("FG.Finalize: unexpected exception: " &
                        Exception_Name (E) & ": " & Exception_Message (E) & " -- " &
                        Exception_Information (E));
   end Finalize;

end Alire.Directories;
