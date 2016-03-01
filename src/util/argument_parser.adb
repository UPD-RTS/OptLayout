with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Global; use Global;
with Ada.Strings.Unbounded;
with Ada.Calendar, Utilities;  --use Ada.Calendar;

package body Argument_Parser is

   package US renames Ada.Strings.Unbounded;

   procedure Get_Arguments is
      use Ada.Command_Line, Ada.Text_IO, US;
      nextFIle : FileInput := NOTAFILE;
      modeFlag : Boolean := False;
      lctOpt : Boolean := True;
      -- Has a specific target been defined?
      targetDefined : Boolean := False;
      --option : Unbounded_String;
   begin
      if Argument_Count = 0 then
         raise Argument_Error;
      end if;
      New_Line;
      Put ("  Shell output mode:");
      for I in 1..Argument_Count loop
         declare
            A: String := Argument(I);
         begin
            if nextFile /= NOTAFILE then
               if (nextFIle = ROOT) then
                  File_Name := new String'(Argument(I));
               elsif (nextFIle = PROGRAM) then
                  --Program_File := new String'(Argument(I));
                  Global.Program := US.To_Unbounded_String(Argument(I));
                  Global.PName_Spec := True;
               elsif (nextFIle = LOOPS) then
                  Loop_File := new String'(Argument(I));
               elsif (nextFIle = CACHE) then
                  Cache_File := new String'(Argument(I));
               else
                  Constr_File := new String'(Argument(I));
               end if;
               nextFile := NOTAFILE;
            elsif    A(1..2) = "-v" then
               modeFlag := True;
               Put (" Verbose");
               Global.E_M := VERBOSE;
            elsif    A(1..2) = "-d" then
               modeFlag := True;
               Put (" Debug");
               Global.E_M := DEBUG;
            elsif    A(1..2) = "-g" then
               modeFlag := True;
               Put (" Graph Debug");
               Global.E_M := GRAPHDB;
            elsif (A(1..2) = "-t" and then
                     A(2..5) = "tcfg") then
               Put (" Print CFG Debug Info");
               tbshootCFG := True;
            elsif (A(1..2) = "-t" and then
                     A(2..5) = "tlct") then
               Put (" Print LCT Debug Info");
               tbshootLCT := True;
            elsif (A(1..2) = "-t" and then
                     A(2..5) = "tdom") then
               Put (" Print DOM Debug Info");
               tbshootDOM := True;
            elsif (A(1..2) = "-t" and then
                     A(2..5) = "tSiz") then
               Put (" Print SIZE Debug Info");
               tbshootSize := True;
            elsif (A(1..2) = "-t" and then
                     A(2..5) = "tlay") then
               Put (" Print LAYOUT Debug Info");
               tbshootCFG := True;
            elsif (A(1..2) = "-t" and then
                     A(2..5) = "tfin") then
               Put (" Print FINAL LOOP TREE");
               drawFinal := True;
            elsif (A(1..2) = "-t" and then
                     A(2..5) = "tlop") then
               Put (" Print procedure-loop criticality ");
               tbshootLOP := True;
            elsif (A(1..2) = "-t" and then
                     A(2..5) = "twcg") then
               Put (" Print WCG Debug Info ");
               tbshootWCG := True;
            elsif    A(1..2) = "-m" then
               modeFlag := True;
               Put (" Trace intermediate progress");
               Global.E_M := MINIMAL;
            elsif (A(1..2) = "-n" and then
                   A(2..7) = "noDraw"  ) then
               drawGraphs := False;
            elsif (A(1..2) = "-n" and then
                   A(2..9) = "noLocOpt"  ) then
               localOpti := False;
               lctOpt := False;
               --Put (" No Local Optimization");
               US.Append(execOpt," LCT-based optimization (no Local Optimization)");
            elsif (A(1..2) = "-w" and then
                     A(2..4) = "wcg") then
               --Put (" WCG Optimization");
               US.Append(execOpt,"WCG-based optimization");
               lctOpt := False;
               wcgOpti := True;
            elsif (A(1..2) = "-w" and then
                     A(2..5) = "ways"  ) then
               printWays := True;
            elsif A(1..2) = "-h" then
               Usage;
               Global.E_M := NONE;
               Ada.Command_Line.Set_Exit_Status(Failure);
               return;
            elsif (A(1..2) = "-t" and then
                     A(2..5) = "tsim") then
               targetDefined := True;
               target := TSIM;
            elsif (A(1..2) = "-t" and then
                     A(2..4) = "tas") then
               targetDefined := True;
               target := TAS;
            elsif (A(1..2) = "-l" and then
                     A(2..6) = "leon2") then
               targetDefined := True;
               target := LEON2;
            elsif A(1..2) = "-b" then
               nextFile := LOOPS;
            elsif A(1..2) = "-c" then
               nextFile := CACHE;
            elsif A(1..2) = "-p" then
               nextFile := PROGRAM;
            elsif A(1..2) = "-s" then
               nextFile := CONSTRAINTS;
            else
               if (I=Argument_Count) then
                  File_Name := new String'(Argument(I));
               else
                  --raise Argument_Error;
                  New_Line;
                  Put_Line(A);
               end if;
            end if;
         exception
            when Constraint_Error =>
               Put_Line("# " & A);
               raise Argument_Error;
         end;
      end loop;

      if (not modeFlag) then
         Put_Line (" silent");
      else
         Put_line (" ");
      end if;
      Put ("  Optimization mode: ");
      if lctOpt then
         Put_Line ("LCT-based optimization ");
      else
         Put_Line (Utilities.Utrim(execOpt));
      end if;

      -- Default execution target is TSIM
      if not targetDefined then
         target := TSIM;
      end if;

      --File_Name := new String'(Argument(Argument_Count));
      return;
  exception
    when Argument_Error =>
         Usage;
         Ada.Command_Line.Set_Exit_Status(Failure);

  end Get_Arguments;



   function Release_Date return String is
      use Ada.Calendar, Utilities;
      --  Release info
      Now        : Time := Clock;
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Seconds    : Day_Duration;
   begin
      Split (Now, Year, Month, Day, Seconds);
      if Month > 9 and then Day > 9 then
         return "build" & Trim(Year) & Trim(Month) & Trim(Day);
      elsif Month > 9 then
         return "build" & Trim(Year) & Trim(Month) & "0" & Trim(Day);
      elsif Day > 9 then
         return "build" & Trim(Year) & "0" & Trim(Month) & Trim(Day);
      else
         return "build" & Trim(Year) & "0" & Trim(Month) & "0" & Trim(Day);
      end if;
   end Release_Date;


   procedure Usage is
   begin
      New_Line;
      Put_Line ("OptLayout  v" & Version & " - (" & Release_Date & ")");
      New_Line;
      Put_Line ("USAGE:");
      Put_Line ("optLayout [options] file");
      New_Line;
      Put_Line ("INPUTS:");
      Put_Line ("  file                                 XML file describing the program structure");
      New_Line;
      Put_Line ("OPTIONS:");
      Put_Line ("  -c <config>                          XML file describing the instruction cache");
      Put_Line ("                                       features (size, line size, associativity)");
      Put_Line ("  -s <constraints>                     XML file describing a set of constraints");
      Put_Line ("  -b <bounds>                          XML file providing loop bounds for the ");
      Put_line ("                                       analysed module");
      Put_Line ("  -p <pathToExec>                      Executable name");
      Put_Line ("  -wcg                                 Performs a WCG-based optimization");
      Put_Line ("  -noLocOpt                            LCT-based optimization with no local greedy placement");
      Put_Line ("  -v -verbose                          Verbose mode");
      Put_Line ("  -d                                   Debug mode");
      Put_Line ("  -g                                   Export debug information in Graphs");
      Put_Line ("  -m                                   Minimal verbosity mode");
      Put_Line ("  -noDraw                              Does not output graph drawings");
      Put_Line ("  -t[cfg|dom|lct|fin|lay|lop|wcg]      Low-level tree debug");
      Put_Line ("  -h -help                             Displays this message");
      New_Line;
      New_Line;
      Put_Line ("More recent version may be found at " &
               "http://www.math.unipd.it/~emezzett");
      Put_Line ("Contact: " & Author & ".");
      New_Line;

   end Usage;

end Argument_Parser;
