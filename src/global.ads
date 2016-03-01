-----------------------------------------------------------------------
--         OptLayout - A Cache-aware Memory Layout Optimizer         --
--                                                                   --
--               Copyright (c) 2011 Enrico Mezzetti                  --
--                     University of Padua                           --
--                  <emezzett@math.unipd.it>                         --
--                                                                   --
-- This file is part of OptLayout.                                   --
--                                                                   --
-- OptLayout is free software: you can redistribute it and/or modify --
-- it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, --
-- or (at your option) any later version.                            --
--                                                                   --
-- OptLayout is distributed in the hope that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public License --
-- along with OptLayout. If not, see <http://www.gnu.org/licenses/>. --
--                                                                   --
--                                                                   --
--                                                                   --
-----------------------------------------------------------------------
-- FILE NAME      : global.ads
-- PACKAGE        : GLOBAL spec
-- PURPOSE        : Global variables for program inputs and options
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;


package Global is

   -- Different program execution modes (normal, debug, verbose, etc.)
   type Exec_mode is (NONE, NORMAL, GRAPHDB, VERBOSE, DEBUG, MINIMAL);

   type Exec_Target is (TSIM, TAS, LEON2, UNDEF);

   ramStartAddress : array (Exec_Target) of Integer := (
                                                        16#40000000#,
                                                        16#40020020#,
                                                        16#40000000#,
                                                        16#40000000#);
   -- Basic address type
   subtype Address is Integer range 16#40000000#..16#45000000#;

   -- Output on the selected optimization approach (WCG, LCT greedy or LCT)
   execOpt : Unbounded_String;

   target : Exec_Target := UNDEF;

   -- Output of low-level debug info on CFG reconstruction
   tbshootCFG : Boolean := False;
   -- Output of low-level debug info on Dominator trees
   tbshootDOM : Boolean := False;
   -- Output of low-level debug info on LCT construction
   tbshootLCT : Boolean := False;
   -- Output of low-level debug info on Layout computation
   tbshootLAY : Boolean := False;
   -- Output of low-level debug info on subprograms size
   tbshootSIZE : Boolean := False;
   -- Output of low-level debug info on loop bounds
   tbshootLOP  : Boolean := False;
   -- Output of low-level debug info on LCT to WCG transformation
   tbshootWCG : Boolean := False;

   -- Draw the final (super-) LCT
   drawFinal : Boolean := False;
   -- Exploit local optimization in LCT-based approach
   localOpti : Boolean := True;
   -- Apply a WCG-based optimization
   wcgOpti : Boolean := False;

   -- XML files describing the program structure
   File_Name : access String;
   -- The program xecutable
   Program_File : access String;
   -- XML file describing the loop bounds
   Loop_File : access String;
   -- XML file describing the cache configuration
   Cache_File : access String;
   -- XML representation of a set of layout constraints
   -- (possibly from previous execution of the optimizer)
   Constr_File : access String;

   --  Name of the analysed program
   Program : Unbounded_String;
   PName_Spec : Boolean := False;

   -- Execution mode
   E_M : Exec_mode := NORMAL;
   -- Need to consider a set of given layout constraint
   Constrained : Boolean := False;
   -- Loops bound are provided and will be applied
   Bounded : Boolean := False;
   -- Export a graphical representation of CFGs, LCTs and WCG
   drawGraphs : Boolean := True;
   -- Shell output of the computed layout
   printWays  : Boolean := False;

   --  Constant info
   Author : constant String := "emezzett@math.unipd.it";
   Version : constant String := "0.5b";
   Release : constant String := "2011";

end Global;
