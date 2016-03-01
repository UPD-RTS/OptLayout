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
-- FILE NAME      : ram.ads
-- PACKAGE        : RAM spec
-- PURPOSE        : Representation of the current RAM memory.
--                  responsible of the actual mapping of procedures
--                  as well as the linker script generation
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
----------------------------------------------------------------------

with Global; use Global;
with Ada.Unchecked_Deallocation; --use Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package RAM is

   -- Size of the available memory
   ramSize : constant Integer := 400;

   -- Debug variable
   Mapped : Integer := 0;

   -- RAM tyoe declaration
   type RAM_Type is private;
   type RAM_Ptr is access all RAM_Type;

   -- Bins (from the bin packing problem) practically corresponds to cache ways
   -- Abstraction of a memory segment corresponding to a cache way,
   -- originally designed to apply binpacking algorithms
   type Bin_State is (FREE, USED, PH, FULL);

   -- Generic representation of RAM contents but limited to
   -- cache set boundaries
   type Descriptor is
      record
         name     : String (1..100);
         size     : Integer;
         startAdd : Address;
         offSet   : Integer;
      end record;

   -- Used to convert usual strings to fixed-size ones
   Fixed_Str_Desc : String (1..100);

   -- Used to represent artificial memory displacements
   PLACE_HOLDER : constant String := "__PHolder__                                                                                         ";

   -- Generic uninitialized descriptor
   Null_Descriptor : constant Descriptor :=
     ("****************************************************************************************************",
      0,
      Integer'Value("16#"&"40000000"& "#"),
      0);

   -- Equality test for descriptors
   function "=" (Left, Right : Descriptor) return Boolean;

   -- A procedure may involve more than one descriptor
   type Procedures is array (integer range <>) of Descriptor;
   -- Pointer to an array of descriptors
   type Contents is access Procedures;

    -- Unchecked deallocation of procedures
   procedure Free_Proc is new
     Ada.Unchecked_Deallocation (Procedures,
                                 Contents);

   --  Return the implementation-defined start address
   function Start_Address return Integer;

   --  Maps a procedure to the next available slot in the main memory
   --  (i.e., without constraints)
   procedure Map_Procedure (procName: String; size : Integer);

   --  Maps a procedure to the main memory overwriting a placeholder
   function Map_In_Place_Holder (procName: String; size : Integer)
                                 return Boolean;

   --  Output the computed memory map (per WAYS) to the shell
   procedure Print_Bins (flag : Boolean);

   -- Pretty outputs a descriptor
   procedure Print_Descriptor (d : Descriptor);

   --procedure Set_New_Start_Bin (i : Integer);

   --  Produces a custom linker for the computed layout
   procedure Export_Linker_Script;

   --  Produces a LEON2-compliant custom linker for the computed layout
   procedure Export_Linker_Script_Tsim;

   --  Produces a (LEON2) TAS-compliant custom linker for the computed layout
   procedure Export_Linker_Script_TAS;

   --  Produces a LEON2-compliant custom linker for the WCG computed layout
   procedure Export_Linker_Script_Tsim_WCG;


   --  Check the effects of mapping a pool (in term of conflicts)
   procedure Check_Mapping (shProc : Contents; shNum: Positive; size : Integer;
                            disp : in out Natural);

   --  Check the effects of mapping a pool (in term of conflicts)
   procedure Check_Mapping_Pool (shProc : Contents; shNum: Positive; size : Integer;
                           disp : in out Natural);

   function Already_Mapped (proc : Descriptor) return Boolean;

   procedure Already_Mapped (procName : Unbounded_String; found : out Boolean;
                             d : out Descriptor);

   --  Abstraction of a memory segment corresponding to a cache way
   --  Originally designed to apply binpacking algorithms
   type Bin is record
      Index    : Positive;
      State    : Bin_State;
      StartAdd : Address;
      EndAdd   : Address;
      NextAdd  : Address;
      Proc     : Contents;
--    Cont     : Boolean := False;
   end record;

   --  Pointer to a BIN
   type Bin_Ptr is access all Bin;

   --  Array of pointers to BINs
   type Bin_Array is array (1..ramSize) of Bin_Ptr;

   --  Returns the array of actual BINS
   function Get_Ram_Sets return Bin_Array;

   --  Initialize the memory space
   procedure Init_Ram;

   --  **UNUSED**
   procedure Load_Mapping;

   --  **UNUSED**
   procedure Save_Mapping;

   -- Type definition for relatively self-contained "Pool" of procedures
   type Proc_Pool is
      record
         -- Set of descriptors in the pool
         proc : Contents;
         -- Cumulative size of all descriptors in the pool
         size : Integer;
         -- Number of descriptors in the pool
         num : Natural;
         -- A procedure in the pool is shared with another pool
         sharing : Boolean := False;
      end record;
   type Proc_Pool_ptr is access Proc_Pool;

    -- Unchecked deallocation of a procedure pool
   procedure Free_Pool is new
     Ada.Unchecked_Deallocation (Proc_Pool,
                                 Proc_Pool_ptr);

   -- Wrapper for unchecked deallocation
   procedure Erase_Pool (p : Proc_Pool_ptr);

   -- Return the overall size of the pool
   -- i.e. size = SUM{ size of all procedures }
   function Size (p : Proc_Pool_ptr) return Integer;

   -- Return the overall size of the pool
   -- i.e. size = SUM{ size of all procedures }
   function Size (p : Proc_Pool) return Integer;

   --  Return the number of procedures in the pool
   function Num (p : Proc_Pool_ptr) return Integer;

   --  Return the number of procedures in the pool
   function Num (p : Proc_Pool) return Integer;

   --  Add a procedure to a pool
   procedure Add_Procedure (p : in out Proc_Pool_ptr; proc: Descriptor;
                            size : Integer);

   --  Add a procedure to a pool
   procedure Add_Procedure (p : in out Proc_Pool_ptr; procName: Unbounded_String;
                            size : Integer);

   --  Add a procedure to a pool only iof it is not already included in it
   procedure Add_Procedure_Unique (p : in out Proc_Pool_ptr;
                                   proc: Descriptor;
                                   size : Integer);

   --  Add a procedure to a pool only iof it is not already included in it
   procedure Add_Procedure_Unique (p : in out Proc_Pool_ptr;
                                   procName: Unbounded_String;
                                   size : Integer);

   --  Get the n-th procedure from pool
   function Get_Procedure (p : Proc_Pool_ptr; n : Integer) return Descriptor;

   --  Get the n-th procedure from pool
   function Get_Procedure (p : Proc_Pool; n : Integer) return Descriptor;

   -- Concretely maps a procedure in memory
   procedure Map_This (procName: String; size : Integer);

   -- Used by the LCT approach with local optimizations
   -- Maps the current pool in memory
   procedure Flush_Pool (pool : in out Proc_Pool_ptr);

   -- Static record of procedures
   -- **TO DO** should be initialized according
   --           to static information from the program
   MAPPED_PROCEDURES : Proc_Pool_ptr :=
     new Proc_Pool'(new Procedures (1..2000), 0,0,False);

private

   --  **UNUSED**
   name        : constant String := "RAM";

   --  List of way-sized memory chunks
   binArr      : Bin_Array;

   --  Abstaction of the memory space
   type RAM_Type is
      record
         startAdd    : Integer := 16#40000000#;
         sets        : Bin_Array;
         bins        : Natural;
   end record;

   --  Singleton occurrence of the main Memory
   ramSingleton : RAM_Ptr;

   --  **UNUSED**
   ramCopy : RAM_Ptr;

end RAM;
