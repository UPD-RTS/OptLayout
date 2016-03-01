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
-- FILE NAME      : ram.adb
-- PACKAGE        : RAM body
-- PURPOSE        : Representation of the current RAM memory.
--                  responsible of the actual mapping of procedures
--                  as well as the linker script generation
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
----------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Global, Cache, Utilities;
use Utilities;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions; use Ada.Exceptions;
with WCG_Graph;

package body RAM is



   package T_IO renames Ada.Integer_Text_IO;

   -- Unchecked deallocation of bins
   procedure Free_Bins is new
     Ada.Unchecked_Deallocation ( Bin,
                                 Bin_Ptr);

--     -- Unchecked deallocation of procedures
--     procedure Free_Proc is new
--       Ada.Unchecked_Deallocation (Procedures,
--                                   Contents);

   --new_Component_Start_BinIndex : Integer := 0;


--     procedure Set_New_Start_Bin (i : Integer) is
--     begin
--        new_Component_Start_BinIndex := i;
--     end Set_New_Start_Bin;


   procedure Print_Bins (flag : Boolean) is
      use Utilities;
      flagPrint : Boolean := True;
      curBin : Bin_Ptr;
      procNum : Positive;
      curDesc : Descriptor;
      I : Positive := 1;
      actualRam : RAM_Ptr;
   begin
      if(flag) then
         actualRam := ramSingleton;
      else
         actualRam := ramCopy;
      end if;

      New_Line;
      New_Line;
      Put_Line("*****************************************************************************************************");
      Put_Line("* Procedure placement (per cache ways)                                                              *");
      Put_Line("*****************************************************************************************************");
      while flagPrint loop
         curBin := actualRam.sets (I);
         procNum := curBin.Proc'Length;
         Put ("* WAY " & Integer'Image(curBin.Index) & " (" &
              Integer'Image(procNum) & ")   ");
         T_IO.Put (Base => 16, Item => curBin.StartAdd);
         Put (" :: ");
         T_IO.Put (Base => 16, Item => curBin.EndAdd);
         Put("                                                        *");
         New_Line;
         --Put_Line("procNum: " & );
         for p in 1..procNum loop
            curDesc := curBin.Proc(p);
            Put ("* " & curDesc.name & " S:");-- & Integer'Image(curDesc.size) & " ");
            T_IO.Put (Base => 16, Item=>curDesc.size);
            Put ("   St:");
            T_IO.Put (Base => 16, Item=>curDesc.startAdd);
            Put (" -> ");
            T_IO.Put (Base => 16, Item=>curDesc.startAdd+curDesc.size-1);
            Put ("   Off:");
            T_IO.Put (Base => 16, Item=>curDesc.offset);
            Put_Line("  *");
         end loop;
         Put_Line("*---------------------------------------------------------------------------------------------------*");
         if curBin.State /= FULL then
            flagPrint := false;
         end if;
         I := I + 1;
      end loop;
      Put_Line("*****************************************************************************************************");
      New_Line;
      New_Line;
   end Print_Bins;

   procedure Export_Linker_Script_TAS is
      use Global, Utilities;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      flag : Boolean := True;
      curBin : BIn_Ptr;
      procNum : Positive;
      curDesc : Descriptor;
      prevDesc : Descriptor := Null_Descriptor;
      I : Positive := 1;
      addr : String (1..12);
   begin
--        if (Global.E_M in VERBOSE..MINIMAL) then
--           Put ("Exporting the linker script............");
--        end if;

      Create (Output, Out_File, "optScript.ld");
      Put_line (Output, "/****************************************************************************");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                         GNAT COMPILER COMPONENTS                         *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                                 L E O N                                  *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                            Linker Script File                            *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *");
      Put_line (Output, "*             Copyright (C) 2003-2006 The European Space Agency            *");
      Put_line (Output, "*                   Copyright (C) 2003-2007 AdaCore                        *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* GNAT is free software;  you can  redistribute it  and/or modify it under *");
      Put_line (Output, "* terms of the  GNU General Public License as published  by the Free Soft- *");
      Put_line (Output, "* ware  Foundation;  either version 2,  or (at your option) any later ver- *");
      Put_line (Output, "* sion.  GNAT is distributed in the hope that it will be useful, but WITH- *");
      Put_line (Output, "* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *");
      Put_line (Output, "* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *");
      Put_line (Output, "* for  more details.  You should have  received  a copy of the GNU General *");
      Put_line (Output, "* Public License  distributed with GNAT;  see file COPYING.  If not, write *");
      Put_line (Output, "* to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *");
      Put_line (Output, "* Boston, MA 02110-1301, USA.                                              *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* As a  special  exception,  if you  link  this file  with other  files to *");
      Put_line (Output, "* produce an executable,  this file does not by itself cause the resulting *");
      Put_line (Output, "* executable to be covered by the GNU General Public License. This except- *");
      Put_line (Output, "* ion does not  however invalidate  any other reasons  why the  executable *");
      Put_line (Output, "* file might be covered by the  GNU Public License.                        *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* GNARL was developed by the GNARL team at Florida State University.       *");
      Put_line (Output, "* Extensive contributions were provided by Ada Core Technologies, Inc.     *");
      Put_line (Output, "* The  executive  was developed  by the  Real-Time  Systems  Group  at the *");
      Put_line (Output, "* Technical University of Madrid.                                          *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "****************************************************************************/");
      Put_line (Output, "  ");
      Put_line (Output, "/* This is a LEON specific version of this file */");
      Put_line (Output, "  ");
      Put_line (Output, "/* This script replaces ld's default linker script, providing the");
      Put_line (Output, "appropriate LEON memory map and output format. */");
      Put_line (Output, "  ");
      Put_line (Output, "/*");
      Put_line (Output, "* Uncomment this if you want the linker to output srecords.");
      Put_line (Output, "OUTPUT_FORMAT(srec)");
      Put_line (Output, "* ");
      Put_line (Output, "*/");
      Put_line (Output, "OUTPUT_ARCH(sparc)");
      Put_line (Output, " ");
      Put_line (Output, "__DYNAMIC  =  0;");
      Put_line (Output, " ");
      Put_line (Output, "ENTRY(obsw_entry_point)");
      Put_line (Output, "obsw_entry_point = trap_table;");
      Put_line (Output, " ");
      Put_line (Output, "galmon_print = 0x00001844;");
      Put_line (Output, " ");
      Put_line (Output, "/*");
      Put_line (Output, " * The memory map looks like this:");
      Put_line (Output, " * +--------------------+ <- low memory");
      Put_line (Output, " * | .text              |");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * | .data              | initialized data goes here");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * | .bss               |");
      Put_line (Output, " * |        __bss_start | start of bss, cleared by crt0");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * |        _stack_start|");
      Put_line (Output, " * |    stack space     |");
      Put_line (Output, " * |        __stack     | top of stack");
      Put_line (Output, " * |        _end        | start of heap, used by sbrk()");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * |    heap space      |");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * |  remote monitor    |");
      Put_line (Output, " * +--------------------+ <- high memory");
      Put_line (Output, " */");
      Put_line (Output, " ");
      Put_line (Output, "/*");
      Put_line (Output, " * User modifiable values:");
      Put_line (Output, " *");
      Put_line (Output, " * _PROM_SIZE                 size of PROM (permissible values are 4K, 8K, 16K");
      Put_line (Output, " *                                 32K, 64K, 128K, 256K, and 512K)");
      Put_line (Output, " * _RAM_SIZE                  size of RAM  (permissible values are 256K, 512K,");
      Put_line (Output, " *                                 1MB, 2Mb, 4Mb, 8Mb, 16Mb, and 32Mb)");
      Put_line (Output, " *");
      Put_line (Output, " * _STACK_SIZE                size of the stack to be used by the main");
      Put_line (Output, " *                            procedure (environment task)");
      Put_line (Output, " *");
      Put_Line (Output, " * _REM_MON_SIZE              space reserved for the remote monitor");
      Put_Line (Output, " *");
      Put_Line (Output, " * _CLOCK_SPEED               system clock frequency in Hz");
      Put_Line (Output, " *");
      Put_Line (Output, " *");
      Put_Line (Output, " * These symbols are only used in assembler code, so they only need to");
      Put_Line (Output, " * be listed once. They should always be refered to without SYM().");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "_PROM_SIZE = 128K;");
      Put_Line (Output, "_RAM_SIZE = 4M;");
      Put_Line (Output, "_REM_MON_SIZE = 0K;");
      Put_Line (Output, " ");
      Put_Line (Output, "_RAM_START = 0x40000000;");
      Put_Line (Output, "_RAM_END = _RAM_START + _RAM_SIZE - _REM_MON_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "_PROM_START = 0x00000000;");
      Put_Line (Output, "_PROM_END = _PROM_START + _PROM_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "_STACK_SIZE = (20 * 1024);");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, "MEMORY");
      Put_Line (Output, "{");
      Put_Line (Output, "rom    : ORIGIN = 0x00000000, LENGTH = 128K");
      Put_Line (Output, "ram    : ORIGIN = 0x40000000, LENGTH = 4M");
      Put_Line (Output, "eeprom : ORIGIN = 0x20000000, LENGTH = 2M");
      Put_Line (Output, "}");
      Put_Line (Output, "*/");
      Put_Line (Output, " ");
      Put_Line (Output, "SECTIONS");
      Put_Line (Output, "{");
      Put_Line (Output, " ");
      Put_Line (Output, "   .Z_CONF_OBSWRR 0x40020000 :");
      Put_Line (Output, "   { ");
      Put_Line (Output, "    __BEGIN_PROTECTED_MEMORY_AREA__ = . ;");
      Put_Line (Output, "    __OBSWRR_CONF_DST__ = . ;");
      Put_Line (Output, "    LONG(0x10000020)                              /*OBSWRR_ADDR_SRC*/");
      Put_Line (Output, "    LONG(ABSOLUTE(__OBSWRR_CONF_DST__) + 0x20)    /*OBSWRR_ADDR_DST*/");
      Put_Line (Output, "    LONG(ABSOLUTE(__ENDAPPLI__) - ABSOLUTE(__OBSWRR_CONF_DST__) - 0x20) /*OBSWRR_SIZ*/");
      Put_Line (Output, "    LONG(0xFFFFFFFF)                              /*OBSWRR_CRC*/");
      Put_Line (Output, "    LONG(ABSOLUTE(obsw_entry_point))                     /*OBSWRR_ADDR_EXE*/");
      Put_Line (Output, "    LONG(ABSOLUTE(__OBSWRR_CONF_DST__))           /*OBSWRR_CONF_DST*/");
      Put_Line (Output, "    LONG(0xADADADAD)                              /*Reserved*/");
      Put_Line (Output, "    LONG(0x00000000)                              /*Z_BOOT_TEST_ADR*/");
      Put_Line (Output, "   }");
      Put_Line (Output, " ");
--      Put_Line (Output, "  .text._my_section 0x40100000 : {");
      Put_Line (Output, "   .text.optimizedLayout 0x40020020 :");
      Put_Line (Output, "   { ");
    --  Put_Line (Output, "      stext = . ;");
    --  Put_Line (Output, "      _stext = .;");
      while flag loop
         curBin := ramSingleton.sets (I);
         procNum := curBin.Proc'Length;
         --Put ("* BIN " & Integer'Image(curBin.Index) & " (" &
         --     Integer'Image(procNum) & ")    ");
         --T_IO.Put (Base => 16, Item => curBin.StartAdd);
         --T_IO.Put (Base => 16, Item => curBin.EndAdd);
         --Put(" * ");
         --Put_Line("procNum: " & );
         for p in 1..procNum loop
            curDesc := curBin.Proc(p);
            if (not (curDesc=prevDesc)) then
--                 .text._ada_main 0x40100000 : {
--                 *(.text.ops__myop)
--                   *(.text._ada_main)
--                   } > ram
               if (Trim(curDesc.name) = Trim(PLACE_HOLDER)) then
                  Put (Output, "    . = . + 0x");
                  T_IO.Put (To   => addr,
                            Item => curDesc.size,
                            Base => 16);
                  Put_line (Output, addr(First_Index(addr,'#')+1..11) & ";");

                  if(Global.E_M = DEBUG) then
                     Put_line ("PLACE_HOLDER" & " . = . + 0x"
                               & addr(First_Index(addr,'#')+1..11) &    ";");
                  end if;

               else
                  Put_Line (Output, "    *(.text." & Trim(curDesc.name) &")");
               end if;

               prevDesc := curDesc;
            end if;
            --Put ( Trim(curDesc.name) & " ");-- & Integer'Image(curDesc.size) & " ");
            --T_IO.Put (Base => 16, Item=>curDesc.size);
            --Put (" ");
         end loop;
         --Put_Line ("*");
         --Put_Line("-------------------------------------");
         if curBin.State /= FULL then
            flag := false;
         end if;
         I := I + 1;
      end loop;
      Put_Line (Output, "    . = ALIGN(0x1000); ");
      Put_Line (Output, "   } ");
      Put_Line (Output, " ");
      Put_Line (Output, "   .text . :");
      Put_Line (Output, "   { ");
      Put_Line (Output, "      stext = . ;");
      Put_Line (Output, "      _stext = .;");
      Put_Line (Output, "      *(EXCLUDE_FILE(vm_mgt-c-*.o) .text)");
      Put_Line (Output, "      *(EXCLUDE_FILE(vm_mgt-c-*.o) .text.*)");
      Put_Line (Output, "      _etext  =  .;");
      Put_Line (Output, " ");
      Put_Line (Output, "      __CTOR_LIST__ = .;");
      Put_Line (Output, "      LONG((__CTOR_END__ - __CTOR_LIST__) / 4 - 2)");
      Put_Line (Output, "      *(.ctors)");
      Put_Line (Output, "      LONG(0)");
      Put_Line (Output, "      __CTOR_END__ = .;");
      Put_Line (Output, "      __DTOR_LIST__ = .;");
      Put_Line (Output, "      LONG((__DTOR_END__ - __DTOR_LIST__) / 4 - 2)");
      Put_Line (Output, "      *(.dtors)");
      Put_Line (Output, "      LONG(0)");
      Put_Line (Output, "      __DTOR_END__ = .;");
      Put_Line (Output, " ");
      Put_Line (Output, "      *(.jcr)");
      Put_Line (Output, "      *(.init)");
      Put_Line (Output, "      *(.fini)");
      Put_Line (Output, "      *(.lit)");
      Put_Line (Output, "      *(EXCLUDE_FILE(vm_mgt-c-*.o) .rodata)");
      Put_Line (Output, "      *(EXCLUDE_FILE(vm_mgt-c-*.o) .rodata.*)");
      Put_Line (Output, "      *(.shdata)");
      Put_Line (Output, "      *(.data1)");
      Put_Line (Output, "      __EH_FRAME_BEGIN__ = .;");
      Put_Line (Output, "      *(.eh_frame)");
      Put_Line (Output, "      *(.gnu.linkonce.t*)");
      Put_Line (Output, "      *(.gnu.linkonce.r*)");
      Put_Line (Output, "      *(.gcc_except_table)");
      Put_Line (Output, " ");
      Put_Line (Output, "      vm_mgt-c-*.o(.text .text.* .rodata .rodata.*)");
      Put_Line (Output, " ");
      Put_Line (Output, "      . = ALIGN (0x4);");
      Put_Line (Output, "      __END_PROTECTED_MEMORY_AREA__ = . ;");
      Put_Line (Output, " ");
      Put_Line (Output, "      vm_mgt*.o(.data)");
      Put_Line (Output, "  ");
      Put_Line (Output, "      . = ALIGN(0x4);");
      Put_Line (Output, "      _endtext = .;");
      Put_Line (Output, "   }");
      Put_Line (Output, " ");
      Put_Line (Output, "   /* Align the next 4 kBytes in order to modify addresses as less as possible */");
      Put_Line (Output, "   .bss_autotest ALIGN(0x1000) (NOLOAD) :");
      Put_Line (Output, "   {      ");
      Put_Line (Output, "      vm_mgt-c-*.o(COMMON)");
      Put_Line (Output, "      vm_mgt-c-*.o(.bss)");
      Put_Line (Output, "      . = ALIGN(0x1000);");
      Put_Line (Output, "      *(at_data_fix_addr);");
      Put_Line (Output, "   }");
      Put_Line (Output, " ");
      Put_Line (Output, "   .data 0x401F3000 :");
      Put_Line (Output, "   {");
      Put_Line (Output, "      sdata = . ;");
      Put_Line (Output, "      _sdata = . ;");
      Put_Line (Output, "      *(.TRAPS)");
      Put_Line (Output, "      *(.data)");
      Put_Line (Output, "      *(.data.*)");
      Put_Line (Output, "      *(.gnu.linkonce.d*)");
      Put_Line (Output, " ");
      Put_Line (Output, "      /* due to BOOT prom: length of the OBSW shall be 8-bytes multiple */");
      Put_Line (Output, " ");
      Put_Line (Output, "      . = ALIGN (0x8);");
      Put_Line (Output, "      edata = . ;");
      Put_Line (Output, "      _edata = . ;");
      Put_Line (Output, "      __ENDAPPLI__ = . ;");
      Put_Line (Output, "   }");
      Put_Line (Output, " ");
      Put_Line (Output, "   .bss ALIGN (0x8) (NOLOAD) :");
      Put_Line (Output, "   {");
      Put_Line (Output, "      _sbss = . ;");
      Put_Line (Output, "      __bss_start = .; /* for initialization of bss section */");
      Put_Line (Output, "      *(.bss)");
      Put_Line (Output, "      *(COMMON)");
      Put_Line (Output, " ");
      Put_Line (Output, "      /* Align the stack to 64 bits */");
      Put_Line (Output, " ");
      Put_Line (Output, "      . = ALIGN(0x8);");
      Put_Line (Output, "      _stack_start = .;");
      Put_Line (Output, " ");
      Put_Line (Output, "      /* Reserve the space for the stack to be used by the environment task */");
      Put_Line (Output, " ");
      Put_Line (Output, "      . += _STACK_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "      /* Pointer to the top of the stack to be used by the main procedure (the");
      Put_Line (Output, "         environment task. */");
      Put_Line (Output, " ");
      Put_Line (Output, "      __stack = ALIGN(0x8);");
      Put_Line (Output, " ");
      Put_Line (Output, " ");
      Put_Line (Output, "      end = ALIGN(0x8);");
      Put_Line (Output, "      _end = ALIGN(0x8);");
      Put_Line (Output, "      __end = ALIGN(0x8);");
      Put_Line (Output, " ");
      Put_Line (Output, "      ebss = . ;");
      Put_Line (Output, "      _ebss = . ;");
      Put_Line (Output, "   }");
      Put_Line (Output, " ");
      Put_Line (Output, "   sizeoftext  = _etext  - _stext ;");
      Put_Line (Output, "   sizeofdata  = _edata  - _sdata ;");
      Put_Line (Output, "   sizeofbss   = _ebss   - _sbss ;");
      Put_Line (Output, " ");
      Put_Line (Output, "   _datacopy ALIGN (0x1000) :");
      Put_Line (Output, "   {");
      Put_Line (Output, "   _initdata = . ;");
      Put_Line (Output, "   _end = . ;");
      Put_Line (Output, "  }");
      Put_Line (Output, "  .stab  0 (NOLOAD) :");
      Put_Line (Output, "  {");
      Put_Line (Output, "    [ .stab ]");
      Put_Line (Output, "  }");
      Put_Line (Output, "  .stabstr  0 (NOLOAD) :");
      Put_Line (Output, "  {");
      Put_Line (Output, "    [ .stabstr ]");
      Put_Line (Output, "  }");
      Put_Line (Output, " ");
      Put_Line (Output, "  PROVIDE (end = .);");
      Put_Line (Output, "  /* DWARF debug sections.");
      Put_Line (Output, "     Symbols in the DWARF debugging sections are relative to the beginning");
      Put_Line (Output, "     of the section so we begin them at 0.  */");
      Put_Line (Output, "  /* DWARF 1 */");
      Put_Line (Output, "  .debug          0 : { *(.debug) }");
      Put_Line (Output, "  .line           0 : { *(.line) }");
      Put_Line (Output, "  /* GNU DWARF 1 extensions */");
      Put_Line (Output, "  .debug_srcinfo  0 : { *(.debug_srcinfo) }");
      Put_Line (Output, "  .debug_sfnames  0 : { *(.debug_sfnames) }");
      Put_Line (Output, "  /* DWARF 1.1 and DWARF 2 */");
      Put_Line (Output, "  .debug_aranges  0 : { *(.debug_aranges) }");
      Put_Line (Output, "  .debug_pubnames 0 : { *(.debug_pubnames) }");
      Put_Line (Output, "  /* DWARF 2 */");
      Put_Line (Output, "  .debug_info     0 : { *(.debug_info) }");
      Put_Line (Output, "  .debug_abbrev   0 : { *(.debug_abbrev) }");
      Put_Line (Output, "  .debug_line     0 : { *(.debug_line) }");
      Put_Line (Output, "  .debug_frame    0 : { *(.debug_frame) }");
      Put_Line (Output, "  .debug_str      0 : { *(.debug_str) }");
      Put_Line (Output, "  .debug_loc      0 : { *(.debug_loc) }");
      Put_Line (Output, "  .debug_macinfo  0 : { *(.debug_macinfo) }");
      Put_Line (Output, "  /* SGI/MIPS DWARF 2 extensions */");
      Put_Line (Output, "  .debug_weaknames 0 : { *(.debug_weaknames) }");
      Put_Line (Output, "  .debug_funcnames 0 : { *(.debug_funcnames) }");
      Put_Line (Output, "  .debug_typenames 0 : { *(.debug_typenames) }");
      Put_Line (Output, "  .debug_varnames  0 : { *(.debug_varnames) }");
      Put_Line (Output, " ");
      Put_Line (Output, "} ");
      Put_Line (Output, " ");
      Put_Line (Output, " ");
      Put_Line (Output, "/* Set the values that define the memory map */");
      Put_Line (Output, " ");
      Put_Line (Output, "rom_start = _PROM_START;");
      Put_Line (Output, "rom_size = _PROM_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "ram_start = _RAM_START;");
      Put_Line (Output, "ram_size = _RAM_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "heap_start = ebss;");
      Put_Line (Output, "heap_end = _RAM_END;");
      Put_Line (Output, " ");
      Put_Line (Output, "/* END Linker script  */");

      Close (Output);
      --if (Global.E_M in VERBOSE..MINIMAL) then
      --   Put_Line (" OK");
      --end if;
   end Export_Linker_Script_TAS;



   procedure Export_Linker_Script  is
      use Global, Utilities;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      flag : Boolean := True;
      curBin : BIn_Ptr;
      procNum : Positive;
      curDesc : Descriptor;
      prevDesc : Descriptor := Null_Descriptor;
      I : Positive := 1;
      addr : String (1..12);
   begin
--        if (Global.E_M in VERBOSE..MINIMAL) then
--           Put ("Exporting the linker script............");
--        end if;
      Create (Output, Out_File, "optScript.ld");
      Put_line (Output, "/****************************************************************************");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                         GNAT COMPILER COMPONENTS                         *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                                 L E O N                                  *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                            Linker Script File                            *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *");
      Put_line (Output, "*             Copyright (C) 2003-2006 The European Space Agency            *");
      Put_line (Output, "*                   Copyright (C) 2003-2007 AdaCore                        *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* GNAT is free software;  you can  redistribute it  and/or modify it under *");
      Put_line (Output, "* terms of the  GNU General Public License as published  by the Free Soft- *");
      Put_line (Output, "* ware  Foundation;  either version 2,  or (at your option) any later ver- *");
      Put_line (Output, "* sion.  GNAT is distributed in the hope that it will be useful, but WITH- *");
      Put_line (Output, "* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *");
      Put_line (Output, "* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *");
      Put_line (Output, "* for  more details.  You should have  received  a copy of the GNU General *");
      Put_line (Output, "* Public License  distributed with GNAT;  see file COPYING.  If not, write *");
      Put_line (Output, "* to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *");
      Put_line (Output, "* Boston, MA 02110-1301, USA.                                              *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* As a  special  exception,  if you  link  this file  with other  files to *");
      Put_line (Output, "* produce an executable,  this file does not by itself cause the resulting *");
      Put_line (Output, "* executable to be covered by the GNU General Public License. This except- *");
      Put_line (Output, "* ion does not  however invalidate  any other reasons  why the  executable *");
      Put_line (Output, "* file might be covered by the  GNU Public License.                        *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* GNARL was developed by the GNARL team at Florida State University.       *");
      Put_line (Output, "* Extensive contributions were provided by Ada Core Technologies, Inc.     *");
      Put_line (Output, "* The  executive  was developed  by the  Real-Time  Systems  Group  at the *");
      Put_line (Output, "* Technical University of Madrid.                                          *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "****************************************************************************/");
      Put_line (Output, "  ");
      Put_line (Output, "/* This is a LEON specific version of this file */");
      Put_line (Output, "  ");
      Put_line (Output, "/* This script replaces ld's default linker script, providing the");
      Put_line (Output, "appropriate LEON memory map and output format. */");
      Put_line (Output, "  ");
      Put_line (Output, "/*");
      Put_line (Output, "* Uncomment this if you want the linker to output srecords.");
      Put_line (Output, "OUTPUT_FORMAT(srec)");
      Put_line (Output, "* ");
      Put_line (Output, "*/");
      Put_line (Output, "OUTPUT_ARCH(sparc)");
      Put_line (Output, "ENTRY(start)");
      Put_line (Output, "SEARCH_DIR(.)");
      Put_line (Output, "__DYNAMIC  =  0;");
      Put_line (Output, "/*");
      Put_line (Output, " * The memory map looks like this:");
      Put_line (Output, " * +--------------------+ <- low memory");
      Put_line (Output, " * | .text              |");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * | .data              | initialized data goes here");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * | .bss               |");
      Put_line (Output, " * |        __bss_start | start of bss, cleared by crt0");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * |        _stack_start|");
      Put_line (Output, " * |    stack space     |");
      Put_line (Output, " * |        __stack     | top of stack");
      Put_line (Output, " * |        _end        | start of heap, used by sbrk()");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * |    heap space      |");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * |  remote monitor    |");
      Put_line (Output, " * +--------------------+ <- high memory");
      Put_line (Output, " */");
      Put_line (Output, " ");
      Put_line (Output, "/*");
      Put_line (Output, " * User modifiable values:");
      Put_line (Output, " *");
      Put_line (Output, " * _PROM_SIZE                 size of PROM");
      Put_line (Output, " *");
      Put_line (Output, " * _RAM_SIZE                  size of RAM");
      Put_line (Output, " *");
      Put_line (Output, " * _STACK_SIZE                size of the stack to be used by the main");
      Put_line (Output, " *                            procedure (environment task)");
      Put_line (Output, " *");
      Put_Line (Output, " * _REM_MON_SIZE              space reserved for the remote monitor");
      Put_Line (Output, " *");
      Put_Line (Output, " * _CLOCK_SPEED               system clock frequency in Hz");
      Put_Line (Output, " *");
      Put_Line (Output, " * These symbols are only used in assembler code, so they only need to");
      Put_Line (Output, " * be listed once. They should always be refered to without SYM().");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "_PROM_SIZE = 128K;");
      Put_Line (Output, "_RAM_SIZE = 64M;");
      Put_Line (Output, "_REM_MON_SIZE = 32K;");
      Put_Line (Output, " ");
      Put_Line (Output, "_RAM_START = 0x40000000;");
      Put_Line (Output, "_RAM_END = _RAM_START + _RAM_SIZE - _REM_MON_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "_PROM_START = 0x00000000;");
      Put_Line (Output, "_PROM_END = _PROM_START + _PROM_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "_STACK_SIZE = (40 * 1024);");
      Put_Line (Output, " ");
      Put_Line (Output, "_CLOCK_SPEED = 50000000; /* in Hz */");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, " *  Base address of the on-CPU peripherals");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "LEON_REG = 0x80000000;");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, " * Setup the memory map for the SIS simulator.");
      Put_Line (Output, " * stack grows up towards low memory.");
      Put_Line (Output, " */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "MEMORY");
      Put_Line (Output, "{");
      Put_Line (Output, "  /* rom (rx)  : ORIGIN = 0x00000000, LENGTH = 128K */");
      Put_Line (Output, "  /* The highest 32K of memory are reserved for the remote monitor */");
      Put_Line (Output, "  ram (rwx) : ORIGIN = 0x40000000, LENGTH = 64M - 32K");
      Put_Line (Output, "}");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, " * All the symbols that might be accessed from C code need to be");
      Put_Line (Output, " * listed twice, once with an additional underscore. aout format needs");
      Put_Line (Output, " * and extra underscore, whereas coff & elf doesn't. This is to work");
      Put_Line (Output, " * with both.");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "SECTIONS");
      Put_Line (Output, "{");
      Put_Line (Output, " ");
      Put (Output, "  .text 0x");
      -- S(4 .. S'Length - 1)
      T_IO.Put (To   => addr,
                Item => ramSingleton.startAdd,
                Base => 16);
      --T_IO.Put (File => Output,
      --          Base => 16,
      --          Item => ramSingleton.startAdd);
      Put_line (Output, addr(4..11) & " : {");
--      Put_Line (Output, "  .text 0x40000000 : {");
      Put_Line (Output, "    /*KEEP (*(.text))*/");
      --Put_Line (Output, "    /* *(.text._trap_table)*/");
      --Put_Line (Output, "    *(.text._ada_main)");

      while flag loop
         curBin := ramSingleton.sets (I);
         procNum := curBin.Proc'Length;
         --Put ("* BIN " & Integer'Image(curBin.Index) & " (" &
         --     Integer'Image(procNum) & ")    ");
         --T_IO.Put (Base => 16, Item => curBin.StartAdd);
         --T_IO.Put (Base => 16, Item => curBin.EndAdd);
         --Put(" * ");
         --Put_Line("procNum: " & );
         for p in 1..procNum loop
            curDesc := curBin.Proc(p);
            if (not (curDesc=prevDesc)) then
               Put_Line (Output, "    *(.text." & Trim(curDesc.name) &")");
               prevDesc := curDesc;
            end if;
            --Put ( Trim(curDesc.name) & " ");-- & Integer'Image(curDesc.size) & " ");
            --T_IO.Put (Base => 16, Item=>curDesc.size);
            --Put (" ");
         end loop;
         --Put_Line ("*");
         --Put_Line("-------------------------------------");
         if curBin.State /= FULL then
            flag := false;
         end if;
         I := I + 1;
      end loop;

      Put_Line (Output, "    *(.text.*)");
      Put_Line (Output, "    *(.lit)");
      Put_Line (Output, "    *(.rodata)");
      Put_Line (Output, "    *(.rodata.*)");
      Put_Line (Output, "    *(.shdata)");
      Put_Line (Output, "    *(.eh_frame)");
      Put_Line (Output, "    *(.gnu.linkonce.t*)");
      Put_Line (Output, "    *(.gnu.linkonce.r*)");
      Put_Line (Output, "    *(.text.ops__myop)");
      Put_Line (Output, "    *(.text.adafinal)");
      Put_Line (Output, "    *(.text.adainit)");
      Put_Line (Output, "  }  > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .shbss : {");
      Put_Line (Output, "    *(.shbss)");
      Put_Line (Output, "  } > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .talias : { }  > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .data : {");
      Put_Line (Output, "    /* Make the clock frequency defined in this script available to the run");
      Put_Line (Output, "       time via this clock_frequency variable. */");
      Put_Line (Output, "    clock_frequency = .;");
      Put_Line (Output, "    LONG(_CLOCK_SPEED)");
      Put_Line (Output, "    *(.data)");
      Put_Line (Output, "    *(.data.*)");
      Put_Line (Output, "    *(.gnu.linkonce.d*)");
      Put_Line (Output, " ");
      Put_Line (Output, "    /* Ensure that the end of the data section is always word aligned.");
      Put_Line (Output, "       Initial values are stored in 4-bytes blocks so we must guarantee");
      Put_Line (Output, "       that these blocks do not fall out the section (otherwise they are");
      Put_Line (Output, "       truncated and the initial data for the last block are lost). */");
      Put_Line (Output, " ");
      Put_Line (Output, "    . = ALIGN(0x4);");
      Put_Line (Output, "  } > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .bss : {");
      Put_Line (Output, "   __bss_start = ALIGN(0x8);");
      Put_Line (Output, "   *(.bss)");
      Put_Line (Output, "   *(.bss.*)");
      Put_Line (Output, "   *(COMMON)");
      Put_Line (Output, " ");
      Put_Line (Output, "  /* Align the stack to 64 bits */");
      Put_Line (Output, " ");
      Put_Line (Output, "   _stack_start = ALIGN(0x8);");
      Put_Line (Output, " ");
      Put_Line (Output, "   /* Reserve the space for the stack to be used by the environment task */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "   . += _STACK_SIZE; ");
      Put_Line (Output, " ");
      Put_Line (Output, "   /* Pointer to the top of the stack to be used by the main procedure (the ");
      Put_Line (Output, "      environment task. */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "   __stack = ALIGN(0x8); ");
      Put_Line (Output, " ");
      Put_Line (Output, "   _end = ALIGN(0x8); ");
      Put_Line (Output, "  } > ram ");
      Put_Line (Output, " ");
      Put_Line (Output, "  /* Stabs debugging sections.  */ ");
      Put_Line (Output, "  .stab          0 : { *(.stab) } ");
      Put_Line (Output, "  .stabstr       0 : { *(.stabstr) } ");
      Put_Line (Output, "  .stab.excl     0 : { *(.stab.excl) } ");
      Put_Line (Output, "  .stab.exclstr  0 : { *(.stab.exclstr) } ");
      Put_Line (Output, "  .stab.index    0 : { *(.stab.index) } ");
      Put_Line (Output, "  .stab.indexstr 0 : { *(.stab.indexstr) } ");
      Put_Line (Output, "  .comment       0 : { *(.comment) } ");
      Put_Line (Output, "  /* DWARF debug sections. ");
      Put_Line (Output, "     Symbols in the DWARF debugging sections are relative to the beginning ");
      Put_Line (Output, "     of the section so we begin them at 0.  */ ");
      Put_Line (Output, "  /* DWARF 1 */ ");
      Put_Line (Output, "  .debug          0 : { *(.debug) } ");
      Put_Line (Output, "  .line           0 : { *(.line) } ");
      Put_Line (Output, "  /* GNU DWARF 1 extensions */ ");
      Put_Line (Output, "  .debug_srcinfo  0 : { *(.debug_srcinfo) } ");
      Put_Line (Output, "  .debug_sfnames  0 : { *(.debug_sfnames) } ");
      Put_Line (Output, "  /* DWARF 1.1 and DWARF 2 */ ");
      Put_Line (Output, "  .debug_aranges  0 : { *(.debug_aranges) } ");
      Put_Line (Output, "  .debug_pubnames 0 : { *(.debug_pubnames) } ");
      Put_Line (Output, "  /* DWARF 2 */ ");
      Put_Line (Output, "  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) } ");
      Put_Line (Output, "  .debug_abbrev   0 : { *(.debug_abbrev) } ");
      Put_Line (Output, "  .debug_line     0 : { *(.debug_line) } ");
      Put_Line (Output, "  .debug_frame    0 : { *(.debug_frame) } ");
      Put_Line (Output, "  .debug_str      0 : { *(.debug_str) } ");
      Put_Line (Output, "  .debug_loc      0 : { *(.debug_loc) } ");
      Put_Line (Output, "  .debug_macinfo  0 : { *(.debug_macinfo) } ");
      Put_Line (Output, "  /* SGI/MIPS DWARF 2 extensions */ ");
      Put_Line (Output, "  .debug_weaknames 0 : { *(.debug_weaknames) } ");
      Put_Line (Output, "  .debug_funcnames 0 : { *(.debug_funcnames) } ");
      Put_Line (Output, "  .debug_typenames 0 : { *(.debug_typenames) } ");
      Put_Line (Output, "  .debug_varnames  0 : { *(.debug_varnames) } ");
      Put_Line (Output, "  /* DWARF 3 */ ");
      Put_Line (Output, "  .debug_pubtypes 0 : { *(.debug_pubtypes) } ");
      Put_Line (Output, "  .debug_ranges   0 : { *(.debug_ranges) } ");
      Put_Line (Output, "  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) } ");
      Put_Line (Output, "  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) } ");
      Put_Line (Output, "} ");
      Put_Line (Output, " ");
      Put_Line (Output, "/* Set the values that define the memory map */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "rom_start = _PROM_START; ");
      Put_Line (Output, "rom_size = _PROM_SIZE; ");
      Put_Line (Output, " ");
      Put_Line (Output, "ram_start = _RAM_START; ");
      Put_Line (Output, "ram_size = _RAM_SIZE; ");
      Put_Line (Output, " ");
      Put_Line (Output, "heap_start = _end; ");
      Put_Line (Output, "heap_end = _RAM_END; ");
      Put_Line (Output, " ");
      Put_Line (Output, "/* END Linker script  */");

      Close (Output);
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put_Line ("  Linker script generated");
      end if;

   end Export_Linker_Script;


   procedure Export_Linker_Script_Tsim  is
      use Global, Utilities;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      flag : Boolean := True;
      curBin : BIn_Ptr;
      procNum : Positive;
      curDesc : Descriptor;
      prevDesc : Descriptor := Null_Descriptor;
      I : Positive := 1;
      addr : String (1..12);
   begin
--        if (Global.E_M in VERBOSE..MINIMAL) then
--           Put ("Exporting the linker script............");
--        end if;
      Create (Output, Out_File, "optScript.ld");
      Put_line (Output, "/****************************************************************************");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                         GNAT COMPILER COMPONENTS                         *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                                 L E O N                                  *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                            Linker Script File                            *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *");
      Put_line (Output, "*             Copyright (C) 2003-2006 The European Space Agency            *");
      Put_line (Output, "*                   Copyright (C) 2003-2007 AdaCore                        *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* GNAT is free software;  you can  redistribute it  and/or modify it under *");
      Put_line (Output, "* terms of the  GNU General Public License as published  by the Free Soft- *");
      Put_line (Output, "* ware  Foundation;  either version 2,  or (at your option) any later ver- *");
      Put_line (Output, "* sion.  GNAT is distributed in the hope that it will be useful, but WITH- *");
      Put_line (Output, "* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *");
      Put_line (Output, "* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *");
      Put_line (Output, "* for  more details.  You should have  received  a copy of the GNU General *");
      Put_line (Output, "* Public License  distributed with GNAT;  see file COPYING.  If not, write *");
      Put_line (Output, "* to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *");
      Put_line (Output, "* Boston, MA 02110-1301, USA.                                              *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* As a  special  exception,  if you  link  this file  with other  files to *");
      Put_line (Output, "* produce an executable,  this file does not by itself cause the resulting *");
      Put_line (Output, "* executable to be covered by the GNU General Public License. This except- *");
      Put_line (Output, "* ion does not  however invalidate  any other reasons  why the  executable *");
      Put_line (Output, "* file might be covered by the  GNU Public License.                        *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* GNARL was developed by the GNARL team at Florida State University.       *");
      Put_line (Output, "* Extensive contributions were provided by Ada Core Technologies, Inc.     *");
      Put_line (Output, "* The  executive  was developed  by the  Real-Time  Systems  Group  at the *");
      Put_line (Output, "* Technical University of Madrid.                                          *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "****************************************************************************/");
      Put_line (Output, "  ");
      Put_line (Output, "/* This is a LEON specific version of this file */");
      Put_line (Output, "  ");
      Put_line (Output, "/* This script replaces ld's default linker script, providing the");
      Put_line (Output, "appropriate LEON memory map and output format. */");
      Put_line (Output, "  ");
      Put_line (Output, "/*");
      Put_line (Output, "* Uncomment this if you want the linker to output srecords.");
      Put_line (Output, "OUTPUT_FORMAT(srec)");
      Put_line (Output, "* ");
      Put_line (Output, "*/");
      Put_line (Output, "OUTPUT_ARCH(sparc)");
      Put_line (Output, "ENTRY(start)");
      Put_line (Output, "SEARCH_DIR(.)");
      Put_line (Output, "__DYNAMIC  =  0;");
      Put_line (Output, "/*");
      Put_line (Output, " * The memory map looks like this:");
      Put_line (Output, " * +--------------------+ <- low memory");
      Put_line (Output, " * | .text              |");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * | .data              | initialized data goes here");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * | .bss               |");
      Put_line (Output, " * |        __bss_start | start of bss, cleared by crt0");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * |        _stack_start|");
      Put_line (Output, " * |    stack space     |");
      Put_line (Output, " * |        __stack     | top of stack");
      Put_line (Output, " * |        _end        | start of heap, used by sbrk()");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * |    heap space      |");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * |  remote monitor    |");
      Put_line (Output, " * +--------------------+ <- high memory");
      Put_line (Output, " */");
      Put_line (Output, " ");
      Put_line (Output, "/*");
      Put_line (Output, " * User modifiable values:");
      Put_line (Output, " *");
      Put_line (Output, " * _PROM_SIZE                 size of PROM");
      Put_line (Output, " *");
      Put_line (Output, " * _RAM_SIZE                  size of RAM");
      Put_line (Output, " *");
      Put_line (Output, " * _STACK_SIZE                size of the stack to be used by the main");
      Put_line (Output, " *                            procedure (environment task)");
      Put_line (Output, " *");
      Put_Line (Output, " * _REM_MON_SIZE              space reserved for the remote monitor");
      Put_Line (Output, " *");
      Put_Line (Output, " * _CLOCK_SPEED               system clock frequency in Hz");
      Put_Line (Output, " *");
      Put_Line (Output, " * These symbols are only used in assembler code, so they only need to");
      Put_Line (Output, " * be listed once. They should always be refered to without SYM().");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "_PROM_SIZE = 128K;");
      Put_Line (Output, "_RAM_SIZE = 64M;");
      Put_Line (Output, "_REM_MON_SIZE = 32K;");
      Put_Line (Output, " ");
      Put_Line (Output, "_RAM_START = 0x40000000;");
      Put_Line (Output, "_RAM_END = _RAM_START + _RAM_SIZE - _REM_MON_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "_PROM_START = 0x00000000;");
      Put_Line (Output, "_PROM_END = _PROM_START + _PROM_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "_STACK_SIZE = (40 * 1024);");
      Put_Line (Output, " ");
      Put_Line (Output, "_CLOCK_SPEED = 50000000; /* in Hz */");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, " *  Base address of the on-CPU peripherals");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "LEON_REG = 0x80000000;");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, " * Setup the memory map for the SIS simulator.");
      Put_Line (Output, " * stack grows up towards low memory.");
      Put_Line (Output, " */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "MEMORY");
      Put_Line (Output, "{");
      Put_Line (Output, "  /* rom (rx)  : ORIGIN = 0x00000000, LENGTH = 128K */");
      Put_Line (Output, "  /* The highest 32K of memory are reserved for the remote monitor */");
      Put_Line (Output, "  ram (rwx) : ORIGIN = 0x40000000, LENGTH = 64M - 32K");
      Put_Line (Output, "}");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, " * All the symbols that might be accessed from C code need to be");
      Put_Line (Output, " * listed twice, once with an additional underscore. aout format needs");
      Put_Line (Output, " * and extra underscore, whereas coff & elf doesn't. This is to work");
      Put_Line (Output, " * with both.");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "SECTIONS");
      Put_Line (Output, "{");
      Put_Line (Output, " ");

      Put_Line (Output, "  .text._my_section 0x40100000 : {");
      while flag loop
         curBin := ramSingleton.sets (I);
         procNum := curBin.Proc'Length;
         --Put ("* BIN " & Integer'Image(curBin.Index) & " (" &
         --     Integer'Image(procNum) & ")    ");
         --T_IO.Put (Base => 16, Item => curBin.StartAdd);
         --T_IO.Put (Base => 16, Item => curBin.EndAdd);
         --Put(" * ");
         --Put_Line("procNum: " & );
         for p in 1..procNum loop
            curDesc := curBin.Proc(p);
            if (not (curDesc=prevDesc)) then
--                 .text._ada_main 0x40100000 : {
--                 *(.text.ops__myop)
--                   *(.text._ada_main)
--                   } > ram
               if (Trim(curDesc.name) = Trim(PLACE_HOLDER)) then
                  Put (Output, "    . = . + 0x");
                  T_IO.Put (To   => addr,
                            Item => curDesc.size,
                            Base => 16);
                  Put_line (Output, addr(First_Index(addr,'#')+1..11) & ";");

                  if(Global.E_M = DEBUG) then
                     Put_line ("PLACE_HOLDER" & " . = . + 0x"
                               & addr(First_Index(addr,'#')+1..11) &    ";");
                  end if;

               else
                  Put_Line (Output, "    *(.text." & Trim(curDesc.name) &")");
               end if;

               prevDesc := curDesc;
            end if;
            --Put ( Trim(curDesc.name) & " ");-- & Integer'Image(curDesc.size) & " ");
            --T_IO.Put (Base => 16, Item=>curDesc.size);
            --Put (" ");
         end loop;
         --Put_Line ("*");
         --Put_Line("-------------------------------------");
         if curBin.State /= FULL then
            flag := false;
         end if;
         I := I + 1;
      end loop;
      Put_line (Output, "  }  > ram ");
      Put_line (Output, "  ");
      Put_line (Output, "  ");
      Put (Output, "  .text 0x");
      T_IO.Put (To   => addr,
                Item => ramSingleton.startAdd,
                Base => 16);
      Put_line (Output, addr(4..11) & " : {");
      Put_Line (Output, "    /*KEEP (*(.text))*/");
      Put_line (Output, "CREATE_OBJECT_SYMBOLS ");
      Put_line (Output, "    *(.text) ");
      Put_Line (Output, "    *(.text.*)");
      Put_Line (Output, "    *(.lit)");
      Put_Line (Output, "    *(.rodata)");
      Put_Line (Output, "    *(.rodata.*)");
      Put_Line (Output, "    *(.shdata)");
      Put_Line (Output, "    *(.eh_frame)");
      Put_Line (Output, "    *(.gnu.linkonce.t*)");
      Put_Line (Output, "    *(.gnu.linkonce.r*)");
      Put_Line (Output, "    *(.text.ops__myop)");
      Put_Line (Output, "    *(.text.adafinal)");
      Put_Line (Output, "    *(.text.adainit)");
      Put_Line (Output, "  }  > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .shbss : {");
      Put_Line (Output, "    *(.shbss)");
      Put_Line (Output, "  } > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .talias : { }  > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .data : {");
      Put_Line (Output, "    /* Make the clock frequency defined in this script available to the run");
      Put_Line (Output, "       time via this clock_frequency variable. */");
      Put_Line (Output, "    clock_frequency = .;");
      Put_Line (Output, "    LONG(_CLOCK_SPEED)");
      Put_Line (Output, "    *(.data)");
      Put_Line (Output, "    *(.data.*)");
      Put_Line (Output, "    *(.gnu.linkonce.d*)");
      Put_Line (Output, " ");
      Put_Line (Output, "    /* Ensure that the end of the data section is always word aligned.");
      Put_Line (Output, "       Initial values are stored in 4-bytes blocks so we must guarantee");
      Put_Line (Output, "       that these blocks do not fall out the section (otherwise they are");
      Put_Line (Output, "       truncated and the initial data for the last block are lost). */");
      Put_Line (Output, " ");
      Put_Line (Output, "    . = ALIGN(0x4);");
      Put_Line (Output, "  } > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .bss : {");
      Put_Line (Output, "   __bss_start = ALIGN(0x8);");
      Put_Line (Output, "   *(.bss)");
      Put_Line (Output, "   *(.bss.*)");
      Put_Line (Output, "   *(COMMON)");
      Put_Line (Output, " ");
      Put_Line (Output, "  /* Align the stack to 64 bits */");
      Put_Line (Output, " ");
      Put_Line (Output, "   _stack_start = ALIGN(0x8);");
      Put_Line (Output, " ");
      Put_Line (Output, "   /* Reserve the space for the stack to be used by the environment task */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "   . += _STACK_SIZE; ");
      Put_Line (Output, " ");
      Put_Line (Output, "   /* Pointer to the top of the stack to be used by the main procedure (the ");
      Put_Line (Output, "      environment task. */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "   __stack = ALIGN(0x8); ");
      Put_Line (Output, " ");
      Put_Line (Output, "   _end = ALIGN(0x8); ");
      Put_Line (Output, "  } > ram ");
      Put_Line (Output, " ");
      Put_Line (Output, "  /* Stabs debugging sections.  */ ");
      Put_Line (Output, "  .stab          0 : { *(.stab) } ");
      Put_Line (Output, "  .stabstr       0 : { *(.stabstr) } ");
      Put_Line (Output, "  .stab.excl     0 : { *(.stab.excl) } ");
      Put_Line (Output, "  .stab.exclstr  0 : { *(.stab.exclstr) } ");
      Put_Line (Output, "  .stab.index    0 : { *(.stab.index) } ");
      Put_Line (Output, "  .stab.indexstr 0 : { *(.stab.indexstr) } ");
      Put_Line (Output, "  .comment       0 : { *(.comment) } ");
      Put_Line (Output, "  /* DWARF debug sections. ");
      Put_Line (Output, "     Symbols in the DWARF debugging sections are relative to the beginning ");
      Put_Line (Output, "     of the section so we begin them at 0.  */ ");
      Put_Line (Output, "  /* DWARF 1 */ ");
      Put_Line (Output, "  .debug          0 : { *(.debug) } ");
      Put_Line (Output, "  .line           0 : { *(.line) } ");
      Put_Line (Output, "  /* GNU DWARF 1 extensions */ ");
      Put_Line (Output, "  .debug_srcinfo  0 : { *(.debug_srcinfo) } ");
      Put_Line (Output, "  .debug_sfnames  0 : { *(.debug_sfnames) } ");
      Put_Line (Output, "  /* DWARF 1.1 and DWARF 2 */ ");
      Put_Line (Output, "  .debug_aranges  0 : { *(.debug_aranges) } ");
      Put_Line (Output, "  .debug_pubnames 0 : { *(.debug_pubnames) } ");
      Put_Line (Output, "  /* DWARF 2 */ ");
      Put_Line (Output, "  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) } ");
      Put_Line (Output, "  .debug_abbrev   0 : { *(.debug_abbrev) } ");
      Put_Line (Output, "  .debug_line     0 : { *(.debug_line) } ");
      Put_Line (Output, "  .debug_frame    0 : { *(.debug_frame) } ");
      Put_Line (Output, "  .debug_str      0 : { *(.debug_str) } ");
      Put_Line (Output, "  .debug_loc      0 : { *(.debug_loc) } ");
      Put_Line (Output, "  .debug_macinfo  0 : { *(.debug_macinfo) } ");
      Put_Line (Output, "  /* SGI/MIPS DWARF 2 extensions */ ");
      Put_Line (Output, "  .debug_weaknames 0 : { *(.debug_weaknames) } ");
      Put_Line (Output, "  .debug_funcnames 0 : { *(.debug_funcnames) } ");
      Put_Line (Output, "  .debug_typenames 0 : { *(.debug_typenames) } ");
      Put_Line (Output, "  .debug_varnames  0 : { *(.debug_varnames) } ");
      Put_Line (Output, "  /* DWARF 3 */ ");
      Put_Line (Output, "  .debug_pubtypes 0 : { *(.debug_pubtypes) } ");
      Put_Line (Output, "  .debug_ranges   0 : { *(.debug_ranges) } ");
      Put_Line (Output, "  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) } ");
      Put_Line (Output, "  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) } ");
      Put_Line (Output, "} ");
      Put_Line (Output, " ");
      Put_Line (Output, "/* Set the values that define the memory map */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "rom_start = _PROM_START; ");
      Put_Line (Output, "rom_size = _PROM_SIZE; ");
      Put_Line (Output, " ");
      Put_Line (Output, "ram_start = _RAM_START; ");
      Put_Line (Output, "ram_size = _RAM_SIZE; ");
      Put_Line (Output, " ");
      Put_Line (Output, "heap_start = _end; ");
      Put_Line (Output, "heap_end = _RAM_END; ");
      Put_Line (Output, " ");
      Put_Line (Output, "/* END Linker script  */");

      Close (Output);
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put_Line ("  Linker script generated");
      end if;

   end Export_Linker_Script_Tsim;

   procedure Export_Linker_Script_Tsim_WCG  is
      use Global, Utilities, WCG_Graph;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      wcgraph : WCG_ptr := Get_WCG;
      wcgNode : WCG_Vertex := Head (wcgraph.all);
      procNum : Integer := wcgNode.size;
      procList : WCG_Proc_Names_ptr := wcgNode.procedures;
      addr : String (1..12);
   begin
      if procNum < 1 then
         Put_Line ("\n No procedure to be mapped!");
      end if;
      Create (Output, Out_File, "optScriptWCG.ld");
      Put_line (Output, "/****************************************************************************");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                         GNAT COMPILER COMPONENTS                         *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                                 L E O N                                  *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*                            Linker Script File                            *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "*      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *");
      Put_line (Output, "*             Copyright (C) 2003-2006 The European Space Agency            *");
      Put_line (Output, "*                   Copyright (C) 2003-2007 AdaCore                        *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* GNAT is free software;  you can  redistribute it  and/or modify it under *");
      Put_line (Output, "* terms of the  GNU General Public License as published  by the Free Soft- *");
      Put_line (Output, "* ware  Foundation;  either version 2,  or (at your option) any later ver- *");
      Put_line (Output, "* sion.  GNAT is distributed in the hope that it will be useful, but WITH- *");
      Put_line (Output, "* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *");
      Put_line (Output, "* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *");
      Put_line (Output, "* for  more details.  You should have  received  a copy of the GNU General *");
      Put_line (Output, "* Public License  distributed with GNAT;  see file COPYING.  If not, write *");
      Put_line (Output, "* to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *");
      Put_line (Output, "* Boston, MA 02110-1301, USA.                                              *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* As a  special  exception,  if you  link  this file  with other  files to *");
      Put_line (Output, "* produce an executable,  this file does not by itself cause the resulting *");
      Put_line (Output, "* executable to be covered by the GNU General Public License. This except- *");
      Put_line (Output, "* ion does not  however invalidate  any other reasons  why the  executable *");
      Put_line (Output, "* file might be covered by the  GNU Public License.                        *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "* GNARL was developed by the GNARL team at Florida State University.       *");
      Put_line (Output, "* Extensive contributions were provided by Ada Core Technologies, Inc.     *");
      Put_line (Output, "* The  executive  was developed  by the  Real-Time  Systems  Group  at the *");
      Put_line (Output, "* Technical University of Madrid.                                          *");
      Put_line (Output, "*                                                                          *");
      Put_line (Output, "****************************************************************************/");
      Put_line (Output, "  ");
      Put_line (Output, "/* This is a LEON specific version of this file */");
      Put_line (Output, "  ");
      Put_line (Output, "/* This script replaces ld's default linker script, providing the");
      Put_line (Output, "appropriate LEON memory map and output format. */");
      Put_line (Output, "  ");
      Put_line (Output, "/*");
      Put_line (Output, "* Uncomment this if you want the linker to output srecords.");
      Put_line (Output, "OUTPUT_FORMAT(srec)");
      Put_line (Output, "* ");
      Put_line (Output, "*/");
      Put_line (Output, "OUTPUT_ARCH(sparc)");
      Put_line (Output, "ENTRY(start)");
      Put_line (Output, "SEARCH_DIR(.)");
      Put_line (Output, "__DYNAMIC  =  0;");
      Put_line (Output, "/*");
      Put_line (Output, " * The memory map looks like this:");
      Put_line (Output, " * +--------------------+ <- low memory");
      Put_line (Output, " * | .text              |");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * | .data              | initialized data goes here");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * | .bss               |");
      Put_line (Output, " * |        __bss_start | start of bss, cleared by crt0");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * |        _stack_start|");
      Put_line (Output, " * |    stack space     |");
      Put_line (Output, " * |        __stack     | top of stack");
      Put_line (Output, " * |        _end        | start of heap, used by sbrk()");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * |    heap space      |");
      Put_line (Output, " * |                    |");
      Put_line (Output, " * +--------------------+");
      Put_line (Output, " * |  remote monitor    |");
      Put_line (Output, " * +--------------------+ <- high memory");
      Put_line (Output, " */");
      Put_line (Output, " ");
      Put_line (Output, "/*");
      Put_line (Output, " * User modifiable values:");
      Put_line (Output, " *");
      Put_line (Output, " * _PROM_SIZE                 size of PROM");
      Put_line (Output, " *");
      Put_line (Output, " * _RAM_SIZE                  size of RAM");
      Put_line (Output, " *");
      Put_line (Output, " * _STACK_SIZE                size of the stack to be used by the main");
      Put_line (Output, " *                            procedure (environment task)");
      Put_line (Output, " *");
      Put_Line (Output, " * _REM_MON_SIZE              space reserved for the remote monitor");
      Put_Line (Output, " *");
      Put_Line (Output, " * _CLOCK_SPEED               system clock frequency in Hz");
      Put_Line (Output, " *");
      Put_Line (Output, " * These symbols are only used in assembler code, so they only need to");
      Put_Line (Output, " * be listed once. They should always be refered to without SYM().");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "_PROM_SIZE = 128K;");
      Put_Line (Output, "_RAM_SIZE = 64M;");
      Put_Line (Output, "_REM_MON_SIZE = 32K;");
      Put_Line (Output, " ");
      Put_Line (Output, "_RAM_START = 0x40000000;");
      Put_Line (Output, "_RAM_END = _RAM_START + _RAM_SIZE - _REM_MON_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "_PROM_START = 0x00000000;");
      Put_Line (Output, "_PROM_END = _PROM_START + _PROM_SIZE;");
      Put_Line (Output, " ");
      Put_Line (Output, "_STACK_SIZE = (40 * 1024);");
      Put_Line (Output, " ");
      Put_Line (Output, "_CLOCK_SPEED = 50000000; /* in Hz */");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, " *  Base address of the on-CPU peripherals");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "LEON_REG = 0x80000000;");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, " * Setup the memory map for the SIS simulator.");
      Put_Line (Output, " * stack grows up towards low memory.");
      Put_Line (Output, " */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "MEMORY");
      Put_Line (Output, "{");
      Put_Line (Output, "  /* rom (rx)  : ORIGIN = 0x00000000, LENGTH = 128K */");
      Put_Line (Output, "  /* The highest 32K of memory are reserved for the remote monitor */");
      Put_Line (Output, "  ram (rwx) : ORIGIN = 0x40000000, LENGTH = 64M - 32K");
      Put_Line (Output, "}");
      Put_Line (Output, " ");
      Put_Line (Output, "/*");
      Put_Line (Output, " * All the symbols that might be accessed from C code need to be");
      Put_Line (Output, " * listed twice, once with an additional underscore. aout format needs");
      Put_Line (Output, " * and extra underscore, whereas coff & elf doesn't. This is to work");
      Put_Line (Output, " * with both.");
      Put_Line (Output, " */");
      Put_Line (Output, " ");
      Put_Line (Output, "SECTIONS");
      Put_Line (Output, "{");
      Put_Line (Output, " ");

      Put_Line (Output, "  .text._my_section 0x40100000 : {");

      for p in 1..procNum loop
         --+Put_Line (UTrim(procList (p)));
         Put_Line (Output, "    *(.text." & UTrim(procList (p)) &")");
      end loop;

      Put_line (Output, "  }  > ram ");
      Put_line (Output, "  ");
      Put_line (Output, "  ");
      Put (Output, "  .text 0x");
      T_IO.Put (To   => addr,
                Item => ramSingleton.startAdd,
                Base => 16);
      Put_line (Output, addr(4..11) & " : {");
      Put_Line (Output, "    /*KEEP (*(.text))*/");
      Put_line (Output, "CREATE_OBJECT_SYMBOLS ");
      Put_line (Output, "    *(.text) ");
      Put_Line (Output, "    *(.text.*)");
      Put_Line (Output, "    *(.lit)");
      Put_Line (Output, "    *(.rodata)");
      Put_Line (Output, "    *(.rodata.*)");
      Put_Line (Output, "    *(.shdata)");
      Put_Line (Output, "    *(.eh_frame)");
      Put_Line (Output, "    *(.gnu.linkonce.t*)");
      Put_Line (Output, "    *(.gnu.linkonce.r*)");
      Put_Line (Output, "    *(.text.ops__myop)");
      Put_Line (Output, "    *(.text.adafinal)");
      Put_Line (Output, "    *(.text.adainit)");
      Put_Line (Output, "  }  > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .shbss : {");
      Put_Line (Output, "    *(.shbss)");
      Put_Line (Output, "  } > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .talias : { }  > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .data : {");
      Put_Line (Output, "    /* Make the clock frequency defined in this script available to the run");
      Put_Line (Output, "       time via this clock_frequency variable. */");
      Put_Line (Output, "    clock_frequency = .;");
      Put_Line (Output, "    LONG(_CLOCK_SPEED)");
      Put_Line (Output, "    *(.data)");
      Put_Line (Output, "    *(.data.*)");
      Put_Line (Output, "    *(.gnu.linkonce.d*)");
      Put_Line (Output, " ");
      Put_Line (Output, "    /* Ensure that the end of the data section is always word aligned.");
      Put_Line (Output, "       Initial values are stored in 4-bytes blocks so we must guarantee");
      Put_Line (Output, "       that these blocks do not fall out the section (otherwise they are");
      Put_Line (Output, "       truncated and the initial data for the last block are lost). */");
      Put_Line (Output, " ");
      Put_Line (Output, "    . = ALIGN(0x4);");
      Put_Line (Output, "  } > ram");
      Put_Line (Output, " ");
      Put_Line (Output, "  .bss : {");
      Put_Line (Output, "   __bss_start = ALIGN(0x8);");
      Put_Line (Output, "   *(.bss)");
      Put_Line (Output, "   *(.bss.*)");
      Put_Line (Output, "   *(COMMON)");
      Put_Line (Output, " ");
      Put_Line (Output, "  /* Align the stack to 64 bits */");
      Put_Line (Output, " ");
      Put_Line (Output, "   _stack_start = ALIGN(0x8);");
      Put_Line (Output, " ");
      Put_Line (Output, "   /* Reserve the space for the stack to be used by the environment task */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "   . += _STACK_SIZE; ");
      Put_Line (Output, " ");
      Put_Line (Output, "   /* Pointer to the top of the stack to be used by the main procedure (the ");
      Put_Line (Output, "      environment task. */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "   __stack = ALIGN(0x8); ");
      Put_Line (Output, " ");
      Put_Line (Output, "   _end = ALIGN(0x8); ");
      Put_Line (Output, "  } > ram ");
      Put_Line (Output, " ");
      Put_Line (Output, "  /* Stabs debugging sections.  */ ");
      Put_Line (Output, "  .stab          0 : { *(.stab) } ");
      Put_Line (Output, "  .stabstr       0 : { *(.stabstr) } ");
      Put_Line (Output, "  .stab.excl     0 : { *(.stab.excl) } ");
      Put_Line (Output, "  .stab.exclstr  0 : { *(.stab.exclstr) } ");
      Put_Line (Output, "  .stab.index    0 : { *(.stab.index) } ");
      Put_Line (Output, "  .stab.indexstr 0 : { *(.stab.indexstr) } ");
      Put_Line (Output, "  .comment       0 : { *(.comment) } ");
      Put_Line (Output, "  /* DWARF debug sections. ");
      Put_Line (Output, "     Symbols in the DWARF debugging sections are relative to the beginning ");
      Put_Line (Output, "     of the section so we begin them at 0.  */ ");
      Put_Line (Output, "  /* DWARF 1 */ ");
      Put_Line (Output, "  .debug          0 : { *(.debug) } ");
      Put_Line (Output, "  .line           0 : { *(.line) } ");
      Put_Line (Output, "  /* GNU DWARF 1 extensions */ ");
      Put_Line (Output, "  .debug_srcinfo  0 : { *(.debug_srcinfo) } ");
      Put_Line (Output, "  .debug_sfnames  0 : { *(.debug_sfnames) } ");
      Put_Line (Output, "  /* DWARF 1.1 and DWARF 2 */ ");
      Put_Line (Output, "  .debug_aranges  0 : { *(.debug_aranges) } ");
      Put_Line (Output, "  .debug_pubnames 0 : { *(.debug_pubnames) } ");
      Put_Line (Output, "  /* DWARF 2 */ ");
      Put_Line (Output, "  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) } ");
      Put_Line (Output, "  .debug_abbrev   0 : { *(.debug_abbrev) } ");
      Put_Line (Output, "  .debug_line     0 : { *(.debug_line) } ");
      Put_Line (Output, "  .debug_frame    0 : { *(.debug_frame) } ");
      Put_Line (Output, "  .debug_str      0 : { *(.debug_str) } ");
      Put_Line (Output, "  .debug_loc      0 : { *(.debug_loc) } ");
      Put_Line (Output, "  .debug_macinfo  0 : { *(.debug_macinfo) } ");
      Put_Line (Output, "  /* SGI/MIPS DWARF 2 extensions */ ");
      Put_Line (Output, "  .debug_weaknames 0 : { *(.debug_weaknames) } ");
      Put_Line (Output, "  .debug_funcnames 0 : { *(.debug_funcnames) } ");
      Put_Line (Output, "  .debug_typenames 0 : { *(.debug_typenames) } ");
      Put_Line (Output, "  .debug_varnames  0 : { *(.debug_varnames) } ");
      Put_Line (Output, "  /* DWARF 3 */ ");
      Put_Line (Output, "  .debug_pubtypes 0 : { *(.debug_pubtypes) } ");
      Put_Line (Output, "  .debug_ranges   0 : { *(.debug_ranges) } ");
      Put_Line (Output, "  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) } ");
      Put_Line (Output, "  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) } ");
      Put_Line (Output, "} ");
      Put_Line (Output, " ");
      Put_Line (Output, "/* Set the values that define the memory map */ ");
      Put_Line (Output, " ");
      Put_Line (Output, "rom_start = _PROM_START; ");
      Put_Line (Output, "rom_size = _PROM_SIZE; ");
      Put_Line (Output, " ");
      Put_Line (Output, "ram_start = _RAM_START; ");
      Put_Line (Output, "ram_size = _RAM_SIZE; ");
      Put_Line (Output, " ");
      Put_Line (Output, "heap_start = _end; ");
      Put_Line (Output, "heap_end = _RAM_END; ");
      Put_Line (Output, " ");
      Put_Line (Output, "/* END Linker script  */");

      Close (Output);
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put_Line ("  Linker script generated");
      end if;

   end Export_Linker_Script_Tsim_WCG;

   function Start_Address return Integer is
   begin
      return ramSingleton.startAdd;
   end Start_Address;


   function Get_Ram_Sets return Bin_Array is
   begin
      return ramSingleton.sets;
   end Get_Ram_Sets;


   --  Equality test for Descriptors
   function "=" (Left, Right : Descriptor) return Boolean is
   begin
      return Left.name = Right.name;
   end "=";


   -- Returns the first BIN non completely filled
   function Bin_Available return Bin_Ptr is
         res : Bin_Ptr;
   begin
      for I in 1..ramSize loop
         --Put ("* ");
         res := ramSingleton.Sets(I);
         --res := ramCopy.Sets(I);
         if (res.State /= Full) then
            return res;
         end if;
      end loop;
      return res;
   end Bin_Available;

   --  Return FALSE only in case of overflow
   function Fit_In_Bin (b : BIn_Ptr; size : Integer) return Boolean is
   begin
      return ((b.NextAdd + size) <= b.EndAdd);
   end Fit_In_Bin;

   --  Finds a BIN including a placeholder (gap) in which the current procedure
   --  perfectly fits
   procedure PH_Available (size : Integer; index, procInd, offset, space : in out Integer) is
      use Cache, Utilities;
      curBin : Bin_Ptr;
      procNUm : Integer;
      curDesc : Descriptor;
      prevDesc : Descriptor := Null_Descriptor;
      prevProcIndex : Integer := 0;
      bestFit : Integer := 0;
      fitness : Integer := Get_Set_Size;
   begin
      --index := 0;
      offset := 0;
      mainLoop:

      for I in 1..ramSingleton.bins loop--ramSize loop
         --Put ("* ");
         -- ramSingleton
         curBin := ramSingleton.Sets(I);
         if (curBin.State /= FREE) then
            --Put_line("not free");

            procNum := curBin.Proc'Length;
            for pr in 1..procNum loop
               curDesc := curBin.Proc(pr);
               if (Global.E_M in VERBOSE..DEBUG) then
                  Put_Line(Trim(curDesc.name) & " " & PLACE_HOLDER
                           & " size: " & Integer'Image(curDesc.size)
                           & " proc size: " & Integer'Image(size));
                  Put_line(Boolean'Image(Trim(curDesc.name)=Trim(PLACE_HOLDER)));
                  --Put_Line(Integer'Image(curDesc.size + prevDesc.size));
                   --"__PHolder__"));
               end if;


               if (Trim(curDesc.name)= Trim(PLACE_HOLDER)) then
--                    if (curDesc.size >= size) then
--                       Put_Line(" -a- ");
--                       --if (curDesc.size - size <= fitness) then
--                          index := I;
--                          procInd := pr;
--                          offset := curDesc.startAdd;
--                          space := curDesc.size;
--                       --end if;
--                       exit mainLoop;

                  --Put_Line(Integer'Image(curDesc.size + prevDesc.size));
                  --Put("Fitness: " & Integer'Image(fitness));
                  if (Trim(prevDesc.name)= Trim(PLACE_HOLDER) and then
                            prevDesc.size+curDesc.size >= size) then
                     --Put(" -b- ");
                     if (curDesc.size - size <= fitness) then
                        fitness := curDesc.size + prevDesc.size - size;
                        --Put_Line(" newFitness: " & Integer'Image(fitness));
                        index := I-1;
                        procInd := prevProcIndex;
                        offset := prevDesc.startAdd;
                        space := prevDesc.size;
                     end if;
                     --exit mainLoop;
                  end if;

                  if (curDesc.size >= size) then
                     --Put(" -a- ");
                     if (curDesc.size - size <= fitness) then
                        fitness := curDesc.size - size;
                        --Put_Line(" newFitness: " & Integer'Image(fitness));
                        index := I;
                        procInd := pr;
                        offset := curDesc.startAdd;
                        space := curDesc.size;
                     end if;
                     --exit mainLoop;
                  end if;
                     prevDesc := curDesc;
                     prevProcIndex := pr;
                  --end if;

               end if;
               prevDesc := curDesc;
               prevProcIndex := pr;
            end loop;
         else
            exit mainLoop;
         end if;
      end loop mainLoop;
      if (Global.E_M = DEBUG) then
         Put_line("PH available: BIN (" & Integer'IMage(index) &") Proc (" &
                  Integer'Image(procInd) & ") Start (" & Integer'Image(offset) &
                  ") " & Integer'Image(space) );
      end if;
   exception
      when Error: others =>
         Put_line("PH_Available (" & Exception_Name(Error) & ")");
         raise;
   end PH_Available;


   function Map_In_Place_Holder (procName: String; size : Integer)
                                 return Boolean is
      use Cache, Ada.Strings.Fixed;
      curBin  : Bin_Ptr;-- := Bin_Available;
      --nextBin : Bin_Ptr;
      procNum : Positive;
      incr : Integer := 0;
      curDesc  : Descriptor;
      binIndex : Integer :=0;
      procIndex : Integer := 0;
      offset : Integer := 0;
      spaceAvail : Integer := 0;
   begin
      Put_line("map in placeholder");
      PH_Available (size, binIndex, procIndex, offset, spaceAvail);
      if(binIndex/=0) then
         -- ramSingleton
         curBin := ramSingleton.sets(binIndex);
         procNUm := curBin.Proc'Length;
         if (Global.E_M in VERBOSE..DEBUG) then
            Put_Line("Inserting 'Independent' procedure '" & procName &
                     "' into BIN " & Integer'Image(curBin.Index));
         end if;

         --  Unnecessary check ?
         if (spaceAvail-size > 0) then
            incr := incr + 1;
         end if;

         declare
            newProc : Contents := new Procedures (1..procNum+incr);
            curPh : Descriptor := curBin.Proc(procIndex);
         begin
            --Put_line(procName);
            Ada.Strings.Fixed.Move (procName, Fixed_Str_Desc, Ada.Strings.Right,
                                    Ada.Strings.Left,Ada.Strings.Space);

            -- Copy the first binIndex-1 procedures
            for I in 1..procIndex-1 loop
               --Put (Integer'Image(I) & " ");
               newProc(I) := curBin.Proc(I);
            end loop;

            --+New_Line;
            -- Add the new procdure
            if (spaceAvail-size < 0) then
               curDesc := (Fixed_Str_Desc, spaceAvail,
                           offset, curPh.offSet);
            else
               curDesc := (Fixed_Str_Desc, size,
                           offset, curPh.offSet);
            end if;

            --+Put ("'" & Integer'Image(procIndex) & "' ");
            --+curBin.Proc(procIndex) := curDesc;

            newProc(procIndex) := curDesc;
            -- If we still have some spare space
            if (spaceAvail-size > 0) then
               --+Put_line("spaceAvail-size>0");
               curDesc := (PLACE_HOLDER, spaceAvail-size,
                           offset+size, 0);--(16 - curBin.NextAdd mod 16));
               --+curBin.Proc(procIndex +1) := curDesc;
               newProc (procIndex + 1) := curDesc;
            end if;
            --+Put_Line("newProcSize: " & Integer'Image(newProc'Length));
            -- Copy residual procedures (if any)
            for J in procIndex+1..procNum loop
               --+Put (Integer'Image(J) & " ");
               newProc(J+incr) := curBin.Proc(J);
               --+Put ("->");
            end loop;

            curBin.Proc := newProc;

            if (spaceAvail-size < 0) then
               -- Insert the residual into the next bin
               --+Put_line("rec: " & Integer'Image(size-spaceAvail));
               return Map_In_Place_Holder (procName, size-spaceAvail);
            end if;
            --+New_line;
            --+Put_Line (newProc(procIndex).name);
            --+Put_Line (curBin.Proc(procIndex).name);
         end;
         return True;
      else
         return False;
      end if;
   end Map_In_Place_Holder;

   procedure Erase_Pool (p : Proc_Pool_ptr) is
      temp : Proc_Pool_ptr := p;
   begin
      Free_Pool (temp);
   end Erase_Pool;


   -- Concretely maps the procedure in memory
   procedure Map_This (procName: String; size : Integer) is
      use Cache, Ada.Strings.Fixed;
      curBin  : Bin_Ptr := Bin_Available;
      nextBin : Bin_Ptr;
      procNum : Positive;
      residual : Integer;
      curDesc  : Descriptor;
      copyDesc : Descriptor;
      offset : Integer;
   begin
      --Put_Line("Inserting procedure '" & procName &"' into BIN " &
      --          Integer'Image(curBin.Index));
      if(curBin.State = FREE) then
         --Put_line(" BIN " & Integer'Image(curBin.Index) & " is FREE");
         --  Initialize procedure list
         curBin.Proc := new Procedures (1..1);
         --Put (" 1*ù*");
         -- Add the procedure to the list
         Ada.Strings.Fixed.Move (procName, Fixed_Str_Desc,
                                 Ada.Strings.Right,
                                 Ada.Strings.Left,Ada.Strings.Space);
         --Put_line("mmmm");
         offset := 0;
         curDesc := (Fixed_Str_Desc, size, curBin.StartAdd, offset);
         curBin.Proc (1) := curDesc;
                  --Put (" 2*ù*");
         --Put_Line(" Procedure '" & procName &"' inserted");
      elsif(curBin.State = USED) then
         --Put_line(" BIN " & Integer'Image(curBin.Index) & " is USED");
         procNum := curBin.Proc'Length;
         declare
            tempContents : Contents;
            newProc : Contents := new Procedures (1..procNum+1);
         begin
            for I in 1..procNum loop
               newProc(I) := curBin.Proc(I);
            end loop;
            Ada.Strings.Fixed.Move (procName, Fixed_Str_Desc, Ada.Strings.Right,
                                    Ada.Strings.Left,Ada.Strings.Space);
            --  Offset 4 Byte -> 16
            offset := 0;--(16 - curBin.NextAdd mod 16);
            curDesc := (Fixed_Str_Desc, size,
                        curBin.NextAdd + offset, offset);
            newProc(procNum+1):= curDesc;
            tempContents := curBin.Proc;
            curBin.Proc := newProc;
            -- Deallocate
            Free_Proc (tempContents);
            --Put_Line(" Procedure '" & procName &"' inserted");
         end;
      else
         --Put_line(" BIN " & Integer'Image(curBin.Index) & " is FULL!");
         Put_LINE("null BRANCH!!!");
         null;
      end if;

      if (Fit_In_Bin(curBin, size) and
            curBin.NextAdd /= curBin.EndAdd) then
         --Put (" 3*ù*");

         -- Update bin info
         curBin.State := USED;
         curBin.NextAdd := curBin.NextAdd + offset + size;
         -- -1;
         --Put_line(" Procedure '" & procName &"' completely fits in BIN "
         --         & Integer'Image(curBin.Index));

         -- *** ADD_PROCEDURE to MAPPED_PROCEDURES **
--           Put ("Map_This: [" & curDesc.name);
--           Put ("] (");T_IO.Put (Base => 16, Item => curDesc.size);
--           Put (") -> "); T_IO.Put (Base => 16, Item => curDesc.startAdd);
         --+T_IO.Put (Base => 16, Item => curDesc.offset); New_Line;
--         Put (" 4*ù*");
         copyDesc := (curDesc.name, size, curDesc.startAdd, curDesc.offSet);
--         Put(" 5*ù*");
         Add_Procedure_Unique (p    => MAPPED_PROCEDURES,
                               proc => (curDesc.name,
                                        size, --curDesc.size,
                                        curDesc.startAdd,
                                        curDesc.offSet),
                               size => curDesc.size);

      else
         --  Procedure does NOT fit completely in the current bin
         --  and overflows in the next one
         --if (Global.E_M in VERBOSE..DEBUG) then
          --  Put (" Bin overflow: oldSize: ");
          --  T_IO.Put (Base => 16, Item=>curDesc.size);
         --end if;
	 --Put (" A");
         curDesc.size := (curBin.EndAdd+1)-curBin.NextAdd;
         --Put (" B");
         --  Put_line( Integer'Image(curBin.Proc'Length) & "-"
         --           & Integer'Image(procNum+1));
         curBin.Proc(curBin.Proc'Length):= curDesc;
         --Put (" C");
         if (Global.E_M in VERBOSE..DEBUG) then
            Put (" newSize: "); T_IO.Put (Base => 16, Item=>curDesc.size);
            New_Line;
         end if;
         --Put (" D");
         curBin.State := FULL;
         residual := size - ((curBin.EndAdd+1)-curBin.NextAdd);
         curBin.NextAdd := curBin.EndAdd + 1;
         nextBin := new Bin'(Index    => curBin.Index+1,
                             State    => Free,
                             StartAdd => curBin.EndAdd+1,
                             EndAdd   => End_Of_Set(curBin.EndAdd+1),
                             NextAdd  => curBin.EndAdd+1,
                             Proc     => null
                            );
         --Put (" E");
         -- ramSingleton
         ramSingleton.sets(curBin.Index+1) := nextBin;
         --Put (" F");
         -- ramSingleton
         ramSingleton.bins := ramSingleton.bins +1;
         --Put (" * ");
         Map_This(procName, residual);
      end if;
      exception
         when Error: others =>
         Put_line("Map_This -> " & Trim(procName) & " [" & Integer'Image(Mapped) & "] ("
                  & Exception_Name(Error) & ")");
   end Map_This;


   procedure Print_Descriptor (d : Descriptor) is
   begin
      Put_Line ("************************ Descriptor ****************************");
      Put_Line ("Name: " & Trim(d.name));
      Put ("Size: "); T_IO.Put (Base => 16, Item=>d.size); New_Line;
      Put ("StartAdd: "); T_IO.Put (Base => 16, Item=>d.startAdd); New_Line;
      Put ("Offset: "); T_IO.Put (Base => 16, Item=>d.offset); New_Line;
      Put_Line ("****************************************************************");
   end Print_Descriptor;


   procedure Map_Procedure (procName: String; size : Integer) is
      use Cache, Utilities;
      curBin  : Bin_Ptr;
      procNum : Positive;
      curDesc : Descriptor;
      flag, placeHolder : Boolean := False;
   begin
      --Put("Going to insert procedure '" & procName &"' [");
      --Put_line(Integer'Image(size) & "]");
      mainLoop : for I in 1..ramSize loop
         --Put_line(" Index: " & Integer'Image(I));
         curBin := ramSingleton.sets (I);
         if (curBin.State /= FREE) then
            procNum := curBin.Proc'Length;
            --Put_line(" Index: " & Integer'Image(I) &
            --         "Proc in BIN: " & Integer'Image(procNum));
            for d in 1..procNum loop
               curDesc := curBin.Proc(d);
               if (Trim(curDesc.name)=procName) then
                  if (Global.E_M in VERBOSE..DEBUG) then
                     Put_Line (" Procedure '" & Trim(procName) & "' already mapped");
                  end if;


--++++++++++++++++++++++++ PLACEHOLDER +++++++++++++++-----
--                    if(I < new_Component_Start_BinIndex) then
--                       --Put_line("PLACE holder is True");
--                       placeHolder := True;
--                    end if;


                  flag := True;
                  exit mainLoop when flag = True;
               end if;
            end loop;
         else
            --Put_Line("EXIT");
            exit mainLoop;
         end if;
         if(curBin.State /= FULL) then
            exit mainLoop;
         end if;
      end loop mainLoop;
      if (not flag) then
         --Put_Line("Mapping a new procedure");
         Map_This (procName, size);
      --elsif (placeHolder) then
      --   null;
         --Put_Line("Mapping a place holder for a shared procedure");
         --Map_This (PLACE_HOLDER, size);
      end if;
   exception
      when Error: others =>
         Put_line("Map_Procedure -> " & procName & "(" & Exception_Name(Error) & ")");
   end Map_Procedure;



   function Compute_Conflicts (st1,end1,st2,end2: Address) return Natural is
      use Cache;
      modSt1 : Integer := st1 mod Get_Set_Size;
      modEnd1 : Integer := end1 mod Get_Set_Size;
      modSt2 : Integer := st2 mod Get_Set_Size;
      modEnd2 : Integer := end2 mod Get_Set_Size;
   begin
      if (modSt1 > modSt2 and modEnd1 < modEnd2) then
         --Put_Line("Sh included in pool");
         return 2;
      elsif (modSt1 < modSt2 and modEnd1 > modEnd2) then
         --Put_Line("Pool included in Sh");
         return 2;
      elsif (modSt1 < modSt2 and modEnd1 > modSt2) then
         --Put_Line("Sh intersects pool");
         return 1;
      elsif (modSt1 > modSt2 and modSt1 < modEnd2) then
         --Put_Line("Sh intersects pool");
         return 1;
      end if;

      return 0;
       exception
         when Error: others =>
         Put_line("Compute_conflicts (" & Exception_Name(Error) & ")");
         raise;
   end Compute_Conflicts;



   procedure Check_Mapping (shProc : Contents; shNum: Positive; size : Integer;
                           disp : in out Natural) is
      use Cache;
      binAv : Bin_Ptr := Bin_Available;
      nextAddress : Address;
      stAdd, endAdd : Address;
      poolStart, poolEnd : Address;
      conflicts : Natural := 0;
      curDesc : Descriptor;
      displacement : Integer := 0;
      setSize : Integer := Get_Set_Size;
      flag : Boolean := False;
   begin
      if(shNum > 0) then
         nextAddress := binAv.NextAdd;
         --        poolStart := nextAddress + displacement;
         --        poolEnd := poolStart + size -1;
         while (not flag and then displacement < setSize) loop
            --Put_Line ("CheckMapping");
            conflicts := 0;
            poolStart := nextAddress + displacement;
            poolEnd := poolStart + size -1;
            for I in 1..shNum loop
               --Put ("$[1]");
               curDesc := shProc (I);
               --Put ("$[2] {" & Trim(curDesc.name) & "}");
               stAdd := curDesc.startAdd;
               --Put ("$[3]");
               --Put("stAdd=> ");T_IO.Put (Base => 16, Item => stAdd); New_Line;
               --Put("cusDesc.size=> ");T_IO.Put (Base => 16, Item => curDesc.size); New_Line;
               endAdd := stAdd + curDesc.size -1;
--                 Put_Line("Report:");
--                 T_IO.Put (Base => 16, Item=>stAdd); Put (" ");
--                 T_IO.Put (Base => 16, Item=>endAdd); Put (" - ");
--                 T_IO.Put (Base => 16, Item=>poolStart); Put (" ");
--                 T_IO.Put (Base => 16, Item=>poolEnd);
--                 New_Line;
               --Put ("$[4]");
               conflicts := conflicts + Compute_Conflicts (stAdd, endAdd, poolStart, poolEnd);
               --Put ("$[5]");
            end loop;
            if (conflicts = 0) then
               if (Global.E_M in VERBOSE..DEBUG) then
                  Put ("No conflicts with a ");
                  T_IO.Put (Base=>16, Item=>displacement);
                  Put_Line(" displacement");
               end if;
               flag := True;
               disp := displacement;
            end if;
            --  Increment by a cache line size
            displacement := displacement + Get_Line_Size;
         end loop;
         -- No displacement allowed to avoid conflicts
      end if;
      exception
         when Error: others =>
         Put_line("Check_Mapping (" & Exception_Name(Error) & ")");
         raise;
   end Check_Mapping;

   procedure Check_Mapping_Pool (shProc : Contents; shNum: Positive; size : Integer;
                           disp : in out Natural) is
      use Cache;
      binAv : Bin_Ptr := Bin_Available;
      nextAddress : Address;
      stAdd, endAdd : Address;
      poolStart, poolEnd : Address;
      conflicts : Natural := 0;
      curDesc : Descriptor;
      displacement : Integer := 0;
      setSize : Integer := Get_Set_Size;
      flag : Boolean := False;
   begin
      if(shNum > 0) then
         nextAddress := binAv.NextAdd;
         --        poolStart := nextAddress + displacement;
         --        poolEnd := poolStart + size -1;
         while (not flag and then displacement < setSize) loop
            conflicts := 0;
            poolStart := nextAddress + displacement;
            poolEnd := poolStart + size -1;
            for I in 1..shNum loop
               curDesc := shProc (I);            stAdd := curDesc.startAdd;
               endAdd := stAdd + curDesc.size -1;
--                 Put_Line("Report:");
--                 T_IO.Put (Base => 16, Item=>stAdd); Put (" ");
--                 T_IO.Put (Base => 16, Item=>endAdd); Put (" - ");
--                 T_IO.Put (Base => 16, Item=>poolStart); Put (" ");
--                 T_IO.Put (Base => 16, Item=>poolEnd);
--                 New_Line;
               conflicts := conflicts + Compute_Conflicts (stAdd, endAdd, poolStart, poolEnd);
            end loop;
            if (conflicts = 0) then
               if (Global.E_M in VERBOSE..DEBUG) then
                  Put ("No conflicts with a ");
                  T_IO.Put (Base=>16, Item=>displacement);
                  Put_Line(" displacement");
               end if;
               flag := True;
               disp := displacement;
            end if;
            --  Increment by a cache line size
            displacement := displacement + Get_Line_Size;
         end loop;
         -- No displacement allowed to avoid conflicts
      end if;
      exception
         when Error: others =>
         Put_line("Check_Mapping (" & Exception_Name(Error) & ")");
         raise;
   end Check_Mapping_Pool;


   --  Initialize the Memory space
   procedure Init_Ram is
      use Global, Cache;
      firstBin : Bin_Ptr;
   begin
      ramSingleton := new RAM_Type'(startAdd => ramStartAddress (target),--16#40020020#,
                                    sets => binArr,
                                    bins => 0);
      firstBin := new Bin'(Index    => 1,
                           State    => Free,
                           StartAdd => Start_Address,
                           EndAdd   => End_Of_Set(Start_Address),
                           NextAdd  => Start_Address,
                           Proc     => null
                          );

      ramSingleton.sets(1) := firstBin;
      ramSingleton.bins := ramSingleton.bins +1;
   end Init_Ram;



   --  Unused
   procedure Load_Mapping is
      --curBin : Bin_Ptr;
   begin
      --curBin := ramSingleton.sets(1);
      ramCopy := new RAM_Type'(startAdd => 16#40000000#,
                               sets => Get_Ram_Sets,
                               bins => ramSingleton.bins);
      --Put_Line(Integer'Image(ramSIngleton.bins));
      --Print_Bins (true);
      --Print_Bins (false);
   end Load_Mapping;


   --  Unused
   procedure Save_Mapping is
   begin
      ramSingleton := ramCopy;
   end Save_Mapping;

--     -- Static record of procedures
--
--     MAPPED_PROCEDURES : Proc_Pool_ptr := new Proc_Pool'(null, 0,0,False);

   -- Check if a procedure is already mapped
   function Already_Mapped (proc : Descriptor) return Boolean is
      --procList : Contents := MAPPED_PROCEDURES.proc;
      curProc : Descriptor;
      found : Boolean := False;
   begin
      Search:
      for I in 1..MAPPED_PROCEDURES.num loop
         curProc := MAPPED_PROCEDURES.proc (I);
         if (curProc.name=proc.name) then
            found := True;
            exit Search;
         end if;
      end loop Search;
      return found;
   end Already_Mapped;

   -- Check if a procedure is already mapped
   procedure Already_Mapped (procName : Unbounded_String;
                             found : out Boolean;
                             d : out Descriptor) is
      procList : Contents := MAPPED_PROCEDURES.proc;
      curProc : Descriptor;
      --+found : Boolean := False;
   begin
      found:=false;
      --+Put_Line ("$");
      if MAPPED_PROCEDURES.num = 0 then
         found := False;
         --Put ("No mapped procedures yet!");
      else
         Search:
         for I in 1..MAPPED_PROCEDURES.num loop
            curProc := MAPPED_PROCEDURES.proc (I);
            if (Trim(curProc.name)=UTrim(procName)) then
               found := True;
               d := (curProc.name, curProc.size,
                     curProc.startAdd, curProc.offSet);--curProc;
--                 Put_Line ("Proc " & UTrim(procName) & " is not new");
--                 Put_Line ("  d->" & Trim(d.name) & " ");
--                 Put_Line ("   ->" & Trim(curProc.name) & " existed.");
               exit Search;
            end if;
         end loop Search;
--           if not found then
--              Put ("Proc " & UTrim(procName) & " has not been mapped yet");
--           end if;
      end if;
   end Already_Mapped;


   function Size (p : Proc_Pool_ptr) return Integer is
   begin
      return p.size;
   end Size;

   function Size (p : Proc_Pool) return Integer is
   begin
      return p.size;
   end Size;

   --  Return the number of procedures in the pool
   function Num (p : Proc_Pool_ptr) return Integer is
   begin
      return p.num;
   end Num;

   function Num (p : Proc_Pool) return Integer is
   begin
      return p.num;
   end Num;

   --  Add a procedure to the pool
   procedure Add_Procedure (p : in out Proc_Pool_ptr;
                            proc: Descriptor;
                            size : Integer) is
   begin
      p.num := p.num + 1;
      p.proc (p.num+1) := proc;
      p.size := p.size + proc.size;
   end Add_Procedure;


   --  Add a procedure to the pool
   procedure Add_Procedure_Unique (p : in out Proc_Pool_ptr;
                                   procName: Unbounded_String;
                                   size : Integer) is
      pos : Integer := p.num +1;
      lastAdded : Descriptor := p.proc (p.num);
   begin
      --Put (" AAAAAAAAAAAAAAAA ");
      if p.num > 0 then
         --Put_Line (">0");
         if (lastAdded.name /= procName) then
            Mapped := Mapped + 1;
            p.num := p.num + 1;
            Ada.Strings.Fixed.Move (UTrim(procName),
                                    Fixed_Str_Desc,
                                    Ada.Strings.Right,
                                    Ada.Strings.Left,Ada.Strings.Space);
            p.proc (p.num) := (Fixed_Str_Desc,
                               size,
                               Integer'Value("16#"&"40000000"& "#"),
                               0);
            p.size := p.size + size;
--           else
--              Put_Line ("     [repeat]  ");
         end if;
      else
         Mapped := Mapped + 1;
         p.num := p.num + 1;
         Ada.Strings.Fixed.Move (UTrim(procName),
                                 Fixed_Str_Desc,
                                 Ada.Strings.Right,
                                 Ada.Strings.Left,Ada.Strings.Space);
         p.proc (p.num) := (Fixed_Str_Desc,
                            size,
                            Integer'Value("16#"&"40000000"& "#"),
                            0);
         p.size := p.size + size;
      end if;
   end Add_Procedure_Unique;

   --  Add a procedure to the pool
   procedure Add_Procedure_Unique (p : in out Proc_Pool_ptr;
                                   proc: Descriptor;
                                   size : Integer) is
      lastAdded : Descriptor;
   begin
      --Put (" AAAAAAAAAAAAAAAA ");
      if p.num > 0 then
         --Put_Line (">0");
         lastAdded := p.proc (p.num);
         if (proc /= lastAdded) then
            Mapped := Mapped + 1;
            p.num := p.num + 1;
            p.proc (p.num) := proc;
            p.size := p.size + proc.size;
--           else
--              Put_line ("     [repeat]");
         end if;
      else
         Mapped := Mapped + 1;
         p.num := p.num + 1;
         p.proc (p.num) := proc;
         p.size := p.size + proc.size;
      end if;
   end Add_Procedure_Unique;



   procedure Add_Procedure (p : in out Proc_Pool_ptr; procName: Unbounded_String;
                            size : Integer) is
      place : Integer := p.num +1;
   begin
      p.num := p.num + 1;
      --+Put_line("???->" & UTrim(procName));
      Ada.Strings.Fixed.Move (UTrim(procName), Fixed_Str_Desc, Ada.Strings.Right,
                              Ada.Strings.Left,Ada.Strings.Space);
--        name     : String (1..100);
--           size     : Integer;
--           startAdd : Address;
--           offSet   : Integer;
      --+Put ("§" & Integer'Image(p.proc'Length));
      p.proc (place) := (Fixed_Str_Desc,
                         size,
                         Integer'Value("16#"&"40000000"& "#"),
                         0);
      --+Put_line("size: " & Integer'IMage(size));
      p.size := p.size + size;
   end Add_Procedure;



   --  Get the n-th procedure from pool
   function Get_Procedure (p : Proc_Pool_ptr; n : Integer) return Descriptor is
   begin
      return Get_Procedure (p.all, n);
   end Get_Procedure;

   function Get_Procedure (p : Proc_Pool; n : Integer) return Descriptor is
   begin
      return p.proc (n);
   end Get_Procedure;

   procedure Flush_Pool (pool : in out Proc_Pool_ptr) is
      curProc : Descriptor;
      curProcCopy : Descriptor;
   begin
      if (pool.num > 0) then
         for J in 1..pool.num loop
--              Put ("Map (FLUSH) ");
            curProc := RAM.Get_Procedure (p => pool,
                                          n => J);
            Map_This (procName => curProc.name,
                      size     => curProc.size);
            -- At the same time update the MAPPED_PROCEDURES pool
            -- ********** DOUBLE insertion (see MAP_THIS)
            curProcCopy := (curProc.name, curProc.size,
                            curProc.startAdd, curProc.offSet);
            Add_Procedure_unique (p    => MAPPED_PROCEDURES,
                                  proc => curProcCopy,
                                  size => 0);
            -- **********
            --Print_Descriptor (curProcCopy);
         end loop;
--           Put_Line (" ");
         -- Deallocate
         Erase_Pool (pool);
         pool := new Proc_Pool'(new Procedures (1..1400),
                                0,
                                0,
                                False);
      end if;
   end Flush_Pool;

end RAM;
