/****************************************************************************
*                                                                          *
*                         GNAT COMPILER COMPONENTS                         *
*                                                                          *
*                                 L E O N                                  *
*                                                                          *
*                            Linker Script File                            *
*                                                                          *
*      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
*             Copyright (C) 2003-2006 The European Space Agency            *
*                   Copyright (C) 2003-2007 AdaCore                        *
*                                                                          *
* GNAT is free software;  you can  redistribute it  and/or modify it under *
* terms of the  GNU General Public License as published  by the Free Soft- *
* ware  Foundation;  either version 2,  or (at your option) any later ver- *
* sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
* for  more details.  You should have  received  a copy of the GNU General *
* Public License  distributed with GNAT;  see file COPYING.  If not, write *
* to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
* Boston, MA 02110-1301, USA.                                              *
*                                                                          *
* As a  special  exception,  if you  link  this file  with other  files to *
* produce an executable,  this file does not by itself cause the resulting *
* executable to be covered by the GNU General Public License. This except- *
* ion does not  however invalidate  any other reasons  why the  executable *
* file might be covered by the  GNU Public License.                        *
*                                                                          *
* GNARL was developed by the GNARL team at Florida State University.       *
* Extensive contributions were provided by Ada Core Technologies, Inc.     *
* The  executive  was developed  by the  Real-Time  Systems  Group  at the *
* Technical University of Madrid.                                          *
*                                                                          *
****************************************************************************/
  
/* This is a LEON specific version of this file */
  
/* This script replaces ld's default linker script, providing the
appropriate LEON memory map and output format. */
  
/*
* Uncomment this if you want the linker to output srecords.
OUTPUT_FORMAT(srec)
* 
*/
OUTPUT_ARCH(sparc)
ENTRY(start)
SEARCH_DIR(.)
__DYNAMIC  =  0;
/*
 * The memory map looks like this:
 * +--------------------+ <- low memory
 * | .text              |
 * |                    |
 * +--------------------+
 * | .data              | initialized data goes here
 * |                    |
 * +--------------------+
 * | .bss               |
 * |        __bss_start | start of bss, cleared by crt0
 * |                    |
 * |        _stack_start|
 * |    stack space     |
 * |        __stack     | top of stack
 * |        _end        | start of heap, used by sbrk()
 * +--------------------+
 * |    heap space      |
 * |                    |
 * +--------------------+
 * |  remote monitor    |
 * +--------------------+ <- high memory
 */
 
/*
 * User modifiable values:
 *
 * _PROM_SIZE                 size of PROM
 *
 * _RAM_SIZE                  size of RAM
 *
 * _STACK_SIZE                size of the stack to be used by the main
 *                            procedure (environment task)
 *
 * _REM_MON_SIZE              space reserved for the remote monitor
 *
 * _CLOCK_SPEED               system clock frequency in Hz
 *
 * These symbols are only used in assembler code, so they only need to
 * be listed once. They should always be refered to without SYM().
 */
 
_PROM_SIZE = 128K;
_RAM_SIZE = 64M;
_REM_MON_SIZE = 32K;
 
_RAM_START = 0x40000000;
_RAM_END = _RAM_START + _RAM_SIZE - _REM_MON_SIZE;
 
_PROM_START = 0x00000000;
_PROM_END = _PROM_START + _PROM_SIZE;
 
_STACK_SIZE = (40 * 1024);
 
_CLOCK_SPEED = 50000000; /* in Hz */
 
/*
 *  Base address of the on-CPU peripherals
 */
 
LEON_REG = 0x80000000;
 
/*
 * Setup the memory map for the SIS simulator.
 * stack grows up towards low memory.
 */ 
 
MEMORY
{
  /* rom (rx)  : ORIGIN = 0x00000000, LENGTH = 128K */
  /* The highest 32K of memory are reserved for the remote monitor */
  ram (rwx) : ORIGIN = 0x40000000, LENGTH = 64M - 32K
}
 
/*
 * All the symbols that might be accessed from C code need to be
 * listed twice, once with an additional underscore. aout format needs
 * and extra underscore, whereas coff & elf doesn't. This is to work
 * with both.
 */
 
SECTIONS
{
 
  .text._my_section 0x40100000 : {
    *(.text.gnc__passive__opcs__compute)
    *(.text.xgnc__aocs_computation)
    *(.text.xgnc__aocs)
    *(.text.cnt__cnt_main)
    *(.text.xgnc__control__step)
    *(.text.xgnc__guidance__gnc_q4_0_q3_0_q2_0_q1_0)
    *(.text.xgnc__guidance__gnc_attitude_calculation_start)
    *(.text.xgnc__guidance__gnc_attitude_calculation_disable)
    *(.text.xgnc__guidance__gnc_attitude_calculation_init)
    *(.text.xgnc__guidance__gnc_attitude_calculation)
    *(.text.xgnc__navigation__gnc_select_sun_versor_enable)
    *(.text.xgnc__navigation__gnc_select_sun_versor)
    *(.text.xgnc__navigation__gnc_select_sun_versor_disable)
    *(.text.xgnc__navigation__gnc_ambiguous_sun_direction)
    *(.text.xgnc__navigation__gnc_ambiguous_sun_direction_disable)
    *(.text.cnt__init_seed)
    *(.text.cnt__test)
    *(.text.cnt__cnt_initialize)
    *(.text.cnt__random_integer)
    *(.text.cnt__sum)
  }  > ram 
  
  
  .text 0x40000000 : {
    /*KEEP (*(.text))*/
CREATE_OBJECT_SYMBOLS 
    *(.text) 
    *(.text.*)
    *(.lit)
    *(.rodata)
    *(.rodata.*)
    *(.shdata)
    *(.eh_frame)
    *(.gnu.linkonce.t*)
    *(.gnu.linkonce.r*)
    *(.text.ops__myop)
    *(.text.adafinal)
    *(.text.adainit)
  }  > ram
 
  .shbss : {
    *(.shbss)
  } > ram
 
  .talias : { }  > ram
 
  .data : {
    /* Make the clock frequency defined in this script available to the run
       time via this clock_frequency variable. */
    clock_frequency = .;
    LONG(_CLOCK_SPEED)
    *(.data)
    *(.data.*)
    *(.gnu.linkonce.d*)
 
    /* Ensure that the end of the data section is always word aligned.
       Initial values are stored in 4-bytes blocks so we must guarantee
       that these blocks do not fall out the section (otherwise they are
       truncated and the initial data for the last block are lost). */
 
    . = ALIGN(0x4);
  } > ram
 
  .bss : {
   __bss_start = ALIGN(0x8);
   *(.bss)
   *(.bss.*)
   *(COMMON)
 
  /* Align the stack to 64 bits */
 
   _stack_start = ALIGN(0x8);
 
   /* Reserve the space for the stack to be used by the environment task */ 
 
   . += _STACK_SIZE; 
 
   /* Pointer to the top of the stack to be used by the main procedure (the 
      environment task. */ 
 
   __stack = ALIGN(0x8); 
 
   _end = ALIGN(0x8); 
  } > ram 
 
  /* Stabs debugging sections.  */ 
  .stab          0 : { *(.stab) } 
  .stabstr       0 : { *(.stabstr) } 
  .stab.excl     0 : { *(.stab.excl) } 
  .stab.exclstr  0 : { *(.stab.exclstr) } 
  .stab.index    0 : { *(.stab.index) } 
  .stab.indexstr 0 : { *(.stab.indexstr) } 
  .comment       0 : { *(.comment) } 
  /* DWARF debug sections. 
     Symbols in the DWARF debugging sections are relative to the beginning 
     of the section so we begin them at 0.  */ 
  /* DWARF 1 */ 
  .debug          0 : { *(.debug) } 
  .line           0 : { *(.line) } 
  /* GNU DWARF 1 extensions */ 
  .debug_srcinfo  0 : { *(.debug_srcinfo) } 
  .debug_sfnames  0 : { *(.debug_sfnames) } 
  /* DWARF 1.1 and DWARF 2 */ 
  .debug_aranges  0 : { *(.debug_aranges) } 
  .debug_pubnames 0 : { *(.debug_pubnames) } 
  /* DWARF 2 */ 
  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) } 
  .debug_abbrev   0 : { *(.debug_abbrev) } 
  .debug_line     0 : { *(.debug_line) } 
  .debug_frame    0 : { *(.debug_frame) } 
  .debug_str      0 : { *(.debug_str) } 
  .debug_loc      0 : { *(.debug_loc) } 
  .debug_macinfo  0 : { *(.debug_macinfo) } 
  /* SGI/MIPS DWARF 2 extensions */ 
  .debug_weaknames 0 : { *(.debug_weaknames) } 
  .debug_funcnames 0 : { *(.debug_funcnames) } 
  .debug_typenames 0 : { *(.debug_typenames) } 
  .debug_varnames  0 : { *(.debug_varnames) } 
  /* DWARF 3 */ 
  .debug_pubtypes 0 : { *(.debug_pubtypes) } 
  .debug_ranges   0 : { *(.debug_ranges) } 
  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) } 
  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) } 
} 
 
/* Set the values that define the memory map */ 
 
rom_start = _PROM_START; 
rom_size = _PROM_SIZE; 
 
ram_start = _RAM_START; 
ram_size = _RAM_SIZE; 
 
heap_start = _end; 
heap_end = _RAM_END; 
 
/* END Linker script  */
