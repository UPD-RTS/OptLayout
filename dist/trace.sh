#!/bin/sh
#
# Shell script to run Bound-T for the example program with
# assertions and output of DOT graphs.
# -fpu=sequential             \
#   -split		       \
#   -warn no_flow	       \
#   -warn no_symbol	       \
#   -table   		       \
#   -assert assert_no_err.txt   \
#   -arithmetic   \
# -assert assert_pro.txt		\
#    -assert assert_erc.txt		\
#   -assert assert_exceptions.txt		\

boundt_sparc -device=v8e			\
   -trap_base=40000000				\
   -code_base=40000000				\
   -code_ws=5					\
   -read_ws=5					\
   -write_ws=5					\
   -stack_read_ws=5				\
   -stack_write_ws=5				\
   -no_rw					\
   -no_fp				\
   -warn no_flow				\
   -warn no_symbol				\
   -rapitime=T1__TaskOp.xml			\
   -no_ipoints				\
   -assert bounds.txt		\
   ./main t1__taskop>trace_T1_res.txt

