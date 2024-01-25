function-pool y369                      . "MESSAGE-ID ...
type-pools yub1.

*"----------------------------------------------------------------------
*"*"DECLARATIONS
*"----------------------------------------------------------------------
* where used declarations
* Types Declaration
* for TADIR
tables : trdir , tstc.
types : begin of t_tadir,
          object like yinp1-object,       "Object Type
          obj_name  like yinp1-obj_name,    "Object Name
        end of t_tadir.
types: begin of t_source,
         line(400) type c,
       end of t_source.
* structure for names
types: begin of t_reps,
*{   REPLACE        ISRK902085                                        1
*\         REPS_NAME(8) TYPE C,
         reps_name like trdir-name,
*}   REPLACE
       end of t_reps.

* internal table declarations
data : i_tadir type t_tadir occurs 0 with header line,
       i_tadir_prog type t_tadir occurs 0 with header line,
       i_tadir_fugr type t_tadir occurs 0 with header line,
       i_tadir_others type t_tadir occurs 0 with header line.
* internal table combindation of PROG & FUGR
data : begin of i_fugr_prog occurs 0,
          object  like yinp1-object,    "Object Type
          obj_name1 like yinp1-obj_name,   "FUGR or PROG
          obj_name2  like yinp1-obj_name,   "Object Name of FUGR/PROG
        end of i_fugr_prog.

* internal table for function module names
data : begin of i_func_mod occurs 0,
         object  like yout1-object,        "Object Type
         obj_name like yout1-obj_name,      "Object Name
         funcname like yout1-funcname,     "Function Module
         include  like yout1-include,        "Include
       end of i_func_mod.
* internal table for prog function & incl
data : begin of i_prg_fminc occurs 0,
         obj_name like yout1-obj_name,
         funcname like yout1-funcname,
         include  like yout1-include,
       end of i_prg_fminc.

* internal table for includes
data : begin of i_includes occurs 0,
         object like yout1-object,        "Object Type
         obj_name like yout1-obj_name,      "Object Name - Main
         include  like yout1-include,        "Include name
       end of i_includes.

* internal table for trdir
data : begin of i_trdir occurs 0 ,
         name like trdir-name,
         cnam like trdir-cnam,
         subc like trdir-subc,
       end of i_trdir.
* flag for WU
data :flg_wu(1) type c.
