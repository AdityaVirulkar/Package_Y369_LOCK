FUNCTION y_wu_01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_OBJECTS STRUCTURE  YINP1
*"      O_RESULTS STRUCTURE  YOUT1
*"      WU_ID STRUCTURE  Y369RET
*"----------------------------------------------------------------------

* refresh the global tables.
  PERFORM f_clear_memory_where.
  CLEAR o_results.  REFRESH o_results.

** Start of change by Taran
  READ TABLE i_objects INDEX 1.
** End of change by Taran

* get the objects list which is given as input into TADIR table
  MOVE : i_objects-object TO i_tadir-object,
         i_objects-obj_name TO i_tadir-obj_name.
  APPEND i_tadir.
  PERFORM f_get_custom_objects.

* determine the program name for each function group and
* add it to the same PROG object set for processing
  PERFORM f_convert_fugr_prog.

* process the objects for std FMs and std Includes
  PERFORM f_process_objects.

  SORT i_func_mod BY object obj_name funcname include.
  DELETE ADJACENT DUPLICATES FROM i_func_mod COMPARING
                  object obj_name funcname include.

* append the FMs
  LOOP AT i_func_mod.
*   detailed results
    MOVE i_func_mod TO o_results.
    APPEND o_results. CLEAR o_results.
  ENDLOOP.

* append  the includes
  LOOP AT i_includes.
*   detailed results
    MOVE : i_includes-object TO o_results-object,
           i_includes-obj_name TO o_results-obj_name,
           i_includes-include TO o_results-include.
    APPEND o_results. CLEAR o_results.
  ENDLOOP.
* set the flag
  IF flg_wu EQ 'T'.
    wu_id-y369flag = 'W'.
    APPEND wu_id.
  ENDIF.




ENDFUNCTION.
