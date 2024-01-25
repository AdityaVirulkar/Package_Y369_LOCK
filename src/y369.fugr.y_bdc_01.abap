function y_bdc_01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_OBJECTS STRUCTURE  YINP1
*"      O_RESULTS STRUCTURE  YOUT2
*"      BDC_ID STRUCTURE  Y369RET
*"----------------------------------------------------------------------
  constants:  c_prog(4)  type c value 'PROG',
              c_fugr(4)  type c value 'FUGR',
              c_sapl(4)  type c value 'SAPL',
              c_squote   type c value '''',    "Single quote
              c_comma    type c value ','.
  data: i_code type t_source occurs 0.
  data: token_itab like stokex occurs 0 with header line,
        token_itab2 like token_itab occurs 0 with header line,
        stmt_itab like sstmnt occurs 0,
        tcode_itab like tstc occurs 0 with header line,
        l_index like sy-tabix.

* ***********INSERT STARTS***********************
* TSTC table
  data : begin of i_tstc occurs 0,
           tcode like tstc-tcode,
           pgmna like tstc-pgmna,
         end   of i_tstc.
  data: fugr_flag(1) type c,
        l_group like rs38l-area,
        l_complete_area like rs38l-area,
        l_namespace like rs38l-namespace,
        l_obj_name like tadir-obj_name,
        flg_known(1) type c.

* get the transaction codes of the system
  select tcode
         pgmna
         from tstc
         into table i_tstc
         where dypno gt 0.  "which has screens

* ***********INSERT ENDS*************************

* loop at i_objects.
* Prepare data variables and tables
    clear i_code. refresh i_code. free i_code.

* ***********INSERT STARTS***********************
    clear bdc_id-y369flag.

* Start of change by Taran
    read table i_objects index 1.
* End of change by Taran
    l_obj_name = i_objects-obj_name.
    if i_objects-object = 'FUGR'.
      fugr_flag = 'X'.
      l_complete_area = i_objects-obj_name.

*    Determine if the function group has a namespace prefix
        perform f_check_fugr_for_namespace
                  using    l_complete_area
                  changing l_namespace
                           l_group.
*    Subroutine successful.  Build function group main program name
        if l_group ne space.
          concatenate l_namespace 'SAPL' l_group
                      into l_obj_name.
*        MOVE 'PROG' TO i_objects-object.
*        append i_objects.
        endif.
      endif.

* ***********INSERT ENDS*************************

*   Read the source code of the program object
    read report l_obj_name into i_code.          "CHANGE
*{   REPLACE        ISRK901830                                        1
*\    SCAN ABAP-SOURCE i_code
*\      TOKENS INTO token_itab
*\      STATEMENTS INTO stmt_itab
*\      WITH INCLUDES.
    scan abap-source i_code
      tokens into token_itab
      statements into stmt_itab
      with includes with analysis.
*}   REPLACE
* (1) Check the source code for "CALL" statements
    token_itab2[] = token_itab[].
    loop at token_itab where str = 'CALL'.

* Retrieve the source following "CALL"
      l_index = sy-tabix + 1.
      read table token_itab index l_index.

* (1A) Check the source code for a "CALL TRANSACTION" statement
      if token_itab-str = 'TRANSACTION'.

* Flag the object record as using a batch method
        move 'B' to bdc_id-y369flag.
*        append bdc_id.

* Retrieve the source following "CALL TRANSACTION"
        l_index = l_index + 1.
        read table token_itab index l_index.

* Determine the transaction code...
* ...from a string literal
        if token_itab-type eq 'S'.
          shift token_itab-str left.
          replace c_squote with space into token_itab-str.

* ...from a data variable or constant
        else.
          perform f_find_value tables   token_itab2
                               changing token_itab-str.
        endif.

* Format the transaction code
        translate token_itab-str to upper case.
        tcode_itab-tcode = token_itab-str.
        append tcode_itab.

*  Start of change by Taran
    read table i_objects index 1.
* End of change by Taran

        o_results-object = i_objects-object.
        o_results-obj_name = i_objects-obj_name.
        o_results-tcode = token_itab-str.
        append o_results.
*       UNKNOWN LOGIC - set the flag
        if not token_itab-str is initial.
          if token_itab-str ne 'UNKNOWN'.
            flg_known = 'T'.
          endif.
        endif.
* (1B) Check the source code for a "CALL FUNCTION" statement
      elseif token_itab-str = 'FUNCTION'.

* Retrieve the source following "CALL FUNCTION"
        l_index = l_index + 1.
        read table token_itab index l_index.

* Determine the function module name from a string literal
* Note: Dynamic function module references not supported!
        if token_itab-type eq 'S'.
          shift token_itab-str left.
          replace c_squote with space into token_itab-str.

* (1B1) Check the source for a "CALL FUNCTION BDC_INSERT" statement
          if token_itab-str eq 'BDC_INSERT'.

* Flag the object record as using a batch method
            move 'B' to bdc_id-y369flag.

* Retrieve the source following the "CALL FUNCTION BDC.." statement
            l_index = l_index + 4.
            read table token_itab index l_index.

* Determine the transaction code...
* ...from a string literal
            if token_itab-type eq 'S'.
              shift token_itab-str left.
              replace c_squote with space into token_itab-str.

* ...from a data variable or constant
            else.
              perform f_find_value tables token_itab2
                                 changing token_itab-str.
            endif.

* Format the transaction code
            translate token_itab-str to upper case.
            tcode_itab-tcode = token_itab-str.
            append tcode_itab.
            o_results-object = i_objects-object.
            o_results-obj_name = i_objects-obj_name.
            o_results-tcode = token_itab-str.
            append o_results.
*           UNKNOWN LOGIC - set the flag
            if not token_itab-str is initial.
              if token_itab-str ne 'UNKNOWN'.
                flg_known = 'T'.
              endif.
            endif.
          endif.
        endif.
      endif.
    endloop.

* ***********INSERT STARTS***********************
*   when zero occurrences of batch methods were identified
*   in the object; cease processing of the object.
    if bdc_id-y369flag ne 'B'.
      exit.
    endif.

*   step1
*   Delete all tokens except literals; remove duplicate records; and
*   remove single quotes from the literals
    delete token_itab where type ne 'S'.
    sort token_itab by str.
    delete adjacent duplicates from token_itab comparing str.
    loop at token_itab.
      translate token_itab-str to upper case.
      shift token_itab-str left.
      replace '''' with space into token_itab-str.
      modify token_itab.
    endloop.
*   step2
*   Clean-up list of transaction codes identified in the object
    sort tcode_itab.
    delete adjacent duplicates from tcode_itab.
*   step3
*   Search literals defined in program for additional transaction codes
    loop at token_itab.

*   Check for literal value in current list of transaction codes
      read table tcode_itab with key tcode = token_itab-str.

*   When not previously identified, check for literal in list of system
*   transaction codes
      check sy-subrc ne 0.
      clear i_tstc.
      read table i_tstc
           with key tcode = token_itab-str
           binary search.

*   When a valid transaction code is found, validate the token's use as
*   transaction code
      check sy-subrc eq 0.
      perform f_find_value tables   token_itab2
                           changing token_itab-str.

*     Format the transaction code
      translate token_itab-str to upper case.
      tcode_itab-tcode = token_itab-str.
      append tcode_itab.
*     UNKNOWN LOGIC - set the flag
      if not token_itab-str is initial.
        flg_known = 'T'.
      endif.
*     APPEND TO FINAL RESULTS
      o_results-object = i_objects-object.
      o_results-obj_name = i_objects-obj_name.
      o_results-tcode = token_itab-str.
      append o_results.
    endloop.
*   STEP4
*   Clean-up list of transaction codes identified in the object
    delete tcode_itab where tcode is initial.
    sort tcode_itab.
    delete adjacent duplicates from tcode_itab.
*   unknown logic.
    if flg_known eq 'T'.
      delete o_results where tcode eq 'UNKNOWN'.
    endif.
* ***********INSERT ENDS*************************
* endloop.
  if bdc_id-y369flag eq 'B'.
    append bdc_id.
  endif.

endfunction.
