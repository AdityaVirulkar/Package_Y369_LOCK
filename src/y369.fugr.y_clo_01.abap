FUNCTION y_clo_01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_OBJECTS STRUCTURE  YINP1
*"      O_RESULTS STRUCTURE  YOUT1
*"      CLONE_ID STRUCTURE  Y369RET
*"----------------------------------------------------------------------
* Clear variables and tables
  DATA: BEGIN OF ltab_include OCCURS 0.
          INCLUDE STRUCTURE d010inc.
  DATA: END OF ltab_include.
  DATA: BEGIN OF includetab OCCURS 0.
          INCLUDE STRUCTURE d010inc.
  DATA: END OF includetab.
  DATA: temp_trdir LIKE trdir OCCURS 0 WITH HEADER LINE.
  DATA: l_devclass LIKE tadir-devclass.
  DATA: l_program_type LIKE trdir-subc,
        l_include(40) TYPE c,
        l_keyword(7) TYPE c,
        p_cln_flag TYPE c,
        i_string TYPE t_source OCCURS 0,
        l_line TYPE t_source,  "Source code line
        l_sandbox TYPE t_source,  "Source code line sandbox
        i_reps_source TYPE t_source OCCURS 0,
        l_fdpos LIKE sy-fdpos,  "Stored offset
        l_validity_flag TYPE c,
        l_object_owner TYPE c,
        l_funcname LIKE tfdir-funcname,
        l_reps_name LIKE trdir-name, "Function module include
        lv_obj LIKE sy-repid.
  DATA: l_text  LIKE sy-ucomm.
  DATA: l_group LIKE rs38l-area.
  DATA: l_complete_area LIKE rs38l-area.
  DATA: l_namespace LIKE rs38l-namespace.
  DATA: fg_flag(1) TYPE c.
  DATA: cloneflag(1) TYPE c.
*  tables: trdir.
  REFRESH : ltab_include,
            includetab,
            temp_trdir,
            o_results.
  CLEAR: ltab_include,
         includetab,
         temp_trdir,
         l_devclass,
         l_program_type,
         l_include,
         l_keyword,
         p_cln_flag,
         i_string,
         l_line,
         l_sandbox,
         i_reps_source,
         l_fdpos,
         l_validity_flag,
         l_object_owner,
         l_funcname,
         l_reps_name,
         l_text,
         l_group,
         l_complete_area,
         l_namespace,
         fg_flag,
         lv_obj.

  cloneflag = ' '.
  IF i_objects-object = 'FUGR'.
    fg_flag = 'X'.
    l_complete_area = i_objects-obj_name.

*  Determine if the function group has a namespace prefix
    PERFORM f_check_fugr_for_namespace
              USING    l_complete_area
              CHANGING l_namespace
                       l_group.
*  Subroutine successful.  Build function group main program name
    IF l_group NE space.
      CLEAR i_objects.
      CONCATENATE l_namespace 'SAPL' l_group
                  INTO i_objects-obj_name.
      MOVE 'PROG' TO i_objects-object.
*     append i_objects.
      lv_obj = i_objects-obj_name.
*  Retrieve all the include program objects for each object
      CALL FUNCTION 'RS_GET_ALL_INCLUDES'
        EXPORTING
          program      = lv_obj
        TABLES
          includetab   = ltab_include
        EXCEPTIONS
          not_existent = 1
          no_program   = 2
          OTHERS       = 3.

      IF sy-subrc = 0.
        LOOP AT ltab_include.
          MOVE 'PROG' TO i_objects-object.
          MOVE ltab_include-master TO i_objects-obj_name.
          APPEND i_objects.
        ENDLOOP.
      ENDIF.
    ELSE.
*    MESSAGE I000 WITH TEXT-M09 L_COMPLETE_AREA.
    ENDIF.
* INSERT STARTS
  ELSEIF i_objects-object = 'PROG'.
    APPEND i_objects.
* INSERT ENDS
  ENDIF.

* Process program Objects
  LOOP AT i_objects.
    lv_obj = i_objects-obj_name.
    IF fg_flag = 'X'.
      CALL FUNCTION 'RS_PROGRAM_GET_DEVCLASS'
        EXPORTING
          progname = lv_obj
        IMPORTING
          devclass = l_devclass
        EXCEPTIONS
          OTHERS   = 1.
* Need to handle customer namespaces  Currently not enabled. Pass
* namespaces as a parameter and handle
      IF l_devclass(1) NE 'Y' AND
         l_devclass(1) NE 'Z' AND
         l_devclass(1) NE '$'.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR l_program_type.
    SELECT SINGLE subc INTO (l_program_type)
      FROM trdir
      WHERE name EQ i_objects-obj_name.
    CASE l_program_type.
      WHEN '1'.
        l_keyword = 'REPORT '.
      WHEN 'I'.
        l_keyword = 'INCLUDE'.
      WHEN 'M' OR 'S'.
        l_keyword = 'PROGRAM '.
      WHEN OTHERS.
        p_cln_flag = 'I'.
        EXIT.
    ENDCASE.
* Load the source code for the program object
    REFRESH i_reps_source.
    READ REPORT i_objects-obj_name INTO i_reps_source.
*   Start of processing for each line of source code
    DATA loopind TYPE i.
    loopind = 1.
    LOOP AT i_reps_source INTO l_line.
      loopind = loopind + 1.
*   Check line for keyword
      IF l_line CS l_keyword.
* When the key statement is found, and if the key statement is INCLUDE'
* and the line is not a comment line, then skip the line
        IF l_keyword EQ 'INCLUDE' AND l_line(1) NE '*'.
          CONTINUE.
        ENDIF.
* When the keyword is found more than four characters into theline,
*       then skip the line
        IF sy-fdpos GT 4.
          CONTINUE.
        ENDIF.
* When the keyword overruns the source code line, then skip the line
        l_fdpos = sy-fdpos + 7.
        IF l_fdpos GT 71.
          CONTINUE.
        ENDIF.
* Isolate object name following the keyword in the source code line
        l_sandbox = l_line+l_fdpos.
        SHIFT l_sandbox RIGHT DELETING TRAILING ' .'.
        SHIFT l_sandbox LEFT DELETING LEADING space.
        SPLIT l_sandbox AT ' ' INTO TABLE i_string.
        CLEAR l_reps_name.
        READ TABLE i_string INTO l_reps_name INDEX 1.
        CHECK sy-subrc EQ 0.
        TRANSLATE l_reps_name TO UPPER CASE.
* Continue processing when the object name (in the code) is different
* than the object name registered in the repository
        CHECK l_reps_name NE i_objects-obj_name.
        REFRESH temp_trdir.
        CLEAR temp_trdir.
        SELECT SINGLE * FROM trdir INTO temp_trdir
        WHERE name = l_reps_name.
        CHECK sy-subrc EQ 0.


* Retrieve the development class of the program object
        CLEAR l_devclass.
        CALL FUNCTION 'RS_PROGRAM_GET_DEVCLASS'
          EXPORTING
            progname = l_reps_name
          IMPORTING
            devclass = l_devclass.
*  Need to add customer namespaces /CLA
*    Determine whether the program object belongs to customer namespace
        IF l_devclass(1) EQ 'Y'
        OR l_devclass(1) EQ 'Z'
        OR l_devclass IS INITIAL
        OR l_devclass(4) EQ '$TMP'.
          l_object_owner = 'C'.

*       Otherwise object belongs to SAP namespace
        ELSE.
          l_object_owner = 'S'.
          cloneflag = 'X'.
          o_results-object   = i_objects-object.
          o_results-obj_name = i_objects-obj_name.
          o_results-funcname = ' '.
          o_results-include  = l_reps_name.
          APPEND o_results.
          CLEAR o_results.
        ENDIF.
      ENDIF.
      CLEAR: l_funcname, l_reps_name, l_validity_flag, l_object_owner.
    ENDLOOP.
  ENDLOOP.
  SORT o_results.
  DELETE ADJACENT DUPLICATES FROM o_results.
  IF cloneflag EQ 'X'.
    clone_id-y369flag = 'C'.
    APPEND clone_id.
  ENDIF.
  CLEAR i_objects.
  REFRESH i_objects.


ENDFUNCTION.
