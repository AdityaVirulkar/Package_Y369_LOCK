FUNCTION y_obj_01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_OBJECTS STRUCTURE  YINP1
*"      O_RESULTS STRUCTURE  YCOMPLEX
*"----------------------------------------------------------------------
* data  declarations
* source code
  DATA : BEGIN OF i_code OCCURS 0,
           line(400),
         END OF i_code.
* keywords
  DATA : BEGIN OF i_keywords OCCURS 0,
           keyword(15),
         END OF i_keywords.
  DATA : i_tokens1 LIKE stokex OCCURS 0 WITH HEADER LINE,
         i_stmt1 LIKE sstmnt OCCURS 0 WITH HEADER LINE,
         i_tokens2 LIKE stokex OCCURS 0 WITH HEADER LINE,
         i_stmt2 LIKE sstmnt OCCURS 0 WITH HEADER LINE,
         i_tokens3 LIKE stokex OCCURS 0 WITH HEADER LINE,
         i_stmt3 LIKE sstmnt OCCURS 0 WITH HEADER LINE.
* versions
  DATA : BEGIN OF i_versions OCCURS 0,
           trkorr LIKE e071-trkorr,
           as4pos LIKE e071-as4pos,
           obj_name LIKE e071-obj_name,
         END OF i_versions.
* counters
  DATA : l_index LIKE sy-tabix,
         cnt_includes(10),
         cnt_funcmods(10),
         cnt_selects(10),
         cnt_performs(10),
         cnt_loops(10),
         cnt_dos(10),
         cnt_whiles(10),
         l_lines(10),
         l_total(10),
         cnt_versions TYPE i,
         l_objname LIKE tadir-obj_name.
* includes itabs
  DATA : BEGIN OF i_includes OCCURS 0,
           prog LIKE tadir-obj_name,
           lines TYPE i,
         END OF i_includes.
  DATA : BEGIN OF i_code3 OCCURS 0,
           line(400),
         END OF i_code3.
  DATA : flg_object_type(15) TYPE c.
  TABLES: tadir, e071.
***********************************************************************
* clear & refresh the global tables
  CLEAR : i_tadir,
          i_tadir_prog,
          i_tadir_fugr.
  REFRESH : i_tadir,
          i_tadir_prog,
          i_tadir_fugr.

* finding the complexity of the program
* assign the keywords which needs to be checked
  i_keywords-keyword = 'SELECT'.   "select stmts
  APPEND i_keywords.
  i_keywords-keyword = 'PERFORM'.  "performs
  APPEND i_keywords.
  i_keywords-keyword = 'INCLUDE'.  "includes
  APPEND i_keywords.
  i_keywords-keyword = 'CALL'.     "calls to function modules
  APPEND i_keywords.
  i_keywords-keyword = 'LOOP'.     "number of loops
  APPEND i_keywords.
  i_keywords-keyword = 'DO'.       "number of do loops
  APPEND i_keywords.
  i_keywords-keyword = 'WHILE'.    "number of while stmts
  APPEND i_keywords.

* Start of change by Taran
  READ TABLE i_objects INDEX 1.
* End of change by Taran

* get the input table to TADIR table
*  append lines of i_objects to i_tadir.
  MOVE : i_objects-object TO i_tadir-object,
         i_objects-obj_name TO i_tadir-obj_name.
  APPEND i_tadir.

  PERFORM f_get_custom_objects.
* determine the program name for each function group and
* add it to the same PROG object set for processing
  PERFORM f_convert_fugr_prog.

*  loop at i_tadir_prog.
*   logic starts
  l_objname = i_tadir_prog-obj_name.            "program name
*   get the number of occurences in E071
*   select count(*)
*   from e071
*   into cnt_versions
*   where obj_name = l_objname
*   group by obj_name.
*   endselect
  CLEAR : i_versions, cnt_versions.
  REFRESH : i_versions.
  SELECT trkorr
         as4pos
         obj_name
         FROM e071
         INTO TABLE i_versions
         WHERE obj_name = l_objname.
  DESCRIBE TABLE i_versions LINES cnt_versions.
*   read the source code of main program name
  CLEAR : i_tokens1,
          i_stmt1,
          i_tokens2,
          i_stmt2,
          i_code.
  REFRESH : i_tokens1,
            i_stmt1,
            i_tokens2,
            i_stmt2,
            i_code.

  READ REPORT l_objname INTO i_code.
  DESCRIBE TABLE i_code LINES l_total.
*   scan the source code
*{   REPLACE        ISRK901830                                        1
*\    scan abap-source i_code tokens into i_tokens1
*\                        statements into i_stmt1
*\                        with includes.

  SCAN ABAP-SOURCE i_code TOKENS INTO i_tokens1
                         STATEMENTS INTO i_stmt1
                         WITH INCLUDES WITH ANALYSIS.
*}   REPLACE
*   check for the keywords
*{   REPLACE        ISRK901830                                        2
*\    scan abap-source i_code tokens into i_tokens2
*\                        statements into i_stmt2
*\                        with includes
*\                        keywords from i_keywords.
  SCAN ABAP-SOURCE i_code TOKENS INTO i_tokens2
                      STATEMENTS INTO i_stmt2
                      WITH INCLUDES
                      KEYWORDS FROM i_keywords WITH ANALYSIS..
*}   REPLACE

*   process for the main program
  LOOP AT i_tokens2.
*     get all the include program names in an internal table
    IF i_tokens2-str EQ 'INCLUDE'.
      l_index = sy-tabix + 1.
      READ TABLE i_tokens2 INDEX l_index.
      CHECK i_tokens2-str NE 'STRUCTURE' AND
            i_tokens2-str NE '='.
      MOVE i_tokens2-str TO i_includes-prog.
      APPEND i_includes.
*       number of includes
      cnt_includes = cnt_includes + 1.
    ENDIF.
*     number of selects
    IF i_tokens2-str EQ 'SELECT'.
      cnt_selects = cnt_selects + 1.
*     number of function modules
    ELSEIF i_tokens2-str EQ 'CALL'.
      cnt_funcmods = cnt_funcmods + 1.
*     number of performs
    ELSEIF i_tokens2-str EQ 'PERFORM'.
      cnt_performs = cnt_performs + 1.
*     number of loops
    ELSEIF i_tokens2-str EQ 'LOOP'.
      cnt_loops = cnt_loops + 1.
*     number of do loops
    ELSEIF i_tokens2-str EQ 'DO'.
      cnt_dos = cnt_dos + 1.
*     number of while stmts
    ELSEIF i_tokens2-str EQ 'WHILE'.
      cnt_whiles = cnt_whiles + 1.
    ENDIF.
  ENDLOOP.

*   process for the includes
*   get the total number of lines of the main prog & includes
  LOOP AT i_includes.
    CLEAR : i_code3,
            i_stmt3,
            i_tokens3.
    REFRESH : i_code3,
            i_stmt3,
            i_tokens3.
    CLEAR l_lines.
    READ REPORT i_includes-prog INTO i_code3.
    DESCRIBE TABLE i_code3 LINES l_lines.
    i_includes-lines = l_lines.
    MODIFY i_includes INDEX sy-tabix.
    l_total = l_total + l_lines.        "Gives the Number of lines
*     scan the source code for function modules
*{   REPLACE        ISRK901830                                        3
*\      scan abap-source i_code3 tokens into i_tokens3
*\                        statements into i_stmt3
*\                        with includes
*\                        keywords from i_keywords.
    SCAN ABAP-SOURCE i_code3 TOKENS INTO i_tokens3
                      STATEMENTS INTO i_stmt3
                      WITH INCLUDES
                      KEYWORDS FROM i_keywords WITH ANALYSIS..
*}   REPLACE
    LOOP AT i_tokens3.
*       number of selects
      IF i_tokens3-str EQ 'SELECT'.
        cnt_selects = cnt_selects + 1.
*       number of function modules
      ELSEIF i_tokens3-str EQ 'CALL'.
        cnt_funcmods = cnt_funcmods + 1.
*       number of performs
      ELSEIF i_tokens3-str EQ 'PERFORM'.
        cnt_performs = cnt_performs + 1.
*       number of loops
      ELSEIF i_tokens3-str EQ 'LOOP'.
        cnt_loops = cnt_loops + 1.
*       number of dos
      ELSEIF i_tokens3-str EQ 'DO'.
        cnt_dos = cnt_dos + 1.
*       number of whiles
      ELSEIF i_tokens3-str EQ 'WHILE'.
        cnt_whiles = cnt_whiles + 1.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  o_results-lines = l_total.
  o_results-selects = cnt_selects.
  o_results-performs = cnt_performs.
  o_results-loops = cnt_loops.
  o_results-whiles = cnt_whiles.
  o_results-doloops = cnt_dos.
  o_results-obj_name = i_tadir_prog-obj_name.
************************************************************************
*   finding the object type classification
*   for main program name - check for batch usage
  CLEAR l_index.
  LOOP AT i_tokens1 WHERE str = 'TRANSACTION'
                       OR str = 'FUNCTION'.
    IF i_tokens1-str EQ  'TRANSACTION'.
      IF i_tokens1-type EQ 'S'.
        flg_object_type = 'Batch Usage'.
      ENDIF.
    ELSEIF i_tokens1-str EQ 'FUNCTION'.
      l_index = sy-tabix + 1.
      READ TABLE i_tokens1 INDEX l_index.
      IF i_tokens1-type EQ 'S' AND
         i_tokens1-str CS 'BDC_INSERT'.
        flg_object_type = 'Batch Usage'.
*        elseif i_tokens1-type eq 'S' and
*               i_tokens1-str CS 'WRITE_FORM' or
*               i_tokens1-str CS 'OPEN_FORM' or
*               i_tokens1-str CS 'START_FORM'.
*          flg_object_type = 'SAP Script'.
      ENDIF.
    ENDIF.
  ENDLOOP.
*   for all include programs - check for batch usage
  LOOP AT i_includes.
    CLEAR   : i_code3,
              i_tokens3,
              i_stmt3.
    REFRESH : i_code3,
              i_tokens3,
              i_stmt3.
    CLEAR l_index.
    READ REPORT i_includes-prog INTO i_code3.
*{   REPLACE        ISRK901830                                        4
*\      scan abap-source i_code3 tokens into i_tokens3
*\                        statements into i_stmt3
*\                        with includes.
    SCAN ABAP-SOURCE i_code3 TOKENS INTO i_tokens3
                      STATEMENTS INTO i_stmt3
                      WITH INCLUDES WITH ANALYSIS..
*}   REPLACE
    LOOP AT i_tokens3 WHERE str = 'TRANSACTION'
                         OR str = 'FUNCTION'.
      IF i_tokens3-str EQ  'TRANSACTION'.
        IF i_tokens3-type EQ 'S'.
          flg_object_type = 'Batch Usage'.
        ENDIF.
      ELSEIF i_tokens3-str EQ 'FUNCTION'.
        l_index = sy-tabix + 1.
        READ TABLE i_tokens3 INDEX l_index.
        IF i_tokens3-type EQ 'S' AND
           i_tokens3-str CS 'BDC_INSERT'.
          flg_object_type = 'Batch Usage'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
*   get the number of occurences in E071
*    select count(*)
*           from e071
*           into cnt_versions
*           where obj_name = i_tadir_prog-obj_name
*                 group by obj_name.
*    endselect.
*   number of occurences
  o_results-versions = cnt_versions.
*   assign the object type
  o_results-object_typ = flg_object_type.
*   object
  o_results-object = i_tadir_prog-object.
  APPEND o_results.
*  endloop.

* complexity calculation
  LOOP AT o_results.
    MOVE :  o_results-selects TO cnt_selects,
            o_results-performs TO cnt_performs,
            o_results-loops TO cnt_loops,
            o_results-whiles TO cnt_whiles,
            o_results-doloops TO cnt_dos,
            o_results-versions TO cnt_versions,
            o_results-lines TO l_total.

*   formulaes for calculating the complexity of the program
*   LOW
    IF cnt_selects LE '5' AND cnt_performs LE '10' AND
       cnt_loops LE '10'  AND cnt_whiles LE '5'    AND
       cnt_dos LE '5'     AND cnt_versions LE '5' AND
       l_total LE '2000'.
      o_results-complexity = 'LOW'.
*   MEDIUM
    ELSEIF cnt_selects GT '5'  AND cnt_selects LE '15' AND
           cnt_performs GT '10' AND  cnt_performs LE '20' AND
           cnt_loops GT '10'  AND  cnt_loops LE '20' AND
           cnt_whiles GT '5' AND cnt_whiles LE '10' AND
           cnt_dos GT '5'AND cnt_dos LE '10' AND
           cnt_versions GT '5' AND cnt_versions LE '10' AND
           l_total GT '2000'AND l_total LE '5000'.
      o_results-complexity = 'MEDIUM'.
*   COMPLEX
    ELSEIF cnt_selects GT '15' AND cnt_selects LE '30' AND
           cnt_performs GT '20' AND  cnt_performs LE '30' AND
           cnt_loops GT '20'  AND  cnt_loops LE '30' AND
           cnt_whiles GT '10' AND cnt_whiles LE '20' AND
           cnt_dos GT '10'AND cnt_dos LE '20' AND
           cnt_versions GT '10' AND cnt_versions LE '20' AND
           l_total GT '5000'AND l_total LE '10000'.
      o_results-complexity = 'HIGH'.
*   VERY COMPLEX
    ELSEIF cnt_selects GT '30' OR
           cnt_performs GT '30' OR
           cnt_loops GT '30'  OR
           cnt_whiles GT '20' OR
           cnt_dos GT '20'OR
           cnt_versions GT '20' OR
           l_total GT '10000'.
      o_results-complexity = 'VERY COMPLEX'.
    ENDIF.
    MODIFY o_results TRANSPORTING complexity
           WHERE obj_name = o_results-obj_name.
  ENDLOOP.
* free memory.
  CLEAR :l_index,
         cnt_includes,
         cnt_funcmods,
         cnt_selects,
         cnt_performs,
         cnt_loops,
         cnt_dos,
         cnt_whiles,
         l_lines,
         l_total,
         l_objname,
         flg_object_type,
         i_tadir,
         i_tadir_prog,
         i_tadir_fugr.

  REFRESH : i_code,
            i_keywords,
            i_tokens1,
            i_stmt1,
            i_tokens2,
            i_stmt2,
            i_tokens3,
            i_stmt3,
            i_includes,
            i_code3,
            i_tadir,
            i_tadir_prog,
            i_tadir_fugr.

  FREE    : i_code,
            i_keywords,
            i_tokens1,
            i_stmt1,
            i_tokens2,
            i_stmt2,
            i_tokens3,
            i_stmt3,
            i_includes,
            i_code3,
            i_tadir,
            i_tadir_prog,
            i_tadir_fugr.





ENDFUNCTION.
