*&---------------------------------------------------------------------*
*& Report  ZCAMS_TABLES                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

report  zcams_tables    no standard page heading
                        line-size 132
                        message-id su.

* Dummy Parameter
selection-screen begin of block b1 with frame title text-t01.
parameters: p_fpath like  rlgrap-filename  default text-d02.
selection-screen end of block b1.


* data declarations
  data : begin of i_tadir occurs 0,
           object like tadir-object,
           obj_name like tadir-obj_name,
         end of i_tadir.

  data : begin of i_tadir_fugr occurs 0,
           object like tadir-object,
           obj_name like tadir-obj_name,
         end of i_tadir_fugr.
  data : begin of i_results occurs 0,
           obj_name like tadir-obj_name,
         end of i_results.

* local variables
  data : l_objname like tadir-obj_name,
         l_namespace like rs38l-include,
         l_group like rs38l-area.

  types: begin of t_source,
           line(400) type c,
         end of t_source.
  data: i_reps_source type t_source occurs 0,
        l_line type t_source.
  tables : tadir.
  data :  l_text(70),
          cnt_objects(10) type c.

  start-of-selection.

* get custom PROG and FUGR

* PROG
  select object
         obj_name
         from tadir
         into table i_tadir
         where object eq 'PROG' and
         ( obj_name like 'Z%' or
           obj_name like 'Y%' or
           obj_name like 'SAPMZ%' or
           obj_name like 'SAPMY%' or
           obj_name like 'MZ%' or
           obj_name like 'MY%' or
           obj_name like 'SAPLZ%' or
           obj_name like 'SAPLY%' or
           obj_name like 'LZ%' or
           obj_name like 'LY%' or
           obj_name like 'SAPDZ%' or
           obj_name like 'SAPDY%' or
           obj_name like 'DY%' or
           obj_name like 'DZ%' or
           obj_name like 'MP9%' or
           obj_name like 'SAPFZ%' or
           obj_name like 'SAPFY%' or
           obj_name like 'FY%' or
           obj_name like 'FZ%' or
           obj_name like 'SAPUZ%' or
           obj_name like 'SAPUY%' or
           obj_name like 'UZ%' or
           obj_name like 'UY%' ) and
           author ne 'SAP' and
           devclass ne '$TMP'.


* FUGR
  select object
         obj_name
         from tadir
         into table i_tadir_fugr
         where object eq 'FUGR' and
            ( obj_name like 'Z%' or
              obj_name like 'Y%' or
              obj_name like 'SAPMZ%' or
              obj_name like 'SAPMY%' or
              obj_name like 'MZ%' or
              obj_name like 'MY%' or
              obj_name like 'SAPLZ%' or
              obj_name like 'SAPLY%' or
              obj_name like 'LZ%' or
              obj_name like 'LY%' or
              obj_name like 'SAPDZ%' or
              obj_name like 'SAPDY%' or
              obj_name like 'DY%' or
              obj_name like 'DZ%' or
              obj_name like 'MP9%' or
              obj_name like 'SAPFZ%' or
              obj_name like 'SAPFY%' or
              obj_name like 'FY%' or
              obj_name like 'FZ%' or
              obj_name like 'SAPUZ%' or
              obj_name like 'SAPUY%' or
              obj_name like 'UZ%' or
              obj_name like 'UY%' ) and
              author ne 'SAP' and
              devclass ne '$TMP'.

* convert the FUGR to PROG
  loop at i_tadir_fugr.
*   check whether the function group name has namespace
    l_objname = i_tadir_fugr-obj_name.
    if l_objname cs '/'.
*     now determine the namespace as well as the object name
      if l_objname(1) = '/'.
        shift l_objname left by 1 places.
        if l_objname ca '/'.
*         increment by 1 to get the end delimiter
          shift l_objname left by sy-fdpos places.
          l_group = l_objname.
*         increment by 1 to get the whole string with /
          add 1 to sy-fdpos.
*         gets the namespace
          l_namespace = i_tadir_fugr-obj_name(sy-fdpos).
        endif.
      endif.
    else.
*     no namespace then function group is the program object
      l_group = l_objname.
    endif.
    if l_group ne space.
*     change the pgmid and object type
      move 'PROG' to i_tadir-object.
      concatenate l_namespace
                  'SAPL'
                  l_group
                  into i_tadir-obj_name.
      append i_tadir.
    endif.
    clear : l_group,
            l_namespace.
  endloop.

  clear l_objname.

  loop at i_tadir.
    clear : l_objname,
            i_reps_source,
            l_line.
    refresh : i_reps_source.
    l_objname = i_tadir-obj_name.

*   show the gui status bar to avoid short dumps
      add 1 to cnt_objects.
      concatenate 'Processing done for'
                    cnt_objects
                    'Objects'
                    into l_text
                    separated by space.
      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          text   = l_text
        exceptions
          others = 1.
      if sy-batch = 'X'.
        message i000 with l_text.
      endif.

*   Read the source code of the program object
    read report l_objname into i_reps_source.
    loop at i_reps_source into l_line.
*   ignore comment/documentation lines
      check l_line(1) ne '*'.
        if l_line cs 'SADR' or
           l_line cs 'SADR2' or
           l_line cs 'SADR3' or
           l_line cs 'SADR4' or
           l_line cs 'SADR5' or
           l_line cs 'SADR7' or
           l_line cs 'SADR8' or
           l_line cs 'SADR10' or
           l_line cs 'USR03'.
           i_results-obj_name = i_tadir-obj_name.
           append i_results.
           clear i_results.
           continue.
        endif.
    endloop.
  endloop.

  sort i_results by obj_name.
  delete adjacent duplicates from i_results comparing obj_name.
*  loop at i_results.
*     write :/10 i_results-obj_name.
*  endloop.
   call function 'WS_DOWNLOAD'
       exporting
            filename            = p_fpath
            filetype            = 'ASC'
        tables
             data_tab            = i_results
        exceptions
             file_open_error     = 1
             file_write_error    = 2
             invalid_filesize    = 3
             invalid_table_width = 4
             invalid_type        = 5
             no_batch            = 6
             unknown_error       = 7
             others              = 8.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  skip.
  write :/03 'File downloaded to path:',
          30 p_fpath.


  clear : l_text,
          cnt_objects,
          i_tadir,
          i_tadir_fugr,
          i_results.
  refresh : i_tadir,
            i_tadir_fugr,
            i_results.
