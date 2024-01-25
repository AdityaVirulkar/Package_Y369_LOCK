*&---------------------------------------------------------------------*
*& Report  Z_IN_01                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

report  z_in_01  no standard page heading
                 line-size 132
                 message-id su.                            .

* declarations
tables : tadir.
* TADIR Objects - PROG & FUGR
  types : begin of t_tadir,
            object    like tadir-object,    "Object Type
            obj_name  like tadir-obj_name,    "Object Name
          end of t_tadir.

* internal table declarations
  data : i_tadir type t_tadir occurs 0 with header line,
         i_tadir_prog type t_tadir occurs 0 with header line,
         i_tadir_fugr type t_tadir occurs 0 with header line,
         i_tadir_others type t_tadir occurs 0 with header line.

* internal table combindation of PROG & FUGR
  data : begin of i_fugr_prog occurs 0,
            object    like tadir-object,    "Object Type
            obj_name1  like tadir-obj_name,    "FUGR or PROG
            obj_name2  like tadir-obj_name,    "Object Name of FUGR/PROG
          end of i_fugr_prog.

* internal table for function module names
  data : begin of i_func_mod occurs 0,
           object like tadir-object,    "Object Type
           obj_name like tadir-obj_name,  "Object Name
           funcname like enlfdir-funcname, "Function Module
           include  like tadir-obj_name,    "Include
         end of i_func_mod.

* internal table for includes
  data : begin of i_includes occurs 0,
           object like tadir-object,    "Object Type
           obj_name like tadir-obj_name,  "Object Name
           include  like tadir-obj_name,    "Include
         end of i_includes.
* internal table for includes
  data : begin of i_all_incl occurs 0,
           object like tadir-object,    "Object Type
           include  like tadir-obj_name,    "Include
         end of i_all_incl.


* local variables
  data : l_objname like tadir-obj_name,
         l_namespace like rs38l-include,
         l_group like rs38l-area.

* Text file containing transaction list to be dowmloaded
selection-screen begin of block b1 with frame title text-t02.
parameters: p_upath like rlgrap-filename obligatory.
selection-screen end of block b1.


* Text file containing transaction list to be dowmloaded
selection-screen begin of block b2 with frame title text-t02.
parameters: p_fpath like rlgrap-filename obligatory.
selection-screen end of block b2.

at selection-screen on value-request for p_upath.
  perform f_get_ufilename.

at selection-screen on value-request for p_fpath.
  perform f_get_filename.

at selection-screen.
  if p_upath is initial.
    message e000 with 'Please eneter file path for Upload file'.
  endif.
  if p_fpath is initial.
    message e000 with 'Please eneter file path for download file'.
  endif.
start-of-selection.

  perform f_upload_data.

*  PERFORM f_fetch_data.

  perform f_get_custom_objects.

  perform f_fugr_to_prog.

  perform f_scan_prog.

  perform f_download_data.

*&---------------------------------------------------------------------*
*&      Form  f_upload_data
*&---------------------------------------------------------------------*
form f_upload_data .

* PROG & FUGR

  call function 'WS_UPLOAD'
   exporting
*     CODEPAGE                      = ' '
      filename                      = p_upath
      filetype                      = 'ASC'
*     HEADLEN                       = ' '
*     LINE_EXIT                     = ' '
*     TRUNCLEN                      = ' '
*     USER_FORM                     = ' '
*     USER_PROG                     = ' '
*     DAT_D_FORMAT                  = ' '
*   IMPORTING
*     FILELENGTH                    =
    tables
      data_tab                      = i_tadir
   exceptions
     conversion_error              = 1
     file_open_error               = 2
     file_read_error               = 3
     invalid_type                  = 4
     no_batch                      = 5
     unknown_error                 = 6
     invalid_table_width           = 7
     gui_refuse_filetransfer       = 8
     customer_error                = 9
     no_authority                  = 10
     others                        = 11
            .
  if sy-subrc <> 0.
   message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


endform.                    " f_upload_data

*&---------------------------------------------------------------------*
*&      Form  f_fetch_data
*&---------------------------------------------------------------------*
form f_fetch_data .

* PROG & FUGR
  select object
       obj_name
       from tadir
       into table i_tadir
       where ( object eq 'PROG' or
               object eq 'FUGR' ) and
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

endform.                    " f_fetch_data


*&---------------------------------------------------------------------*
*&      Form  f_get_custom_objects
*&---------------------------------------------------------------------*
*       the objectname & object type are categorized seperately
*----------------------------------------------------------------------*
form f_get_custom_objects .
* Objects from TADIR as input
* split the TADIR records by PROG FUGR and Others
  loop at i_tadir.
    case i_tadir-object.
      when 'PROG'.
        move i_tadir to i_tadir_prog.
        append i_tadir_prog.
      when 'FUGR'.
        move i_tadir to i_tadir_fugr.
        append i_tadir_fugr.
      when others.
        move i_tadir to i_tadir_others.
        append i_tadir_others.
    endcase.
  endloop.

endform.                    " f_get_custom_objects

*&---------------------------------------------------------------------*
*&      Form  f_fugr_to_prog
*&---------------------------------------------------------------------*
form f_fugr_to_prog .

* local variables
  data : l_objname like tadir-obj_name,
         l_namespace like rs38l-include,
         l_group like rs38l-area.
* temp itab
  data : i_temp type t_tadir occurs 0 with header line.
* move the PROG objects to Temp itab
  append lines of i_tadir_prog to i_temp.
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
      move i_tadir_fugr to i_tadir_prog.
    endif.
    if l_group ne space.
*     change the pgmid and object type
      move 'PROG' to i_tadir_prog-object.
      concatenate l_namespace
                  'SAPL'
                  l_group
                  into i_tadir_prog-obj_name.
      append i_tadir_prog.
*     move to FUGR to the combintaion table
      move : i_tadir_fugr-object to i_fugr_prog-object,      "FUGR
             i_tadir_fugr-obj_name to i_fugr_prog-obj_name1, "FUGR NAME
             i_tadir_prog-obj_name  to i_fugr_prog-obj_name2."FUGRs prg
             append i_fugr_prog.
    endif.
    clear : l_group,
            l_namespace.
  endloop.

* move to PROG to the combintaion table ..if at all PROG there
  loop at i_temp.
      move : i_temp-object to i_fugr_prog-object,      "PROG
             i_temp-obj_name to i_fugr_prog-obj_name2. "PROG Name
             append i_fugr_prog.
  endloop.
  clear : l_objname.

endform.                    " f_fugr_to_prog


*&---------------------------------------------------------------------*
*&      Form  f_scan_prog
*&---------------------------------------------------------------------*
form f_scan_prog .

  types: begin of t_source,
           line(400) type c,
         end of t_source.

  data: l_prog_name like trdir-name,
        l_program_type like trdir-subc,
        l_reps_name like trdir-name, "Function module include
        l_devclass like tadir-devclass.

  data :  l_text(70),
          cnt_objects(10) type c.

  data: begin of ltab_include occurs 0.
          include structure d010inc.
  data: end of ltab_include.


  refresh : ltab_include.

  clear: ltab_include,
         l_devclass,
         l_reps_name,
         l_text.



  loop at i_tadir_prog.

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

    move i_tadir_prog-obj_name to l_prog_name.

    refresh : ltab_include.

    call function 'RS_GET_ALL_INCLUDES'
      exporting
        program      = l_prog_name
      tables
        includetab   = ltab_include
      exceptions
        not_existent = 1
        no_program   = 2
        others       = 3.
   refresh : i_all_incl.
    if sy-subrc = 0.
      loop at ltab_include.
        move 'PROG' to i_all_incl-object.
        move ltab_include-master to i_all_incl-include.
        append i_all_incl.
      endloop.
    endif.

    loop at i_all_incl.
*   Retrieve the development class of the program object
          clear l_devclass.
          call function 'RS_PROGRAM_GET_DEVCLASS'
            exporting
              progname = i_all_incl-include
            importing
              devclass = l_devclass.
*    Need to add customer namespaces /CLA
*      Determine whether the program object belongs to customer namespac
          if l_devclass(1) eq 'Y'
          or l_devclass(1) eq 'Z'
          or l_devclass is initial
          or l_devclass(4) eq '$TMP'.
            i_includes-object   = i_tadir_prog-object.
            i_includes-obj_name = i_tadir_prog-obj_name.
            i_includes-include  = i_all_incl-include.
            append i_includes.
            clear i_includes.
          endif.
    endloop.
  endloop.
endform.                    " f_scan_prog

*&---------------------------------------------------------------------*
*&      Form  f_download_data
*&---------------------------------------------------------------------*
form f_download_data .

 call function 'WS_DOWNLOAD'
     exporting
          filename            = p_fpath
          filetype            = 'ASC'
      tables
           data_tab            = i_includes
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
  else.
    skip.
    write :/03 'File downloaded to path:',
            30 p_fpath.
  endif.

endform.                    " f_download_data
*&---------------------------------------------------------------------*
*&      Form  f_get_filename
*&---------------------------------------------------------------------*
form f_get_filename .
  call function 'F4_FILENAME'
** EXPORTING
**   PROGRAM_NAME        = sy-repid
**   DYNPRO_NUMBER       = SYST-DYNNR
**   FIELD_NAME          = ' '
   importing
     file_name           = p_fpath
            .
endform.                    " f_get_filename

*&---------------------------------------------------------------------*
*&      Form  f_get_ufilename
*&---------------------------------------------------------------------*
form f_get_ufilename .
  call function 'F4_FILENAME'
** EXPORTING
**   PROGRAM_NAME        = sy-repid
**   DYNPRO_NUMBER       = SYST-DYNNR
**   FIELD_NAME          = ' '
   importing
     file_name           = p_upath
            .
endform.                    " f_get_ufilename
