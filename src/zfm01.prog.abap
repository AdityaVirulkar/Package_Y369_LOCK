*&---------------------------------------------------------------------*
*& Report  Z_FM_01                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

report  z_fm_01  no standard page heading
                 line-size 132
                 message-id su.                            .

* declarations
  tables : trdir, tadir.
* TADIR Objects - PROG & FUGR
  types : begin of t_tadir,
            object    like tadir-object,    "Object Type
            obj_name  like tadir-obj_name,    "Object Name
          end of t_tadir.
  types: begin of t_source,
           line(400) type c,
         end of t_source.
* structure for names
  types: begin of t_reps,
           reps_name(40) type c,
         end of t_reps.
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
           object  like tadir-object,    "Object Type
           obj_name like tadir-obj_name,  "Object Name
           funcname like enlfdir-funcname, "Function Module
*          include  like tadir-obj_name,    "Include
         end of i_func_mod.
* internal table for includes
  data : begin of i_includes occurs 0,
           object like tadir-object,    "Object Type
           obj_name like tadir-obj_name,  "Object Name - Main
           include  like tadir-obj_name,    "Include name
         end of i_includes.

* local variables
  data : l_objname like tadir-obj_name,
         l_namespace like rs38l-include,
         l_group like rs38l-area.

* Text file containing transaction list to be dowmloaded
selection-screen begin of block b1 with frame title text-t02.
parameters: p_upath like  rlgrap-filename obligatory.
selection-screen end of block b1.


* Text file containing transaction list to be dowmloaded
selection-screen begin of block b2 with frame title text-t02.
parameters: p_fpath like  rlgrap-filename obligatory.
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

  data :  l_text(70),
          cnt_objects(10) type c.

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
*    check prog object for includes
     perform f_get_includes using i_tadir_prog.
*    check prog object for function modules
     perform f_check_for_functions using i_tadir_prog ' '.
  endloop.

* process for all the includes - to get the function modules
  loop at i_includes.
    move : i_includes-object to i_tadir_prog-object,
           i_includes-include to i_tadir_prog-obj_name.
    append i_tadir_prog.
    perform f_check_for_functions using i_tadir_prog 'X'.
  endloop.


endform.                    " f_scan_prog


*&---------------------------------------------------------------------*
*&      Form  f_check_for_functions
*&---------------------------------------------------------------------*
*       Checks for the Function Modules
*----------------------------------------------------------------------*
*      -->P_TADIR_PROG  All Prog Objects
*      <--P_FLG         Flag
*----------------------------------------------------------------------*
form f_check_for_functions  using    p_tadir_prog type t_tadir
                                     p_flg.
* data declarations
  data: l_prog_name like trdir-name,
        l_validity_flag type c,
        l_object_owner type c,
        i_reps_source type t_source occurs 0,
        l_line type t_source,  "Source code line
        l_sandbox type t_source,  "Source code line sandbox
        i_string type t_source occurs 0,
        l_fdpos like sy-fdpos,  "Stored offset
        l_funcname like tfdir-funcname,
        l_reps_name like trdir-name. "Function module include
* constants
  constants:  c_comma    type c value ',',      " comma
              c_dquote   type c value '"',      " double quote
              c_squote   type c value ''''.     " single quote

  move p_tadir_prog-obj_name to l_prog_name.

* load the source code for the program object
  refresh i_reps_source.
  read report l_prog_name into i_reps_source.

* start of processing for each line of source code
  loop at i_reps_source into l_line.

*   ignore comment/documentation lines
    check l_line(1) ne '*'.

*   check line for 'CALL FUNCTION' statements
    if l_line cs 'CALL FUNCTION'.
*     when key statement is found, perform a couple preliminary checks
*     to ensure the occurence is not a comment or text...
      clear l_fdpos.
      l_fdpos  = sy-fdpos.         "Store offset where statement found
*     ...check for a comment marker before the command
      if l_line cs c_dquote.
        check l_fdpos lt sy-fdpos.
      endif.
*     ...check for a text marker before the command
      if l_line cs c_squote.
        check l_fdpos lt sy-fdpos.
      endif.
*     continue to next line of code when key statement is not identified
    else.
      continue.
    endif.

*   when the key statement is 'CALL FUNCTION', perform some processing
*   to identify the name of the called function module object...
*   ...identify offset of next token
    l_fdpos = l_fdpos + 14.
*   ...check cursor to ensure it is on the same line
    check l_fdpos le 72.
*   ...identify next token following key statement
    clear l_sandbox.
    l_sandbox = l_line+l_fdpos.
    shift l_sandbox left deleting leading space.
*   ...exclude token if dynamic call made to function module
    check l_sandbox(1) eq c_squote.
    shift l_sandbox left deleting leading c_squote.
    replace c_squote with space into l_sandbox.
*   ...split remaining strings in the sandbox into unique records
    refresh i_string.
    split l_sandbox at ' ' into table i_string.
*   ...identify the function module name (from the first string)
    clear l_funcname.
    read table i_string into l_funcname index 1.
    check sy-subrc eq 0.

*   retrieve the include program for the function module
    translate l_funcname to upper case.
    clear l_reps_name.
    call function 'FUNCTION_INCLUDE_INFO'
         changing
              funcname            = l_funcname
              include             = l_reps_name
         exceptions
              function_not_exists = 1
              others              = 6.
    check sy-subrc eq 0.

*   check include for validity and ownership
    perform f_validate_include using l_reps_name
                            changing l_validity_flag
                                     l_object_owner.

*   continue processing if include program is valid
    if l_validity_flag = 'X'.

*     perform additional processing based on include owner
      case l_object_owner.
*       log standard function  module object
        when 'C'.
          move l_funcname to i_func_mod-funcname.
          if p_flg eq 'X'. "coming from includes itab logic
            clear i_includes.
            read table i_includes with key
                       include = p_tadir_prog-obj_name.
            if sy-subrc eq 0.
              clear i_fugr_prog.
              if i_includes-object = 'FUGR'.
*               FUGR Objectname is fetched - field to refer obj_name1
                read table i_fugr_prog with key
                                   obj_name1 = i_includes-obj_name.
              elseif i_includes-object = 'PROG'.
*               PROG Objectname is fetched - field to refer obj_name2
                read table i_fugr_prog with key
                                   obj_name2 = i_includes-obj_name.
              endif.
                if sy-subrc eq 0.
*                 for FUGR - SAPL.... is fetched or
*                 for PROG - OBJECTNAME given as input is fetched
                  move i_fugr_prog-obj_name2 to p_tadir_prog-obj_name.
                endif.
            endif.
          endif.
          clear i_fugr_prog.
          read table i_fugr_prog with key
                                 obj_name2 = i_tadir_prog-obj_name.
          if sy-subrc eq 0.
            move : i_fugr_prog-object to i_func_mod-object.
*                   l_reps_name to i_func_mod-include.  "Include Name
            if i_fugr_prog-object eq 'FUGR'.
              move i_fugr_prog-obj_name1 to i_func_mod-obj_name.
            else.
              move i_fugr_prog-obj_name2 to i_func_mod-obj_name.
            endif.
          endif.
          append i_func_mod.
        when others.
      endcase.

    endif.

    clear: l_funcname, l_reps_name, l_validity_flag, l_object_owner.

  endloop.

  clear : i_reps_source,
          l_line,
          l_sandbox,
          i_string.
endform.                    " f_check_for_functions
*&---------------------------------------------------------------------*
*&      Form  f_validate_include
*&---------------------------------------------------------------------*
*       Validate the Include name
*----------------------------------------------------------------------*
*      -->P_REPS_NAME  Program Name
*      <--P_VALIDITY_FLAG  Flag
*      <--P_OBJECT_OWNER  Owner - SAP or Customer
*----------------------------------------------------------------------*
form f_validate_include  using    p_reps_name
                         changing p_validity_flag
                                  p_object_owner.

  data: l_devclass like tadir-devclass.
  data : begin of i_trdir occurs 0,
           name like trdir-name,
         end of i_trdir.
  clear p_validity_flag.

* check that include program object exists in the program repository
  select name
         from trdir
         into table i_trdir
         where name = p_reps_name.

  check sy-subrc eq 0.
  p_validity_flag = 'X'.

* Retrieve the development class of the program object
  clear l_devclass.
  call function 'RS_PROGRAM_GET_DEVCLASS'
       exporting
            progname = p_reps_name
       importing
            devclass = l_devclass.

* Determine whether the program object belongs to customer namespace
  if l_devclass(1) eq 'Y'
  or l_devclass(1) eq 'Z'
  or l_devclass is initial
  or l_devclass(4) eq '$TMP'.
    p_object_owner = 'C'.

* Otherwise object belongs to SAP namespace
  else.
    p_object_owner = 'S'.
  endif.

  clear : l_devclass,
          i_trdir.
  refresh : i_trdir.
endform.                    " f_validate_include


*&---------------------------------------------------------------------*
*&      Form  f_get_includes
*&---------------------------------------------------------------------*
*       Checks for all the Includes
*----------------------------------------------------------------------*
*      -->P_TADIR_PROG  All PROG type Objects
*----------------------------------------------------------------------*
form f_get_includes  using    p_tadir_prog type t_tadir.
* data declarations
  data: i_object_reps type t_reps occurs 0 with header line,
        l_object_reps type t_reps,
        l_prog_name like trdir-name,
        l_validity_flag type c,
        l_object_owner type c.

  clear: i_object_reps, l_object_reps.
  refresh i_object_reps.
* move the prog name to a var
  move p_tadir_prog-obj_name to l_prog_name.
* get all the includes for the program object
* Get includes for custom program object
  call function 'RS_GET_ALL_INCLUDES'
       exporting
            program      = l_prog_name
       tables
            includetab   = i_object_reps
       exceptions
            not_existent = 1
            no_program   = 2
            others       = 3.
  if sy-subrc <> 0.
    exit.
  endif.

* now process for each include object
  loop at i_object_reps.
*   check include for validity and ownership
    perform f_validate_include  using i_object_reps-reps_name
                             changing l_validity_flag
                                      l_object_owner.

*   continue processing if include program is valid
    if l_validity_flag = 'X'.
*     perform processing based on owner
      case l_object_owner.
        when 'C'.    "for Custom Objects
*         log the standard include object
          clear i_fugr_prog.
          read table i_fugr_prog with key
                             obj_name2 = p_tadir_prog-obj_name.
          if sy-subrc eq 0.
*           object
            move  i_fugr_prog-object to i_includes-object.
*           object name
            if i_fugr_prog-object eq 'FUGR'.
              move i_fugr_prog-obj_name1 to i_includes-obj_name.
            else.
              move i_fugr_prog-obj_name2 to i_includes-obj_name.
            endif.
          endif.
*         include name
          move i_object_reps-reps_name to i_includes-include.
          append i_includes.
        when others.
*         do nothing
      endcase.
    endif.
    clear: l_validity_flag, l_object_owner.
  endloop.

endform.                    " f_get_includes

*&---------------------------------------------------------------------*
*&      Form  f_download_data
*&---------------------------------------------------------------------*
form f_download_data .

 call function 'WS_DOWNLOAD'
     exporting
          filename            = p_fpath
          filetype            = 'ASC'
      tables
           data_tab            = i_func_mod
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
