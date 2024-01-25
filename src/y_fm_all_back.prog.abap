*&---------------------------------------------------------------------*
*& Report  Y_FM_ALL_BACK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


report  Y_FM_ALL_BACK no standard page heading
                line-size 1000
                message-id su.
************************** ALV Declarations **************************************
TYPE-POOLS: slis.DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      it_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      it_layout   TYPE slis_layout_alv.
CONSTANTS: ca_star(1) VALUE '*',
           ca_ast(1)  VALUE '"',
           ca_comma   VALUE ',',
           ca_dot     VALUE '.',
           c_x        VALUE 'X',
           ca_brac(1) VALUE '(' .
************************  Table Declaration  *************************************
tables:tadir , memsd_cust, tstc.
************************* Type Declarations   *************************************
* TADIR Objects - PROG & FUGR
types : begin of t_tadir_obj,
        object  like tadir-object,
        obj_name like tadir-obj_name,
       end of t_tadir_obj.

* BDC Results
types : begin of t_bdc,
         object like tadir-object,
         obj_name like tadir-obj_name,
         tcode like tstc-tcode,
         bdcflag(1) type c,
       end of t_bdc.

types : begin of t_tcodes,
        tcode type tstc-tcode,
        end of t_tcodes.

* Where Used Results
types : begin of t1_wu,
         object like tadir-object,
         obj_name like tadir-obj_name,
         funcname like enlfdir-funcname,
         include like tadir-obj_name,
         wuflag(1) type c,
       end of t1_wu.

types : begin of t_wu,
        funcname like enlfdir-funcname,
  end of t_wu.

* Clone Candidate Results
types : begin of t_clones,
         object like tadir-object,
         obj_name like tadir-obj_name,
         funcname like enlfdir-funcname,
         include like tadir-obj_name,
         cloneflag(1) type c,
       end of t_clones.

* Object Classification results
types : begin of t_obj,
         object like ycomplex-object,
         obj_name like ycomplex-obj_name,
         lines like ycomplex-lines,
         selects like ycomplex-selects,
         performs like ycomplex-performs,
         loops like ycomplex-loops,
         whiles like ycomplex-whiles,
         doloops like ycomplex-doloops,
         versions like ycomplex-versions,
         complexity like ycomplex-complexity,
         object_type like ycomplex-object_typ,
      end of t_obj.
* inventory
types : begin of t_inventory,
        object  like tadir-object,
        obj_name like tadir-obj_name,
        author like tadir-author,
        devclass like tadir-devclass,
       end of t_inventory.
types : begin of t_includes,
        include like tadir-obj_name,
        end of t_includes.
types : begin of t_enlfdir,
        funcname like enlfdir-funcname,
        active like enlfdir-active,
        generated like enlfdir-generated,
        end of t_enlfdir.
types : begin of t_fupararef,
        funcname like fupararef-funcname,
        r3state like fupararef-r3state,
        parameter like fupararef-parameter,
        paramtype like fupararef-paramtype,
        structure like fupararef-structure,
        defaultval like fupararef-defaultval,
        reference like fupararef-reference,
        optional like fupararef-optional,
        type like fupararef-type,
        class like fupararef-class,
        ref_class like fupararef-ref_class,
        end of t_fupararef.
types : begin of t_trdir,
        name like trdir-name,
        cnam like trdir-cnam,
        unam like trdir-unam,
        udat like trdir-udat,
        vern like trdir-vern,
        subc like trdir-subc,
        end of t_trdir.
* screen fields
types : begin of t_screen_output,
         tcode like tstc-tcode,
         programnm(40) type c,
         screen like tstc-dypno,
         fieldnm(132) type c,
        end of t_screen_output.
types : begin of i_cust,
        cust_n type string,
        end of i_cust.
types : begin of t_output,
        teststring(100) type c,
        end of t_output.
************************* Data Declarations   *************************************

data  : i_input_tcodes type standard table of t_tcodes,
        i_global_tcodes type standard table of t_tcodes,
        wa_input_tcodes type t_tcodes,
        wa_global_tcodes type t_tcodes.
data : i_tadir_obj type standard table of t_tadir_obj,
      wa_tadir_obj type t_tadir_obj.
data : i_bdc type standard table of t_bdc,
       wa_bdc type t_bdc.
data  : it_output type standard table of t_output,
        wa_output type t_output.
data : it_fms type standard table of t_wu,
      wa_fms type t_wu,
      it_includes type standard table of t_includes,
      wa_includes type t_includes,
      it_includes2 type standard table of t_includes,
      wa_includes2 type t_includes,
      it_enlfdir type standard table of t_enlfdir,
      wa_enlfdir type t_enlfdir,
      it_fupararef type  standard table of t_fupararef,
      wa_fupararef type t_fupararef,
      it_trdir type standard table of t_trdir,
      wa_trdir type t_trdir,
      i_wu type standard table of t1_wu,
      wa_wu type t1_wu.
data :i_clones type standard table of t_clones,
      wa_clones type t_clones.
data : i_obj type standard table of t_obj,
       wa_obj type t_obj.
data : cnt_tcode type i,
         "l_text(70),
         l_tabix(6) type c,
         l_string1(60).
data : begin of i_tcodes occurs 0,
         tcode(20) type c,
         pgmna(40) type c,
       end of i_tcodes.
* TSTC - with Program names
data : begin of i_tstc1 occurs 0,
        tcode like tstc-tcode,
        pgmna like tstc-pgmna,
        dypno like tstc-dypno,
       end   of i_tstc1.
* screen descriptions
data : begin of i_d020t occurs 0,
         prog like d020t-prog,
         dynr like d020t-dynr,
         lang like d020t-lang,
         dtxt like d020t-dtxt,
       end of i_d020t.
* transaction tree
data : begin of i_screens occurs 0,
         tcode like tstc-tcode,
         programnm(40) type c,
         screen like tstc-dypno,
         fieldnm(132) type c,
        end of i_screens.
data : i_screen_output type standard table of t_screen_output,
       wa_screen_output type t_screen_output.
data  begin of i_dynp_fields occurs 20.
        include structure rsdcf.
data  end of i_dynp_fields.
* screen lines
data  begin of i_lines occurs 20.
        include structure tline.
data  end of i_lines.
data : i_inventory type standard table of t_inventory,
       wa_inventory type t_inventory.
data : i_inventory1 type standard table of t_inventory,
       wa_inventory1 type t_inventory.
data : i_cust_tab type standard table of i_cust,
      wa_cust type i_cust,
      i_cust_all type string value '*',
      i_cust_name type string,
      i_n_space type string,
      i_n_space1 type string.
* Range
ranges: cust_namespace_rng for tadir-obj_name,
rng for tadir-obj_name.
* selection screen parameters
parameters: rb01 radiobutton group radi,  "Batch Usage
            rb02 radiobutton group radi,  "Where Used
            rb03 radiobutton group radi,  "Clone Candidate
            rb04 radiobutton group radi,  "Inventory
            rb05 radiobutton group radi,  "Object Classification
            rb06 radiobutton group radi,  "Enlfdir
            rb07 radiobutton group radi,  "Fupararef
            rb08 radiobutton group radi,  "Trdir
            rb09 radiobutton group radi,  "screendata
            rb10 radiobutton group radi,  "TCODE
            rb11 radiobutton group radi,  "FM
            rb12 radiobutton group radi.  "Includes
************************* Selection Screen   *************************************
selection-screen begin of block b2 with frame title text-t02.
*parameters: p_file like  rlgrap-filename  default text-d02.
select-options : n_space for memsd_cust-namespace no intervals.
selection-screen end of block b2.
* Customer namespace ranges
clear cust_namespace_rng.
cust_namespace_rng-sign = 'I'.
cust_namespace_rng-option = 'CP'.
cust_namespace_rng-low = 'Z*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'Y*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'LZ*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'LY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'SAPLZ*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'SAPLY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'DBZ*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'DBY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'MY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'MZ*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'SAPMY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'SAPMZ*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'DY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'DZ*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'MP9*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'SAPFZ*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'SAPFY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'FY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'FZ*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'SAPUZ*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'SAPUY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'UY*'.
append cust_namespace_rng.
cust_namespace_rng-low = 'UZ*'.
append cust_namespace_rng.


clear rng.
rng-sign = 'I'.
rng-option = 'CP'.
rng-low = 'Z*'.
append rng.
rng-low = 'Y*'.
append rng.

if n_space is not initial or n_space ne space.
  loop at n_space.
*    SPLIT n_space AT 'IEQ' INTO i_n_space n_space.
    concatenate n_space-low i_cust_all into i_n_space1.
    cust_namespace_rng-sign = 'I'.
    cust_namespace_rng-option = 'CP'.
    cust_namespace_rng-low = i_n_space1.
    append cust_namespace_rng.
    clear: i_n_space1,
           cust_namespace_rng.
  endloop.
endif.
************************* Start of Selection  *************************************
start-of-selection.
* fetch all the data ie INVENTORY from TADIR
  if rb04 eq 'X'.
    perform f_fetch_inventory.
  endif.
  if rb01 eq 'X'.     "Batch
    perform f_fetch_tadir.
    perform f_call_bdc.
    perform f_batch_catalog.
    perform f_display_batch.
  elseif rb02 eq 'X'.   "Where
    perform f_fetch_tadir.
    perform f_call_whereused.
    perform f_where_catalog.
    perform f_display_where.
  elseif rb03 eq 'X'.
    perform f_fetch_tadir.
    perform f_call_clones.
    perform f_clone_catalog.
    perform f_display_clone.
  elseif rb04 eq 'X'.
    perform f_fetch_inventory.
    perform f_inventory_catalog.
    perform f_display_inventory.
  elseif rb05 eq 'X'.
    perform f_fetch_tadir.
    perform f_call_obj.
    perform f_object_catalog.
    perform f_display_object.
  elseif rb06 eq 'X'.
    perform f_fetch_tadir.
    perform f_call_whereused.
    perform f_enlfdir.
    perform f_enlfdir_catalog.
    perform f_display_enlfdir.
  elseif rb07 eq 'X'.
    perform f_fetch_tadir.
    perform f_call_whereused.
    perform f_fupararef.
    perform f_fupa_catalog.
    perform f_display_fupa.
  elseif rb08 eq 'X'.
    perform f_fetch_tadir.
    perform f_call_whereused.
    perform f_call_clones.
    perform f_trdir.
    perform f_trdir_catalog.
    perform f_display_trdir.
  elseif rb09 eq 'X'.
    perform f_fetch_tadir.
    perform f_call_bdc.
    perform f_screendata.
    perform f_screen_catalog.
    perform f_display_screen.
 elseif rb10 eq 'X'.
    perform f_fetch_tadir.
    perform f_call_bdc.
    perform f_tcode_catalog.
    PERFORM f_display_tcode.
 elseif rb11 eq 'X'.
    perform f_fetch_tadir.
    perform f_call_whereused.
    perform f_FM_catalog.
    PERFORM f_display_FM.
  elseif rb12 eq 'X'.
    perform f_fetch_tadir.
    perform f_call_whereused.
    perform f_call_clones.
    perform f_include_catalog.
    PERFORM f_display_include.
  endif.
*&---------------------------------------------------------------------*
*&      Form  f_fetch_inventory
*&---------------------------------------------------------------------*
form f_fetch_inventory .
  select object
         obj_name
         author
         devclass
         from tadir
         into table i_inventory
         where obj_name in cust_namespace_rng and object = 'PROG'.
    SELECT object
          obj_name
          author
          devclass FROM TADIR into table i_inventory1 where obj_name in rng.
      LOOP at i_inventory1 into wa_inventory1.
        wa_inventory-object = wa_inventory1-object.
        wa_inventory-obj_name = wa_inventory1-obj_name.
        wa_inventory-author = wa_inventory1-author.
        wa_inventory-devclass = wa_inventory1-devclass.
        APPEND WA_INVENTORY to i_inventory.
        ENDLOOP.
       SORT I_inventory BY OBJECT OBJ_NAME .
   DELETE ADJACENT DUPLICATES FROM i_inventory COMPARING ALL FIELDS.
  if it_output is not initial.
    clear it_output.
  endif.
  loop at i_inventory into wa_inventory.
    concatenate wa_inventory-object wa_inventory-obj_name
    wa_inventory-author wa_inventory-devclass into wa_output separated by '|'.
    append wa_output to it_output.
  endloop.
endform.                    " f_fetch_inventory

*&---------------------------------------------------------------------*
*&      Form  f_fetch_tadir
*&---------------------------------------------------------------------*
form f_fetch_tadir .
  select object
         obj_name
         from tadir
         into table i_tadir_obj
         where ( object eq 'PROG' or
                 object eq 'FUGR' ) and
              ( obj_name in cust_namespace_rng )
              and AUTHOR NE 'SAP'.
endform.                    " f_fetch_tadir

*&---------------------------------------------------------------------*
*&      Form  f_call_bdc
*&---------------------------------------------------------------------*
form f_call_bdc .
  data : begin of o_results occurs 0,
           object like tadir-object,
           obj_name like tadir-obj_name,
           tcode like tstc-tcode,
         end of o_results.
  data : begin of o_flg occurs 0,
          bdcflag(1) type c,
         end of o_flg.
  data :  l_text(70),
          cnt_objects(10) type c.
  types : begin of t_temp,
            object like tadir-object,
            obj_name like tadir-obj_name,
          end of t_temp.
  data : i_temp type standard table of t_temp,
        wa_temp type t_temp.
  loop at i_tadir_obj into wa_tadir_obj.
    wa_temp-object = wa_tadir_obj-object.
    wa_temp-obj_name = wa_tadir_obj-obj_name.
    append wa_temp to i_temp.
    call function 'Y_BDC_01'
      tables
        i_objects = i_temp
        o_results = o_results
        bdc_id    = o_flg.
    if not o_results[] is initial.
      append lines of o_results to i_bdc.
    endif.
    clear : o_results,
            o_flg,
            i_temp,
            wa_temp.
    refresh : o_results,
              o_flg.
  endloop.
  clear : i_temp.
  if it_output is not initial.
    clear it_output.
  endif.
  loop at i_bdc into wa_bdc.
    wa_input_tcodes-tcode = wa_bdc-tcode.
    concatenate wa_bdc-object wa_bdc-obj_name wa_bdc-tcode wa_bdc-bdcflag into wa_output separated by '|'.
    append wa_output to it_output.
    append wa_input_tcodes to i_input_tcodes.
    sort i_input_tcodes descending.
    delete adjacent duplicates from i_input_tcodes.
    delete i_input_tcodes where tcode eq 'UNKNOWN'.
    delete i_input_tcodes where tcode+0(1)  = 'Z' or tcode+0(1) = 'Y'.
  endloop.
endform.                    " f_call_bdc

*&---------------------------------------------------------------------*
*&      Form  f_call_whereused
*&---------------------------------------------------------------------*

form f_call_whereused .
  data : begin of o_results occurs 0,
           object like tadir-object,
           obj_name like tadir-obj_name,
           funcname like enlfdir-funcname,
           include like tadir-obj_name,
         end of o_results.
  data : begin of o_flg occurs 0,
          wuflag(1) type c,
         end of o_flg.
  data :  l_text(70),
          cnt_objects(10) type c.
  types : begin of t_temp,
            object like tadir-object,
            obj_name like tadir-obj_name,
          end of t_temp.
  data : i_temp type standard table of t_temp,
        wa_temp type t_temp.
  loop at i_tadir_obj into wa_tadir_obj.
    wa_temp-object = wa_tadir_obj-object.
    wa_temp-obj_name = wa_tadir_obj-obj_name.
    append wa_temp to i_temp.
    call function 'Y_WU_01'
      tables
        i_objects = i_temp
        o_results = o_results
        wu_id     = o_flg.

    if not o_results[] is initial.
      append lines of o_results to i_wu.
    endif.

    clear : o_results,
            o_flg,
            i_temp,
            wa_temp.
    refresh : o_results,
              o_flg.
  endloop.
** changes for second extraction - FMs
  clear : i_temp.
  if it_output is not initial.
    clear it_output.
  endif.
  loop at i_wu into wa_wu.
    concatenate wa_wu-object wa_wu-obj_name wa_wu-funcname wa_wu-include wa_wu-wuflag into wa_output separated by '|'.
    append wa_output to it_output.
    wa_fms = wa_wu-funcname.
    wa_includes = wa_wu-include .
    append wa_fms to it_fms.
    append wa_includes to it_includes.
    sort it_fms descending.
    sort it_includes descending.
    delete adjacent duplicates from it_fms comparing all fields.
    delete adjacent duplicates from it_includes comparing all fields.
  endloop.
endform.                    " f_call_whereused


*&---------------------------------------------------------------------*
*&      Form  f_call_clones
*&---------------------------------------------------------------------*

form f_call_clones .
  data : begin of o_results occurs 0,
           object like tadir-object,
           obj_name like tadir-obj_name,
           funcname like enlfdir-funcname,
           include like tadir-obj_name,
         end of o_results.
  data : begin of o_flg occurs 0,
          cloneflag(1) type c,
         end of o_flg.

  data :  l_text(70),
          cnt_objects(10) type c.
  types : begin of t_temp,
            object like tadir-object,
            obj_name like tadir-obj_name,
          end of t_temp.
  data : i_temp type standard table of t_temp,
        wa_temp type t_temp.
  loop at i_tadir_obj into wa_tadir_obj.
    wa_temp-object = wa_tadir_obj-object.
    wa_temp-obj_name = wa_tadir_obj-obj_name.
    append wa_temp to i_temp.
    call function 'Y_CLO_01'
      tables
        i_objects = i_temp
        o_results = o_results
        clone_id  = o_flg.

   if not o_results[] is initial.
      append lines of o_results to i_clones.
    endif.
    clear : o_results,
            o_flg,
            i_temp,
            wa_temp.
    refresh : o_results,
              o_flg.
  endloop.
  clear : i_temp.
  if it_output is not initial.
    clear it_output.
  endif.
  loop at  i_clones into wa_clones.
    concatenate wa_clones-object wa_clones-obj_name
                wa_clones-funcname wa_clones-include
                wa_clones-cloneflag into wa_output separated by '|'.
    append wa_output to it_output.
    wa_includes2 = wa_clones-include.
    append wa_includes2 to it_includes2.
    delete adjacent duplicates from it_includes2 comparing all fields.
  endloop.

endform.                    " f_call_clones

*&---------------------------------------------------------------------*
*&      Form  f_call_obj
*&---------------------------------------------------------------------*
form f_call_obj .
  data : begin of o_results occurs 0,
           object like ycomplex-object,
           obj_name like ycomplex-obj_name,
           lines like ycomplex-lines,
           selects like ycomplex-selects,
           performs like ycomplex-performs,
           loops like ycomplex-loops,
           whiles like ycomplex-whiles,
           doloops like ycomplex-doloops,
           versions like ycomplex-versions,
           complexity like ycomplex-complexity,
           object_type like ycomplex-object_typ,
        end of o_results.
  data :  l_text(70),
          cnt_objects(10) type c.

  types : begin of t_temp,
           object like tadir-object,
           obj_name like tadir-obj_name,
         end of t_temp.
  data : i_temp type standard table of t_temp,
        wa_temp type t_temp.
  loop at i_tadir_obj into wa_tadir_obj.
    wa_temp-object = wa_tadir_obj-object.
    wa_temp-obj_name = wa_tadir_obj-obj_name.
    append wa_temp to i_temp.
    call function 'Y_OBJ_01'
      tables
        i_objects = i_temp
        o_results = o_results
      exceptions
        others    = 1.
    if not o_results[] is initial.
      append lines of o_results to i_obj.
    endif.
    clear : o_results,
            i_temp,
            wa_temp.
    refresh : o_results.
  endloop.

  if it_output is not initial.
    clear it_output.
  endif.

  loop at  i_obj into wa_obj.
    concatenate wa_obj-object wa_obj-obj_name wa_obj-lines wa_obj-selects wa_obj-performs wa_obj-loops
    wa_obj-whiles wa_obj-doloops wa_obj-versions
    wa_obj-complexity wa_obj-object_type into wa_output separated by '|'.
    append wa_output to it_output.
  endloop.
endform.                    " f_call_obj
*&---------------------------------------------------------------------*
*&      Form  F_ENLFDIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_enlfdir .
  if it_fms is not initial.
    select funcname active generated
      from enlfdir
      into table it_enlfdir
      for all entries in it_fms
      where funcname = it_fms-funcname.
  endif.
  if it_output is not initial.
    clear it_output.
  endif.

  loop at it_enlfdir into wa_enlfdir.
    concatenate wa_enlfdir-funcname wa_enlfdir-active wa_enlfdir-generated into wa_output separated by '|'.
    append wa_output to it_output.
  endloop.
endform.                    " F_ENLFDIR
*&---------------------------------------------------------------------*
*&      Form  F_FUPARAREF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_fupararef .
  if it_fms is not initial.
    select funcname
    r3state
    parameter
    paramtype
    structure
    defaultval
    reference
    optional
    type
    class
    ref_class from fupararef
      into table it_fupararef
      for all entries in it_fms
      where funcname = it_fms-funcname.
  endif.
  if it_output is not initial.
    clear it_output.
  endif.

  loop at  it_fupararef into wa_fupararef.
    concatenate wa_fupararef-funcname wa_fupararef-r3state
                wa_fupararef-parameter wa_fupararef-paramtype
                wa_fupararef-structure wa_fupararef-defaultval
                wa_fupararef-reference wa_fupararef-optional
                wa_fupararef-type wa_fupararef-class wa_fupararef-ref_class
                into wa_output separated by '|'.
    append wa_output to it_output.
  endloop.
endform.                    " F_FUPARAREF
*&---------------------------------------------------------------------*
*&      Form  F_TRDIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_trdir .

  append lines of it_includes to it_includes2.
  sort it_includes2 descending.
  delete adjacent duplicates from it_includes2.
  if it_includes2 is not initial.
    select
      name
      cnam
      unam
      udat
      vern
      subc from trdir
      into table it_trdir
      for all entries in it_includes2
      where name = it_includes2-include.
  endif.
  if it_output is not initial.
    clear it_output.
  endif.
  loop at it_trdir into wa_trdir.
    concatenate wa_trdir-name wa_trdir-cnam wa_trdir-unam wa_trdir-udat
                wa_trdir-vern wa_trdir-subc into wa_output separated by '|'.
    append wa_output to it_output.
  endloop.
endform.                    " F_TRDIR

*&---------------------------------------------------------------------*
*&      Form  F_SCREENDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_screendata .
* get the prognames & tcodes & screen number
* determine if the tcodes has associcated programs

  if i_input_tcodes is not initial.
    select tcode
         pgmna
         dypno
         from tstc
         into table i_tstc1
         for all entries in i_input_tcodes
    where tcode eq i_input_tcodes-tcode.
  endif.

* validate the screen number with table d020t
* this is to confirm whether the screen number is present or not as SAP
* can remove screens
  sort i_tstc1 by tcode.
  if not i_tstc1[] is initial.
    select prog
           dynr
           lang
           dtxt
           from d020t
           into table i_d020t
           for all entries in i_tstc1
           where prog eq i_tstc1-pgmna
             and lang = 'D'.
  endif.

  loop at i_tstc1.
*   process for all programs
    loop at i_d020t where prog = i_tstc1-pgmna
                      and lang = 'D'.
      refresh i_dynp_fields.

      clear i_screens.
*     tcode
      i_screens-tcode = i_tstc1-tcode.
*     program name
      i_screens-programnm = i_d020t-prog.
      append i_screens.
*     screen number
      i_screens-screen = i_d020t-dynr.
      append i_screens.
*     pass the program name and screen number
      call function 'DYNPRO_FIELD_GET'
        exporting
          dynpro           = i_d020t-dynr
          program          = i_d020t-prog
        tables
          dynp_fields      = i_dynp_fields
          lines            = i_lines
        exceptions
          dynpro_not_found = 1
          others           = 2.
      if sy-subrc eq 0.
        loop at i_dynp_fields.
          if i_dynp_fields-tabname <> ''.
*           create field records in the fields table
            clear i_screens.
*           field name
            i_screens-tcode = i_tstc1-tcode.
            i_screens-screen = i_d020t-dynr.
            i_screens-programnm = i_d020t-prog.
            i_screens-fieldnm = i_dynp_fields-dynpro_fld.
            append i_screens.
          endif.
        endloop.
*       get the bdc_okcode
        clear i_screens.
        i_screens-tcode = i_tstc1-tcode.
        i_screens-screen = i_d020t-dynr.
        i_screens-programnm = i_d020t-prog.
        i_screens-fieldnm = 'BDC_OKCODE'.
        append i_screens.
      endif.
    endloop.
  endloop.

  i_screen_output[] = i_screens[].
  if it_output is not initial.
    clear it_output.
  endif.
  loop at i_screen_output into wa_screen_output.
    concatenate wa_screen_output-tcode wa_screen_output-programnm
    wa_screen_output-screen wa_screen_output-fieldnm into wa_output separated by '|'.
    append wa_output to it_output.
  endloop.
endform.                    " F_SCREENDATA
*&---------------------------------------------------------------------*
*&      Form  F_BATCH_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_BATCH_CATALOG .

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'OBJECT'.
  wa_fieldcat-seltext_l = 'OBJECT TYPE'.
  wa_fieldcat-seltext_m = 'OBJECT TYPE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-fieldname = 'OBJ_NAME'.
  wa_fieldcat-seltext_l = 'OBJECT NAME'.
  wa_fieldcat-seltext_m = 'OBJECT NAME'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-fieldname = 'TCODE'.
  wa_fieldcat-seltext_l = 'TCODE'.
  wa_fieldcat-seltext_m = 'TCODE'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 4.
  wa_fieldcat-fieldname = 'BDCFLAG'.
  wa_fieldcat-seltext_l = 'ROW'.
  wa_fieldcat-seltext_m = 'ROW'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.



endform.                    " F_BATCH_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_BATCH .

  CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = i_bdc
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_BATCH
*&---------------------------------------------------------------------*
*&      Form  F_WHERE_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_WHERE_CATALOG .

CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'OBJECT'.
  wa_fieldcat-seltext_l = 'OBJECT TYPE'.
  wa_fieldcat-seltext_m = 'OBJECT TYPE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-fieldname = 'OBJ_NAME'.
  wa_fieldcat-seltext_l = 'OBJECT NAME'.
  wa_fieldcat-seltext_m = 'OBJECT NAME'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-fieldname = 'FUNCNAME'.
  wa_fieldcat-seltext_l = 'FUNCTION NAME'.
  wa_fieldcat-seltext_m = 'FUNCTION NAME'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 4.
  wa_fieldcat-fieldname = 'INCLUDE'.
  wa_fieldcat-seltext_l = 'INCLUDE'.
  wa_fieldcat-seltext_m = 'INCLUDE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 5.
  wa_fieldcat-fieldname = 'WUFLAG'.
  wa_fieldcat-seltext_l = 'WUFLAG'.
  wa_fieldcat-seltext_m = 'WUFLAG'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

endform.                    " F_WHERE_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_WHERE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_WHERE .

   CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = i_wu
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_WHERE
*&---------------------------------------------------------------------*
*&      Form  F_CLONE_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_CLONE_CATALOG .

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'OBJECT'.
  wa_fieldcat-seltext_l = 'OBJECT TYPE'.
  wa_fieldcat-seltext_m = 'OBJECT TYPE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-fieldname = 'OBJ_NAME'.
  wa_fieldcat-seltext_l = 'OBJECT NAME'.
  wa_fieldcat-seltext_m = 'OBJECT NAME'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-fieldname = 'FUNCNAME'.
  wa_fieldcat-seltext_l = 'FUNCTION NAME'.
  wa_fieldcat-seltext_m = 'FUNCTION NAME'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 4.
  wa_fieldcat-fieldname = 'INCLUDE'.
  wa_fieldcat-seltext_l = 'INCLUDE'.
  wa_fieldcat-seltext_m = 'INCLUDE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 5.
  wa_fieldcat-fieldname = 'CLONEFLAG'.
  wa_fieldcat-seltext_l = 'CLONEFLAG'.
  wa_fieldcat-seltext_m = 'CLONEFLAG'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.



endform.                    " F_CLONE_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_CLONE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_CLONE .

   CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = i_clones
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_CLONE
*&---------------------------------------------------------------------*
*&      Form  F_INVENTORY_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_INVENTORY_CATALOG .

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'OBJECT'.
  wa_fieldcat-seltext_l = 'OBJECT TYPE'.
  wa_fieldcat-seltext_m = 'OBJECT TYPE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-fieldname = 'OBJ_NAME'.
  wa_fieldcat-seltext_l = 'OBJECT NAME'.
  wa_fieldcat-seltext_m = 'OBJECT NAME'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-fieldname = 'AUTHOR'.
  wa_fieldcat-seltext_l = 'AUTHOR'.
  wa_fieldcat-seltext_m = 'AUTHOR'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 4.
  wa_fieldcat-fieldname = 'DEVCLASS'.
  wa_fieldcat-seltext_l = 'DEVCLASS'.
  wa_fieldcat-seltext_m = 'DEVCLASS'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.


endform.                    " F_INVENTORY_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_INVENTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_INVENTORY .

     CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = i_inventory
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_INVENTORY
*&---------------------------------------------------------------------*
*&      Form  F_OBJECT_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_OBJECT_CATALOG .

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'OBJECT'.
  wa_fieldcat-seltext_l = 'OBJECT TYPE'.
  wa_fieldcat-seltext_m = 'OBJECT TYPE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-fieldname = 'OBJ_NAME'.
  wa_fieldcat-seltext_l = 'OBJECT NAME'.
  wa_fieldcat-seltext_m = 'OBJECT NAME'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-fieldname = 'LINES'.
  wa_fieldcat-seltext_l = 'LINES'.
  wa_fieldcat-seltext_m = 'LINES'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 4.
  wa_fieldcat-fieldname = 'SELECTS'.
  wa_fieldcat-seltext_l = 'SELECTS'.
  wa_fieldcat-seltext_m = 'SELECTS'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 5.
  wa_fieldcat-fieldname = 'PERFORMS'.
  wa_fieldcat-seltext_l = 'PERFORMS'.
  wa_fieldcat-seltext_m = 'PERFORMS'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 6.
  wa_fieldcat-fieldname = 'LOOPS'.
  wa_fieldcat-seltext_l = 'LOOPS'.
  wa_fieldcat-seltext_m = 'LOOPS'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 7.
  wa_fieldcat-fieldname = 'WHILES'.
  wa_fieldcat-seltext_l = 'WHILES'.
  wa_fieldcat-seltext_m = 'WHILES'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 8.
  wa_fieldcat-fieldname = 'DOLOOPS'.
  wa_fieldcat-seltext_l = 'DOLOOPS'.
  wa_fieldcat-seltext_m = 'DOLOOPS'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 9.
  wa_fieldcat-fieldname = 'VERSIONS'.
  wa_fieldcat-seltext_l = 'VERSIONS'.
  wa_fieldcat-seltext_m = 'VERSIONS'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 10.
  wa_fieldcat-fieldname = 'COMPLEXITY'.
  wa_fieldcat-seltext_l = 'COMPLEXITY'.
  wa_fieldcat-seltext_m = 'COMPLEXITY'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 11.
  wa_fieldcat-fieldname = 'OBJECT_TYPE'.
  wa_fieldcat-seltext_l = 'OBJECT TYPE'.
  wa_fieldcat-seltext_m = 'OBJECT TYPE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.



endform.                    " F_OBJECT_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_OBJECT .

     CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = i_obj
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_OBJECT
*&---------------------------------------------------------------------*
*&      Form  F_ENLFDIR_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_ENLFDIR_CATALOG .

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'FUNCNAME'.
  wa_fieldcat-seltext_l = 'FUNCTION NAME'.
  wa_fieldcat-seltext_m = 'FUNCTION NAME'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-fieldname = 'ACTIVE'.
  wa_fieldcat-seltext_l = 'ACTIVE'.
  wa_fieldcat-seltext_m = 'ACTIVE'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-fieldname = 'GENERATED'.
  wa_fieldcat-seltext_l = 'GENERATED'.
  wa_fieldcat-seltext_m = 'GENERATED'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.


endform.                    " F_ENLFDIR_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ENLFDIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_ENLFDIR .

     CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = it_enlfdir
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_ENLFDIR
*&---------------------------------------------------------------------*
*&      Form  F_FUPA_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_FUPA_CATALOG .

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'FUNCNAME'.
  wa_fieldcat-seltext_l = 'FUNCTION NAME'.
  wa_fieldcat-seltext_m = 'FUNCTION NAME'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-fieldname = 'R3STATE'.
  wa_fieldcat-seltext_l = 'R3STATE'.
  wa_fieldcat-seltext_m = 'R3STATE'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-fieldname = 'PARAMETER'.
  wa_fieldcat-seltext_l = 'PARAMETER'.
  wa_fieldcat-seltext_m = 'PARAMETER'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 4.
  wa_fieldcat-fieldname = 'PARAMTYPE'.
  wa_fieldcat-seltext_l = 'PARAMETER TYPE'.
  wa_fieldcat-seltext_m = 'PARAMETER TYPE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 5.
  wa_fieldcat-fieldname = 'STRUCTURE'.
  wa_fieldcat-seltext_l = 'STRUCTURE'.
  wa_fieldcat-seltext_m = 'STRUCTURE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 6.
  wa_fieldcat-fieldname = 'DEFAULTVAL'.
  wa_fieldcat-seltext_l = 'DEFAULT VALUE'.
  wa_fieldcat-seltext_m = 'DEFAULT VALUE'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 7.
  wa_fieldcat-fieldname = 'REFERENCE'.
  wa_fieldcat-seltext_l = 'REFERENCE'.
  wa_fieldcat-seltext_m = 'REFERENCE'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 8.
  wa_fieldcat-fieldname = 'OPTIONAL'.
  wa_fieldcat-seltext_l = 'OPTIONAL'.
  wa_fieldcat-seltext_m = 'OPTIONAL'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.


    CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 9.
  wa_fieldcat-fieldname = 'TYPE'.
  wa_fieldcat-seltext_l = 'TYPE'.
  wa_fieldcat-seltext_m = 'TYPE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

    CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 10.
  wa_fieldcat-fieldname = 'CLASS'.
  wa_fieldcat-seltext_l = 'CLASS'.
  wa_fieldcat-seltext_m = 'CLASS'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

    CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 11.
  wa_fieldcat-fieldname = 'REF_CLASS'.
  wa_fieldcat-seltext_l = 'REFERENCE CLASS'.
  wa_fieldcat-seltext_m = 'REFERENCE CLASS'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

endform.                    " F_FUPA_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_FUPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_FUPA .

     CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = it_fupararef
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_FUPA
*&---------------------------------------------------------------------*
*&      Form  F_TRDIR_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_TRDIR_CATALOG .

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'NAME'.
  wa_fieldcat-seltext_l = 'NAME'.
  wa_fieldcat-seltext_m = 'NAME'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-fieldname = 'CNAM'.
  wa_fieldcat-seltext_l = 'CNAM'.
  wa_fieldcat-seltext_m = 'CNAM'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-fieldname = 'UNAM'.
  wa_fieldcat-seltext_l = 'UNAM'.
  wa_fieldcat-seltext_m = 'UNAM'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 4.
  wa_fieldcat-fieldname = 'UDAT'.
  wa_fieldcat-seltext_l = 'UDAT'.
  wa_fieldcat-seltext_m = 'UDAT'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 5.
  wa_fieldcat-fieldname = 'VERN'.
  wa_fieldcat-seltext_l = 'VERSION'.
  wa_fieldcat-seltext_m = 'VERSION'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 6.
  wa_fieldcat-fieldname = 'SUBC'.
  wa_fieldcat-seltext_l = 'SUBC'.
  wa_fieldcat-seltext_m = 'SUBC'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

endform.                    " F_TRDIR_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_TRDIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_TRDIR .

     CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = it_trdir
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_TRDIR
*&---------------------------------------------------------------------*
*&      Form  F_SCREEN_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_SCREEN_CATALOG .

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'TCODE'.
  wa_fieldcat-seltext_l = 'TCODE'.
  wa_fieldcat-seltext_m = 'TCODE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 2.
  wa_fieldcat-fieldname = 'PROGRAMNM'.
  wa_fieldcat-seltext_l = 'PROGRAM NAME'.
  wa_fieldcat-seltext_m = 'PROGRAM NAME'.
  wa_fieldcat-outputlen =  50.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-col_pos   = 3.
  wa_fieldcat-fieldname = 'SCREEN'.
  wa_fieldcat-seltext_l = 'SCREEN'.
  wa_fieldcat-seltext_m = 'SCREEN'.
  wa_fieldcat-outputlen =  20.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 4.
  wa_fieldcat-fieldname = 'FIELDNM'.
  wa_fieldcat-seltext_l = 'FIELD NAME'.
  wa_fieldcat-seltext_m = 'FIELD NAME'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.


endform.                    " F_SCREEN_CATALOG

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_SCREEN .

     CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = i_screen_output
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  F_TCODE_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_TCODE_CATALOG .
    CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'TCODE'.
  wa_fieldcat-seltext_l = 'TCODE'.
  wa_fieldcat-seltext_m = 'TCODE'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

endform.                    " F_TCODE_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_TCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_TCODE .
       CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = i_input_tcodes
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_TCODE
*&---------------------------------------------------------------------*
*&      Form  F_FM_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_FM_CATALOG .
    CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'FUNCNAME'.
  wa_fieldcat-seltext_l = 'FM'.
  wa_fieldcat-seltext_m = 'FM'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

endform.                    " F_FM_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_FM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_FM .
         CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = it_fms
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.

endform.                    " F_DISPLAY_FM
*&---------------------------------------------------------------------*
*&      Form  F_INCLUDE_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_INCLUDE_CATALOG .
   CLEAR: wa_fieldcat.
  wa_fieldcat-col_pos   = 1.
  wa_fieldcat-fieldname = 'INCLUDE'.
  wa_fieldcat-seltext_l = 'include'.
  wa_fieldcat-seltext_m = 'include'.
  wa_fieldcat-outputlen =  40.
  APPEND wa_fieldcat TO it_fieldcat.

endform.                    " F_INCLUDE_CATALOG
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_INCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_DISPLAY_INCLUDE .
         CLEAR it_layout.
  it_layout-zebra = c_x.
  it_layout-colwidth_optimize = c_x.
LOOP at it_includes2 INTO wa_includes2.
  wa_includes = wa_includes2.
  APPEND wa_includes TO it_includes.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = it_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = it_includes
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
*    MESSAGE e000 WITH 'Display Error'.
  ENDIF.
endform.                    " F_DISPLAY_INCLUDE
