*&---------------------------------------------------------------------*
*& Report  Z_ENH_FLOWCHART
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_ENH_FLOWCHART.

"badi
TYPE-POOLS : slis.

*TABLES:  tadir .

TYPES: BEGIN OF t_tadir,
         pgmid    LIKE tadir-pgmid,       " Program ID in Requests
         object   LIKE tadir-object,      " Object Name in TADIR
         obj_name LIKE tadir-obj_name,    " Object Name in TADIR
         devclass TYPE tadir-devclass,    " Development Class
         text     TYPE sxs_attrt-text,    " Badi text
         imp_name TYPE sxc_exit-imp_name, " Implementation
         progname TYPE program_id,
       END OF t_tadir,
       BEGIN OF t_tstc,
         tcode    TYPE tstc-tcode,        " Transaction Code
         pgmna    TYPE tstc-pgmna,        " Program Name
         ttext    TYPE tstct-ttext,       " Transaction Text
         devclass TYPE tadir-devclass,    " Development Class
         area(100)     TYPE c,            " Area
       END OF t_tstc,
       BEGIN OF t_tcode,
         tcode    TYPE tadir-obj_name,        " Transaction Code
       END OF t_tcode,

       BEGIN OF ty_tadir,
       object	TYPE trobjtype,
       obj_name TYPE sobj_name,
       devclass	TYPE devclass,
       END OF ty_tadir,

       BEGIN OF ty_devc,
       devclass TYPE devclass,
       component TYPE uffctr,
       END OF ty_devc,

      BEGIN OF ty_df14l,
       fctr_id  TYPE uffctr,
       ps_posid	TYPE ufps_posid,
       END OF ty_df14l,

       BEGIN OF ty_sxs_attrt,
       exit_name TYPE	exit_def,
       text	TYPE cus_text,
       END OF ty_sxs_attrt,

       BEGIN OF ty_sxs_exit,
       imp_name TYPE  exit_imp,
       exit_name TYPE exit_def,
       flt_val  TYPE filtname ,
       END OF ty_sxs_exit,

       BEGIN OF ty_exit,
       name TYPE exit_def,
       END OF ty_exit,

       BEGIN OF ty_fltval,
        imp_name TYPE	exit_imp,
        exit_name	TYPE exit_def,
        flt_val TYPE filtname,
        END OF ty_fltval.

TYPES: BEGIN OF t_mod,
       name    TYPE progname,
*   modsapt-name,
       typ     LIKE modsap-typ,
       member  TYPE rs38l_fnam,
       modtext LIKE modsapt-modtext,
       END OF t_mod,

       BEGIN OF ty_tfdir,
       funcname	TYPE rs38l_fnam,
       pname  TYPE pname,
       include  TYPE includenr,
       END OF ty_tfdir,

       BEGIN OF ty_tftit,
       funcname	TYPE rs38l_fnam,
       stext TYPE	rs38l_ftxt,
       END OF ty_tftit.

TYPES: BEGIN OF t_userexits,
      name    TYPE t_mod-name,   "Exit name
      modtext TYPE t_mod-modtext,   "Exit text
      member  TYPE t_mod-member,   "Function module exit
      pname   TYPE tfdir-pname,   "Implementation name
      v_devclass TYPE tadir-devclass,   "Package
      area(100)     TYPE c,            " Area
      functext TYPE rs38l_ftxt,    "Function exit text
      tcode    TYPE tstc-tcode,        " Transaction Code
      include TYPE progname,
      END OF t_userexits,

       BEGIN OF t_final_badi,
         tcode    TYPE tstc-tcode,        " Transaction Code
         ttext    TYPE tstct-ttext,       " Transaction Text
         obj_name TYPE tadir-obj_name,    " Badi Name
         text     TYPE sxs_attrt-text,    " Badi text
         imp_name TYPE sxc_exit-imp_name, " Implementation
         devclass TYPE devclass,
         area(100)     TYPE c,            " Area
         prog TYPE progname,
         fltval TYPE filtname,
         usage  TYPE string,
         include TYPE progname,
       END OF t_final_badi.

TYPES: BEGIN OF t_inc,
name(1000) TYPE c,
END OF t_inc.

TYPES: BEGIN OF t_enhobj,
       enhname TYPE enhname,
       obj_type TYPE trobjtype,
       obj_name TYPE trobj_name,
       END OF t_enhobj,

        BEGIN OF t_enhspotobj,
        enhspot TYPE char120,
        obj_name TYPE trobj_name,
        END OF t_enhspotobj,

        BEGIN OF ty_enhincinx,
        enhname TYPE  enhname,
        version TYPE  r3state,
        programname TYPE  progname,
        id TYPE	int4,
        enhmode	TYPE enhmode,
        full_name TYPE string,
        enhinclude TYPE	progname,
        devclass TYPE devclass,
        END OF ty_enhincinx,

        BEGIN OF ty_final,
        program TYPE progname,
        desc    TYPE string,
        enhtype TYPE string,
        enhname TYPE enhname,
        impname TYPE enhname,
        objtyp  TYPE string,
        objname TYPE string,
        devclass TYPE devclass,
        area(100)  TYPE  c,
        tcode TYPE tcode,
        include TYPE progname,
        END OF ty_final,

        BEGIN OF ty_package,
        obj_name TYPE  trobj_name,
        devclass TYPE devclass,
        END OF ty_package.

TYPES : BEGIN OF t_final,
         tcode    TYPE tstc-tcode,
        include TYPE progname,
        desc TYPE char100,
        espot TYPE char20,
        pname TYPE char30,
        text TYPE char100,
        iname TYPE char100,
        package TYPE char30,
        area TYPE char100,
        fltval TYPE char80,
        usage TYPE char15,
        name    TYPE progname,
        END OF t_final.

* [ DATA Declarations ]
DATA: i_tadir       TYPE STANDARD TABLE OF t_tadir      INITIAL SIZE 0,
      i_tadir_ex    TYPE STANDARD TABLE OF ty_tadir,
      i_tstc        TYPE STANDARD TABLE OF t_tstc       INITIAL SIZE 0,
      i_final_badi  TYPE STANDARD TABLE OF t_final_badi INITIAL SIZE 0,
      i_sxs_attrt   TYPE STANDARD TABLE OF ty_sxs_attrt,
      i_sxc_exit    TYPE STANDARD TABLE OF ty_sxs_exit,
      i_tfdir       TYPE STANDARD TABLE OF ty_tfdir,
      i_devc        TYPE TABLE OF ty_devc,
      i_df14l       TYPE TABLE OF ty_df14l,
      i_tftit       TYPE TABLE OF ty_tftit,
      wa_tadir      TYPE t_tadir,
      wa_tstc       TYPE t_tstc,
      wa_final_badi TYPE t_final_badi,
      v_devclass      LIKE tadir-devclass,
       wa_fldcat     TYPE LINE OF slis_t_fieldcat_alv  , "D
       wa_layout     TYPE slis_layout_alv,
      i_fldcat    TYPE slis_t_fieldcat_alv , "D
      i_package TYPE TABLE OF ty_package,
      gt_fltval TYPE TABLE OF ty_fltval.
* [ CONSTANTS ]
CONSTANTS: c_r3tr(4)          TYPE c VALUE 'R3TR',
*           c_sxsd(4)          TYPE c VALUE 'SXSD',
           c_tran(4) TYPE c VALUE 'TRAN'.

** For ISU
DATA: snodetext TYPE TABLE OF snodetext,
      wa_snodetext TYPE snodetext.

DATA: it_mod  TYPE STANDARD TABLE OF t_mod INITIAL SIZE 0,
      wa_mod TYPE t_mod.

DATA: it_userexits TYPE STANDARD TABLE OF t_userexits,
      wa_userexits TYPE t_userexits.

DATA : it_final TYPE STANDARD TABLE OF t_final,
       wa_final TYPE t_final.

*Data declaration for enhancement spots
DATA:   i_enhspotobj TYPE STANDARD TABLE OF t_enhspotobj,
        w_enhspotobj TYPE t_enhspotobj,
        w_enhobj TYPE t_enhobj,
        i_enhobj TYPE STANDARD TABLE OF t_enhobj,
        i_enhobjtemp TYPE STANDARD TABLE OF t_enhobj,
        i_enhincinx TYPE STANDARD TABLE OF ty_enhincinx,
        w_enhincinx TYPE ty_enhincinx,
        i_final TYPE STANDARD TABLE OF ty_final,
        w_final TYPE ty_final,
        i_trdirt TYPE TABLE OF trdirt,
        w_trdirt TYPE trdirt.


selection-screen begin of block screen1.
parameters: p_pid  type string.
selection-screen end of block screen1.
PERFORM f_get_tstc.
PERFORM f_get_devclass.
PERFORM f_get_badi.
"user exit code
PERFORM f_get_details.
*Get enhancement spots
PERFORM f_enh_spot.
"combining

CLEAR wa_final.
LOOP AT i_final_badi INTO wa_final_badi.
  wa_final-name  =  wa_final_badi-prog.
  wa_final-tcode = wa_final_badi-tcode.
  wa_final-desc = wa_final_badi-ttext.
  wa_final-pname = wa_final_badi-obj_name.
  wa_final-text = wa_final_badi-text.
  wa_final-iname =  wa_final_badi-imp_name .
  wa_final-area = wa_final_badi-area.
  wa_final-package = wa_final_badi-devclass.
  wa_final-fltval = wa_final_badi-fltval.
  wa_final-usage = wa_final_badi-usage.
  wa_final-include = wa_final_badi-include.
  wa_final-espot = 'BADI'.
  APPEND wa_final TO it_final.
  CLEAR wa_final.
ENDLOOP.

REFRESH i_final_badi.
LOOP AT it_userexits INTO wa_userexits.

  wa_final-name = wa_userexits-pname.    "Include name
  wa_final-desc = wa_userexits-functext.
  wa_final-pname = wa_userexits-name .
  wa_final-text = wa_userexits-modtext.
  wa_final-iname = wa_userexits-member .
  wa_final-area = wa_userexits-area.
  wa_final-espot = 'EXIT'.
  wa_final-package = wa_userexits-v_devclass.
  wa_final-tcode = wa_userexits-tcode.
  wa_final-include = wa_userexits-include.
  APPEND wa_final TO it_final.
  CLEAR wa_final.
ENDLOOP.
REFRESH it_userexits.
CLEAR w_final.
LOOP AT i_final INTO w_final.
  wa_final-name = w_final-program.    "Program name
  wa_final-desc = w_final-desc.
  wa_final-pname = w_final-enhname .
  wa_final-text = w_final-objname.
  wa_final-iname = w_final-impname .
  wa_final-tcode = w_final-tcode.
  wa_final-area = w_final-area.
  wa_final-espot = w_final-enhtype.
  wa_final-package = w_final-devclass.
  wa_final-include = w_final-include.
  APPEND wa_final TO it_final.
  CLEAR wa_final.
ENDLOOP.
REFRESH i_final.
PERFORM gui_download.
PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  f_get_tstc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_tstc.
  SELECT tcode pgmna ttext
  FROM tstcv
  INTO TABLE i_tstc
    WHERE  sprsl = sy-langu.
  IF sy-subrc = 0.
    SORT i_tstc BY tcode.
    DELETE i_tstc WHERE tcode+0(1) = 'Z'
                      OR tcode+0(1) = 'Y'.
  ENDIF.

ENDFORM.                    "f_get_tstc
*&--------------------------------------------------------------------*
*&      Form  f_get_tcode_description
*&--------------------------------------------------------------------*
*       Get Description for Transaction Codes
*---------------------------------------------------------------------*
*FORM f_get_tcode_description.
*  CLEAR: wa_tstc.
*  LOOP AT i_tstc INTO wa_tstc.
*    SELECT SINGLE ttext FROM tstct INTO v_ttext
*    WHERE sprsl = sy-langu
*    AND   tcode = wa_tstc-tcode.
*    wa_tstc-ttext = v_ttext.
*    MODIFY i_tstc FROM wa_tstc.
*    CLEAR: wa_tstc.
*  ENDLOOP.
*ENDFORM.                    "f_get_tcode_description
*&--------------------------------------------------------------------*
*&      Form  f_get_devclass
*&--------------------------------------------------------------------*
*       Get the Development Class for Transaction Code programs
*---------------------------------------------------------------------*
FORM f_get_devclass.

  DATA: lt_tcode TYPE TABLE OF t_tcode,
        lwa_tcode TYPE t_tcode,
        lwa_tadir TYPE ty_tadir,
        lwa_df14l TYPE ty_df14l,
        lwa_devc TYPE ty_devc.

  LOOP AT i_tstc INTO wa_tstc.
    lwa_tcode-tcode = wa_tstc-tcode.
    APPEND lwa_tcode TO lt_tcode.
    CLEAR: lwa_tcode, wa_tstc.
  ENDLOOP.

  CLEAR: wa_tstc.
  IF lt_tcode IS NOT INITIAL.
    SELECT object obj_name devclass
       FROM tadir
      INTO TABLE i_tadir_ex
      FOR ALL ENTRIES IN lt_tcode
      WHERE pgmid    = c_r3tr
     AND   object   = c_tran
     AND   obj_name = lt_tcode-tcode.
  ENDIF.

  CLEAR: v_devclass.

***for ISU*****
  PERFORM find_fa USING v_devclass.

  LOOP AT i_tstc INTO wa_tstc.

    READ TABLE i_tadir_ex INTO lwa_tadir WITH KEY obj_name =
    wa_tstc-tcode.
    IF sy-subrc = 0.
      wa_tstc-devclass = lwa_tadir-devclass.
    ENDIF.
    READ TABLE i_devc INTO lwa_devc WITH KEY devclass =
    lwa_tadir-devclass.
    IF sy-subrc = 0.
      READ TABLE i_df14l INTO lwa_df14l WITH KEY fctr_id =
      lwa_devc-component.
      IF sy-subrc = 0.
        CLEAR wa_snodetext.
        READ TABLE  snodetext INTO wa_snodetext  WITH KEY name =
         lwa_df14l-ps_posid BINARY SEARCH.
        IF sy-subrc = 0.
          wa_tstc-area = wa_snodetext-name.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY i_tstc FROM wa_tstc TRANSPORTING devclass area.
    CLEAR: wa_tstc, lwa_tadir, wa_snodetext.
  ENDLOOP.

  REFRESH: i_tadir_ex, i_devc, snodetext.
ENDFORM.                    "f_get_devclass
*&--------------------------------------------------------------------*
*&      Form  f_get_badi
*&--------------------------------------------------------------------*
*       Get list of BADIs
*---------------------------------------------------------------------*
FORM f_get_badi.
  DATA: lt_fltval TYPE TABLE OF ty_fltval,
        lwa_tadir TYPE t_tadir,
        lwa_sxs_attrt TYPE ty_sxs_attrt,
        lwa_sxs_exit TYPE ty_sxs_exit,
        lv_objname TYPE sobj_name,
        lt_tadir TYPE TABLE OF t_tadir,
        lt_obj TYPE TABLE OF t_tcode,
        lwa_obj TYPE t_tcode,
        lt_ref TYPE akb_except_type,
        lwa_ref LIKE LINE OF lt_ref,
        lt_mainprog   TYPE TABLE OF d010inc,
        lwa_mainprog TYPE d010inc,
        lwa_trdirt TYPE trdirt,
        lwa_fltval TYPE ty_fltval.

  REFRESH: i_final_badi.

*get Implementation name
  SELECT imp_name exit_name flt_val
    FROM sxc_exit
    INTO TABLE i_sxc_exit
    WHERE ( imp_name LIKE 'Z%' OR imp_name LIKE 'Y%' ).
  IF  sy-subrc = 0.
    SORT i_sxc_exit BY exit_name.
  ENDIF.
  IF i_sxc_exit IS NOT INITIAL.
*   Get filter value for multiple use BADI
    SELECT imp_name
           exit_name
           flt_val
      FROM v_ext_m
      INTO TABLE gt_fltval
      FOR ALL ENTRIES IN i_sxc_exit
      WHERE imp_name = i_sxc_exit-imp_name
        AND exit_name = i_sxc_exit-exit_name.
    IF sy-subrc = 0.
      SORT gt_fltval BY imp_name exit_name.
    ENDIF.
*   Get filter value for single use BADI
    SELECT imp_name
           exit_name
           flt_val
      FROM v_ext_s
      INTO TABLE lt_fltval
      FOR ALL ENTRIES IN i_sxc_exit
      WHERE imp_name = i_sxc_exit-imp_name
        AND exit_name = i_sxc_exit-exit_name.
    IF sy-subrc = 0.
      SORT lt_fltval BY imp_name exit_name.
    ENDIF.

*Get Badi name
    SELECT exit_name text
      FROM sxs_attrt
      INTO TABLE i_sxs_attrt
      FOR ALL ENTRIES IN i_sxc_exit
      WHERE sprsl = sy-langu
        AND exit_name = i_sxc_exit-exit_name.
    IF sy-subrc = 0.
      SORT i_sxs_attrt BY exit_name.
    ENDIF.
  ENDIF.
  LOOP AT i_sxc_exit INTO lwa_sxs_exit.
*    lwa_obj-tcode = lwa_sxs_exit-imp_name.
    lwa_obj-tcode = lwa_sxs_exit-exit_name.
    APPEND lwa_obj TO lt_obj.
    CLEAR: lwa_obj, lwa_sxs_exit.
  ENDLOOP.
*  Get implementation package
  IF lt_obj IS NOT INITIAL.
    SELECT pgmid
           object
           obj_name
           devclass
    FROM   tadir
    INTO  TABLE lt_tadir
    FOR ALL ENTRIES IN lt_obj
    WHERE pgmid    = c_r3tr
    AND   object   = 'SXSD'
    AND   obj_name = lt_obj-tcode.
    IF sy-subrc = 0.
      SORT lt_tadir BY obj_name.
    ENDIF.
  ENDIF.

  CLEAR: wa_tadir.
  LOOP AT i_sxc_exit INTO lwa_sxs_exit.
    CLEAR lv_objname.
    lv_objname = lwa_sxs_exit-exit_name.

*    Get Where used list
    CALL FUNCTION 'AKB_WHERE_USED_LIST'
      EXPORTING
        obj_type   = 'SXSD'
        obj_name   = lv_objname
      IMPORTING
        references = lt_ref.
    IF lt_ref IS NOT INITIAL.
      DELETE lt_ref WHERE ( obj_type NE 'PROG'
                           AND obj_type NE 'FUNC' ).
    ENDIF.

*Get Main program
    LOOP AT lt_ref INTO lwa_ref.
      IF lwa_ref-obj_name+0(1) = 'Z'
        OR lwa_ref-obj_name+0(1) = 'Y'.
        CONTINUE.
      ENDIF.
      CALL FUNCTION 'RS_GET_MAINPROGRAMS'
        EXPORTING
          name         = lwa_ref-obj_name
        TABLES
          mainprograms = lt_mainprog
        EXCEPTIONS
          cancelled    = 1
          OTHERS       = 2.
      IF lt_mainprog IS NOT INITIAL.
        LOOP AT lt_mainprog INTO lwa_mainprog.
          IF lwa_mainprog-master+0(1) = 'Z' OR
            lwa_mainprog-master+0(1) = 'Y'.
            CONTINUE.
          ENDIF.
          wa_tadir-progname = lwa_mainprog-master.

          READ TABLE lt_tadir INTO lwa_tadir
          WITH KEY obj_name = lwa_sxs_exit-exit_name
                                                   BINARY SEARCH.
          IF sy-subrc = 0.
            wa_tadir-imp_name = lwa_sxs_exit-imp_name.
            READ TABLE i_sxs_attrt INTO lwa_sxs_attrt WITH KEY exit_name =
            lwa_sxs_exit-exit_name
                               BINARY   SEARCH.

            IF sy-subrc = 0.
              wa_tadir-text = lwa_sxs_attrt-text.
            ENDIF.

            wa_tadir-pgmid    = lwa_tadir-pgmid .
            wa_tadir-object   = lwa_tadir-object.
            wa_tadir-obj_name = lwa_sxs_exit-exit_name.
            wa_tadir-devclass = lwa_tadir-devclass.
            APPEND wa_tadir TO i_tadir.
          ENDIF.
          CLEAR: wa_tadir, lwa_tadir.
        ENDLOOP.
      ELSE.
        IF lwa_ref-obj_type = 'FUNC'.
          SELECT SINGLE pname INTO wa_tadir-progname
            FROM tfdir WHERE funcname = lwa_ref-obj_name.
        ENDIF.
        READ TABLE lt_tadir INTO lwa_tadir
        WITH KEY obj_name = lwa_sxs_exit-exit_name
                                                 BINARY SEARCH.
        IF sy-subrc = 0.
          wa_tadir-imp_name = lwa_sxs_exit-imp_name.
          READ TABLE i_sxs_attrt INTO lwa_sxs_attrt WITH KEY exit_name =
          lwa_sxs_exit-exit_name
                             BINARY   SEARCH.

          IF sy-subrc = 0.
            wa_tadir-text = lwa_sxs_attrt-text.
          ENDIF.

          wa_tadir-pgmid    = lwa_tadir-pgmid .
          wa_tadir-object   = lwa_tadir-object.
          wa_tadir-obj_name = lwa_sxs_exit-exit_name.
          wa_tadir-devclass = lwa_tadir-devclass.
          APPEND wa_tadir TO i_tadir.
        ENDIF.
      ENDIF.
      CLEAR: wa_tadir, lwa_tadir.
    ENDLOOP.
    CLEAR: lwa_sxs_exit, lwa_sxs_attrt.
  ENDLOOP.
  CLEAR: wa_tadir.

  DELETE i_tadir WHERE imp_name IS INITIAL.
  SORT i_tadir BY obj_name imp_name progname.
  DELETE ADJACENT DUPLICATES FROM i_tadir COMPARING obj_name
  imp_name progname.
  REFRESH lt_tadir.
  lt_tadir = i_tadir .
  SORT lt_tadir BY progname.
  DELETE ADJACENT DUPLICATES FROM lt_tadir COMPARING progname.
*Get program texts
  SELECT name sprsl text FROM trdirt
             INTO TABLE i_trdirt
             FOR ALL ENTRIES IN lt_tadir
             WHERE name = lt_tadir-progname AND
                   sprsl = sy-langu.
  IF sy-subrc = 0.
    SORT i_trdirt BY name.
  ENDIF.
  SORT i_tstc BY devclass.
  SORT i_tadir BY obj_name imp_name progname.
  LOOP AT i_tadir INTO wa_tadir.
    LOOP AT i_tadir INTO lwa_tadir
    WHERE obj_name = wa_tadir-obj_name
         AND    imp_name = wa_tadir-imp_name
         AND    progname NE space.
      DELETE i_tadir WHERE obj_name = wa_tadir-obj_name
             AND    imp_name = wa_tadir-imp_name
             AND    progname EQ space.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
      CLEAR: lwa_tadir.
    ENDLOOP.
  ENDLOOP.
*  sort i_tstc by pgmna.
  LOOP AT i_tadir INTO wa_tadir.
    CLEAR wa_tstc.
    IF wa_tadir-progname IS NOT INITIAL.
      READ TABLE i_tstc INTO wa_tstc
            WITH KEY pgmna = wa_tadir-progname.
*          BINARY SEARCH.
    ENDIF.
    READ TABLE i_trdirt INTO lwa_trdirt
    WITH KEY name = wa_tadir-progname BINARY SEARCH.

    wa_final_badi-prog       = wa_tadir-progname.
    wa_final_badi-include       = wa_tadir-progname.
    wa_final_badi-tcode     = wa_tstc-tcode.
    wa_final_badi-ttext     = lwa_trdirt-text.
    wa_final_badi-obj_name  = wa_tadir-obj_name.
    wa_final_badi-text      = wa_tadir-text.
    wa_final_badi-imp_name  = wa_tadir-imp_name.
    wa_final_badi-devclass  = wa_tadir-devclass.
    READ TABLE gt_fltval INTO lwa_fltval WITH KEY imp_name = wa_tadir-imp_name
                                                  exit_name = wa_tadir-obj_name
                                                  BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final_badi-fltval = lwa_fltval-flt_val.
      wa_final_badi-usage  = 'Multiple Use'.
      CLEAR lwa_fltval.
    ELSE.
      READ TABLE lt_fltval INTO lwa_fltval WITH KEY imp_name = wa_tadir-imp_name
                                                    exit_name = wa_tadir-obj_name
                                                       BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final_badi-fltval = lwa_fltval-flt_val.
        wa_final_badi-usage = 'Single Use'.
        CLEAR lwa_fltval.
      ENDIF.
    ENDIF.


** insert records where AREA is not initial.
    CLEAR wa_tstc.
    READ TABLE  i_tstc INTO wa_tstc
              WITH KEY devclass = wa_tadir-devclass
             BINARY SEARCH.
    IF wa_final_badi-tcode IS INITIAL.
      wa_final_badi-tcode = wa_tstc-tcode.
    ENDIF.
    IF wa_tstc-area IS NOT INITIAL.
      wa_final_badi-area      = wa_tstc-area.
    ENDIF.
    APPEND wa_final_badi TO i_final_badi.
    CLEAR: wa_tadir, wa_final_badi, wa_tstc.
  ENDLOOP.
  DELETE i_final_badi WHERE imp_name = ''.
  SORT i_final_badi BY obj_name imp_name prog.
  DELETE ADJACENT DUPLICATES FROM i_final_badi COMPARING obj_name imp_name prog.

  REFRESH: i_sxc_exit, i_sxs_attrt, gt_fltval, lt_fltval.
ENDFORM.                    "f_get_badi
*&---------------------------------------------------------------------*
*&      Form  FIND_FA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM find_fa  USING  v_devclass TYPE devclass.
*  CLEAR v_area.
*  CLEAR: tdevc, df14l.
  IF snodetext IS INITIAL.
    CALL FUNCTION 'RS_COMPONENT_VIEW'
      EXPORTING
        object_type = 'DEVC'
      TABLES
        nodetab     = snodetext.
  ENDIF.
  SORT snodetext BY name.

*    Get package
  IF i_tadir_ex IS NOT INITIAL.
    SELECT devclass component
      FROM tdevc
      INTO TABLE i_devc
      FOR ALL ENTRIES IN i_tadir_ex
      WHERE devclass = i_tadir_ex-devclass.
    IF sy-subrc = 0.
*     Get Functional area
      SELECT fctr_id ps_posid
        FROM df14l
        INTO TABLE i_df14l
        FOR ALL ENTRIES IN i_devc
        WHERE fctr_id = i_devc-component.
      IF sy-subrc = 0.
        SORT i_df14l BY fctr_id.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " FIND_FA


*&---------------------------------------------------------------------*
*&      Form  f_get_details
*&---------------------------------------------------------------------*
FORM f_get_details.

  TYPES: BEGIN OF ty_include,
         func TYPE modmember,
         incl TYPE sobj_name,
         END OF ty_include,

         BEGIN OF ty_tadir,
         obj_name TYPE sobj_name,
         devclass TYPE devclass,
         END OF ty_tadir
         .

  DATA: l_len TYPE i,
        lwa_tfdir TYPE ty_tfdir,
        lv_pname TYPE pname,
        lwa_tftit TYPE ty_tftit,
        lt_include TYPE TABLE OF ty_include,
        lwa_include TYPE ty_include,
        lt_tadir TYPE TABLE OF ty_tadir,
        lwa_tadir TYPE ty_tadir,
        lv_objname TYPE sobj_name,
        lt_ref TYPE akb_except_type,
        lwa_ref LIKE LINE OF lt_ref,
        lt_mainprog   TYPE TABLE OF d010inc,
        lwa_mainprog TYPE d010inc.

  CLEAR:it_mod.
  REFRESH:it_mod, lt_include.
  SELECT name typ member modtext
    FROM modsapview
    INTO TABLE it_mod
    WHERE sprsl = sy-langu
      AND typ = 'E'.
  IF sy-subrc = 0.
    SORT it_mod BY name member.
  ENDIF.
  IF  it_mod IS NOT INITIAL.
    SELECT funcname pname include
      FROM tfdir
      INTO TABLE i_tfdir
      FOR ALL ENTRIES IN it_mod
      WHERE funcname = it_mod-member.
    IF sy-subrc = 0.
      SORT i_tfdir BY funcname.
    ENDIF.
    IF i_tfdir IS NOT INITIAL.
      SELECT funcname stext
        FROM tftit
        INTO TABLE i_tftit
        FOR ALL ENTRIES IN i_tfdir
        WHERE spras = sy-langu
          AND funcname = i_tfdir-funcname.
    ENDIF.
  ENDIF.

  LOOP AT it_mod INTO wa_mod.

    READ TABLE i_tfdir INTO lwa_tfdir WITH KEY funcname = wa_mod-member
                                                     BINARY SEARCH.
    IF sy-subrc = 0.
      l_len = STRLEN( lwa_tfdir-pname ).
      l_len = l_len - 4.
      CLEAR: lv_pname.
      IF l_len GE 0.
        CONCATENATE 'Z' lwa_tfdir-pname+4(l_len) 'U' lwa_tfdir-include
        INTO lv_pname.
      ENDIF.
      lwa_include-func = wa_mod-member.
      lwa_include-incl = lv_pname.
      APPEND lwa_include TO lt_include.
      CLEAR: lv_pname, lwa_include, wa_mod.
    ENDIF.
  ENDLOOP.
  SORT lt_include BY func.
  IF lt_include IS NOT INITIAL.
    SELECT obj_name devclass
      from tadir INTO TABLE lt_tadir
      FOR ALL ENTRIES IN lt_include
      where pgmid = 'R3TR'
        AND object = 'PROG'
        AND obj_name = lt_include-incl.
      IF sy-subrc = 0.
        SORT lt_tadir by obj_name.
      ENDIF.
     LOOP AT it_mod INTO wa_mod.
       READ TABLE lt_include INTO lwa_include WITH key func = wa_mod-member
          BINARY SEARCH.
       IF sy-subrc = 0.
        READ TABLE lt_tadir INTO lwa_tadir with KEY obj_name = lwa_include-incl
                                   BINARY SEARCH.
        IF sy-subrc <> 0.
         delete TABLE it_mod from wa_mod.
        ENDIF.
       ENDIF.
       CLEAR: wa_mod, lwa_include.
     ENDLOOP.
  ENDIF.

  refresh lt_tadir.

*    Get package
  SELECT obj_name devclass
    FROM tadir
    INTO TABLE lt_tadir
    FOR ALL ENTRIES IN it_mod
    WHERE  pgmid = 'R3TR'
*        AND object = 'PROG'
      AND object = 'SMOD'
      AND obj_name = it_mod-name.
  IF sy-subrc = 0.
    SORT lt_tadir BY obj_name.
  ENDIF.
*  ENDIF.

  REFRESH lt_ref.
  LOOP AT it_mod INTO wa_mod.
    lv_objname = wa_mod-member.
*    *    Get Where used list
    CALL FUNCTION 'AKB_WHERE_USED_LIST'
      EXPORTING
        obj_type   = 'FUNC'
        obj_name   = lv_objname
      IMPORTING
        references = lt_ref.
    IF lt_ref IS NOT INITIAL.
      DELETE lt_ref WHERE ( obj_type NE 'PROG'
                           AND obj_type NE 'FUNC' ).
    ENDIF.

*Get Main program
    LOOP AT lt_ref INTO lwa_ref.
      IF lwa_ref-obj_name+0(1) = 'Z'
        OR lwa_ref-obj_name+0(1) = 'Y'.
        CONTINUE.
      ENDIF.
      CALL FUNCTION 'RS_GET_MAINPROGRAMS'
        EXPORTING
          name         = lwa_ref-obj_name
        TABLES
          mainprograms = lt_mainprog
        EXCEPTIONS
          cancelled    = 1
          OTHERS       = 2.

      IF lt_mainprog IS NOT INITIAL.
        LOOP AT lt_mainprog INTO lwa_mainprog.
          IF lwa_mainprog-master+0(1) = 'Z' OR
               lwa_mainprog-master+0(1) = 'Y'.
            CONTINUE.
          ENDIF.
          wa_userexits-pname      = lwa_mainprog-master.
          wa_userexits-include    = lwa_mainprog-master.
          READ TABLE i_tftit INTO lwa_tftit WITH KEY funcname =
*      lwa_tfdir-funcname
             wa_mod-member
                                                              BINARY SEARCH.
          IF sy-subrc = 0.
            wa_userexits-functext = lwa_tftit-stext.
            CLEAR lwa_tftit.
          ENDIF.

*      SELECT SINGLE *  FROM tadir
*                  INTO  tadir
*                  WHERE obj_name = lv_pname.
          READ TABLE lt_include INTO lwa_include WITH KEY func = wa_mod-member
                                          BINARY SEARCH.
          IF sy-subrc = 0.
            wa_userexits-include = lwa_include-incl.
          ENDIF.
          IF wa_userexits-pname IS NOT INITIAL.
            READ TABLE i_tstc INTO wa_tstc
                  WITH KEY pgmna = wa_userexits-pname.
            IF sy-subrc = 0.
              wa_userexits-tcode = wa_tstc-tcode.
            ENDIF.
          ENDIF.
*          IF sy-subrc = 0.
          READ TABLE lt_tadir INTO lwa_tadir
          WITH KEY obj_name = wa_mod-name
          BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE i_tstc INTO wa_tstc WITH KEY devclass =
            lwa_tadir-devclass.
            IF sy-subrc = 0.
              wa_userexits-area = wa_tstc-area.
            ENDIF.
            IF wa_userexits-tcode IS INITIAL.
              wa_userexits-tcode = wa_tstc-tcode.
            ENDIF.
            wa_userexits-name       = wa_mod-name.
            wa_userexits-modtext    = wa_mod-modtext.
            wa_userexits-member     = wa_mod-member.
            wa_userexits-v_devclass = lwa_tadir-devclass.

            APPEND wa_userexits TO it_userexits.
          ENDIF.
*          ENDIF.

          CLEAR wa_userexits.
          CLEAR: v_devclass, lwa_tadir.

        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  SORT it_userexits BY name member pname.
  DELETE ADJACENT DUPLICATES FROM it_userexits COMPARING name member pname.
  REFRESH it_mod.
ENDFORM.                    "f_get_details

*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv .
  CLEAR wa_fldcat.
  wa_fldcat-col_pos   = 1.
  wa_fldcat-fieldname = 'TCODE'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'TCODE'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.
  wa_fldcat-col_pos   = 2.
  wa_fldcat-fieldname = 'NAME'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'PROGRAM NAME'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.
  wa_fldcat-col_pos   = 3.
  wa_fldcat-fieldname = 'DESC'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'PROGRAM DESCRIPTION/TCODE DESCRIPTION'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.
  wa_fldcat-col_pos   = 4.
  wa_fldcat-fieldname = 'ESPOT'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'ENHANCEMENT TYPE'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.
  wa_fldcat-col_pos   = 5.
  wa_fldcat-fieldname = 'PNAME'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'Enhancement Name'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.
  wa_fldcat-col_pos   = 6.
  wa_fldcat-fieldname = 'TEXT'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'OBJECT TEXT'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.
  wa_fldcat-col_pos   = 7.
  wa_fldcat-fieldname = 'INAME'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'IMPLEMENTATION NAME'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.
  wa_fldcat-col_pos   = 8.
  wa_fldcat-fieldname = 'PACKAGE'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'PACKAGE'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.

  wa_fldcat-col_pos   = 9.
  wa_fldcat-fieldname = 'AREA'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'AREA'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.

  wa_fldcat-col_pos   = 10.
  wa_fldcat-fieldname = 'FLTVAL'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'Filter'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.

  wa_fldcat-col_pos   = 11.
  wa_fldcat-fieldname = 'USAGE'.
  wa_fldcat-tabname   = 'IT_FINAL'.
  wa_fldcat-seltext_l = 'Multiple/Single Use'.
  APPEND wa_fldcat TO i_fldcat.
  CLEAR wa_fldcat.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      is_layout     = wa_layout
      it_fieldcat   = i_fldcat
    TABLES
      t_outtab      = it_final
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  REFRESH: it_final, i_fldcat.
ENDFORM.                    " DISPLAY_ALV             "f_get_details
*&---------------------------------------------------------------------*
*&      Form  F_ENH_SPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_enh_spot .

  TYPES: BEGIN OF ty_obj,
         obj     TYPE tadir-obj_name,
         objtmp  TYPE tadir-obj_name,
         END OF ty_obj.

  DATA: lt_enhincinx TYPE TABLE OF ty_enhincinx,
        lwa_package TYPE ty_package,
        lv_object TYPE trobj_name,
        lt_devclass TYPE TABLE OF ty_package,
        lwa_devclass TYPE ty_package,
        lt_object TYPE TABLE OF ty_obj,
        lwa_object TYPE ty_obj.
*Get enhancement spots (explicit enhancements) from table ENHSPOTOBJ
  SELECT enhspot obj_name
    FROM enhspotobj
    INTO TABLE i_enhspotobj
        WHERE version = 'A'.

  IF sy-subrc = 0.
    SORT i_enhspotobj BY enhspot obj_name.
    DELETE ADJACENT DUPLICATES FROM i_enhspotobj COMPARING ALL FIELDS.

*Get enhancement objects from table ENHOBJ based on enhancement spot
    SELECT enhname obj_type obj_name
      FROM enhobj
      INTO TABLE i_enhobjtemp
      FOR ALL ENTRIES IN i_enhspotobj
       WHERE   ( enhname LIKE 'Z%' OR enhname LIKE 'Y%' )
           AND  version = 'A' AND
               ( obj_name = i_enhspotobj-enhspot
                OR obj_name = i_enhspotobj-obj_name ).

    IF sy-subrc = 0 AND i_enhobjtemp IS NOT INITIAL.
*Get Enhancement objects (both explicit and implicit) from ENHOBJ
      SELECT enhname obj_type obj_name
         FROM enhobj
         INTO TABLE i_enhobj
         FOR ALL ENTRIES IN i_enhobjtemp
          WHERE enhname = i_enhobjtemp-enhname
            AND version = 'A'.
      IF sy-subrc = 0 AND i_enhobj IS NOT INITIAL.
        SORT i_enhobj BY enhname obj_type.
        REFRESH i_enhobjtemp.
        i_enhobjtemp[] = i_enhobj[].
        SORT i_enhobjtemp BY enhname.
        DELETE ADJACENT DUPLICATES FROM i_enhobjtemp COMPARING enhname.
        IF i_enhobjtemp IS NOT INITIAL.
          SELECT enhname version
                 programname id
                 enhmode full_name
                 enhinclude
            FROM enhincinx
            INTO TABLE i_enhincinx
             FOR ALL ENTRIES IN i_enhobjtemp
              WHERE enhname = i_enhobjtemp-enhname
                 AND version = 'A'.
        ENDIF.
*         Get enhancement program
        IF sy-subrc = 0.
          SORT i_enhincinx BY programname.
          DELETE i_enhincinx WHERE programname+0(1) = 'Z'
                                OR programname+0(1) = 'Y'.
          lt_enhincinx = i_enhincinx.
          DELETE ADJACENT DUPLICATES FROM lt_enhincinx COMPARING
          programname.
        ENDIF.
        IF lt_enhincinx IS NOT INITIAL.
          REFRESH i_trdirt.
*       Select programs title.
          SELECT name sprsl text FROM trdirt
            INTO TABLE i_trdirt
            FOR ALL ENTRIES IN lt_enhincinx
            WHERE name = lt_enhincinx-programname AND
                  sprsl = sy-langu.
          IF sy-subrc = 0.
            SORT i_trdirt BY name.
          ENDIF.
          IF lt_enhincinx IS NOT INITIAL.
            SELECT obj_name devclass
              INTO TABLE i_package
              FROM tadir
              FOR ALL ENTRIES IN lt_enhincinx
              WHERE pgmid = c_r3tr
                AND object = 'PROG'
                AND obj_name = lt_enhincinx-programname.
            IF sy-subrc = 0.
              SORT i_package BY obj_name.
              DELETE ADJACENT DUPLICATES FROM i_package COMPARING
              obj_name.
            ENDIF.
          ENDIF.
        ENDIF.
        LOOP AT lt_enhincinx INTO w_enhincinx.
          READ TABLE i_package INTO lwa_package WITH KEY obj_name =
          w_enhincinx-programname
                              BINARY SEARCH TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            CLEAR lv_object.
            IF w_enhincinx-programname+0(3) = '%_C'.
              lwa_object-obj = w_enhincinx-programname.
              lwa_object-objtmp = w_enhincinx-programname+3.
              APPEND lwa_object TO lt_object.
              CLEAR lwa_object.
            ELSEIF w_enhincinx-programname CS '='.
              lwa_object-obj = w_enhincinx-programname.
              SPLIT w_enhincinx-programname AT '=' INTO
              lwa_object-objtmp lv_object.
              CLEAR lv_object.
              APPEND lwa_object TO lt_object.
              CLEAR lwa_object.
            ELSEIF w_enhincinx-programname+0(4) = 'SAPL'.
              lwa_object-obj = w_enhincinx-programname.
              lwa_object-objtmp = w_enhincinx-programname+4.
              APPEND lwa_object TO lt_object.
              CLEAR lwa_object.
            ELSE.
              lwa_object-obj = w_enhincinx-programname.
              lwa_object-objtmp = w_enhincinx-programname.
              APPEND lwa_object TO lt_object.
              CLEAR lwa_object.
            ENDIF.

          ENDIF.
        ENDLOOP.
        SORT lt_object BY obj.
        DELETE ADJACENT DUPLICATES FROM lt_object COMPARING obj.

        IF lt_object IS NOT INITIAL.
          SELECT obj_name devclass
            FROM tadir
            INTO TABLE lt_devclass
            FOR ALL ENTRIES IN lt_object
            WHERE pgmid = c_r3tr
              AND obj_name = lt_object-objtmp.
          IF sy-subrc = 0.
            SORT lt_devclass BY obj_name.
            DELETE ADJACENT DUPLICATES FROM lt_devclass COMPARING
            obj_name devclass.
          ENDIF.
        ENDIF.

        LOOP AT i_enhincinx INTO w_enhincinx.
          w_final-impname = w_enhincinx-enhname.
          w_final-program = w_enhincinx-programname.
          w_final-include = w_enhincinx-programname.

          READ TABLE i_tstc INTO wa_tstc
                WITH KEY pgmna = w_enhincinx-programname.
          IF sy-subrc = 0.
            w_final-tcode = wa_tstc-tcode.
          ENDIF.

          READ TABLE i_trdirt INTO w_trdirt WITH KEY name =
          w_enhincinx-programname
                                                            BINARY
                                                            SEARCH.
          IF sy-subrc = 0.
            w_final-desc = w_trdirt-text.
          ENDIF.

          READ TABLE i_package INTO lwa_package WITH KEY obj_name =
          w_enhincinx-programname
                                                          BINARY SEARCH
                                                            .
          IF sy-subrc = 0.
            w_final-devclass = lwa_package-devclass.
          ELSE.
            READ TABLE lt_object INTO lwa_object WITH KEY obj =
            w_enhincinx-programname
                                                            BINARY
                                                            SEARCH.
            IF sy-subrc = 0.
              READ TABLE lt_devclass INTO lwa_devclass WITH KEY obj_name =
              lwa_object-objtmp
                                                              BINARY
                                                              SEARCH.
              IF sy-subrc = 0.
                w_final-devclass = lwa_devclass-devclass.
              ENDIF.
            ENDIF.
          ENDIF.
          READ TABLE i_tstc INTO wa_tstc
                        WITH KEY devclass = w_final-devclass.
          IF sy-subrc = 0.
            w_final-area = wa_tstc-area.
          ENDIF.
          if w_final-tcode IS INITIAL.
            w_final-tcode = wa_tstc-tcode.
          endif.
          READ TABLE i_enhobj INTO w_enhobj WITH KEY enhname =
          w_enhincinx-enhname
                                                     obj_type = 'ENHS'.
          IF sy-subrc = 0.
*            fill enhanement spot name
            w_final-enhname = w_enhobj-obj_name.
          ENDIF.

          READ TABLE i_enhspotobj INTO w_enhspotobj WITH KEY enhspot =
          w_enhobj-obj_name
          TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            w_final-enhtype = 'EXPLICIT'.
          ELSE.
            w_final-enhtype = 'IMPLICIT'.
          ENDIF.

*        CALL METHOD cl_get_include->get_line_position
*          EXPORTING
*            iv_full_name = w_enhincinx-full_name
*            iv_prog      = w_enhincinx-programname
*            iv_enhname   = w_enhincinx-enhname
*            iv_enhtype   = 'ENHO'
*          IMPORTING
*            ev_line_nr   = v_line_nr
*            ev_include   = v_include
*            ev_line      = v_line.
*        IF sy-subrc = 0 AND v_include is NOT INITIAL.
*          w_final-objtyp = 'INCLUDE'.
*          w_final-objname = v_include.
*          else.
*        split w_enhincinx-full_name at '\' INTO TABLE itab.
          REPLACE ALL OCCURRENCES OF: 'PR:' IN w_enhincinx-full_name
          WITH 'Program:'  ,
                   'FU:' IN  w_enhincinx-full_name WITH
                   'Function Module:' ,
                   'IC:' IN  w_enhincinx-full_name WITH 'Include:' ,
                   'TY:' IN  w_enhincinx-full_name WITH 'Class:' ,
                   'ME:' IN  w_enhincinx-full_name WITH 'Method:' ,
                   'SE:' IN  w_enhincinx-full_name WITH 'Section:',
                   'IN:' IN  w_enhincinx-full_name WITH 'Interface:' ,
                   'EX:' IN  w_enhincinx-full_name WITH
                   'Ehancement-Point:' ,
                   '\EI' IN  w_enhincinx-full_name WITH space .
          w_final-objname = w_enhincinx-full_name.
*        ENDIF.
          APPEND w_final TO i_final.
          CLEAR: w_final, w_enhincinx,
          w_enhspotobj, w_enhobj,
          lwa_package, lwa_devclass, lwa_object.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  SORT i_final BY enhname impname program.
  DELETE ADJACENT DUPLICATES FROM i_final COMPARING enhname impname program.
  REFRESH: i_enhspotobj, i_enhobjtemp, i_enhobj, i_package, i_enhincinx.
ENDFORM.                    " F_ENH_SPOT
*&---------------------------------------------------------------------*
*&      Form  GUI_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gui_download .
  DATA: lv_file TYPE string.

  CONCATENATE 'C:\Enhancement\impl' '_' sy-datum '_' sy-uzeit '.xls' INTO lv_file.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_file
      filetype                = 'DAT'
    TABLES
      data_tab                = it_final
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " GUI_DOWNLOAD
