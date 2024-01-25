FUNCTION y_scr_01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_TCODES STRUCTURE  YTCODES
*"      O_SCREENS STRUCTURE  YSCREENS
*"----------------------------------------------------------------------
  TABLES: d020t.

* TSTC - with Program names
  DATA : BEGIN OF i_tstc1 OCCURS 0,
          tcode LIKE tstc-tcode,
          pgmna LIKE tstc-pgmna,
          dypno LIKE tstc-dypno,
         END   OF i_tstc1.
* screen descriptions
  DATA : BEGIN OF i_d020t OCCURS 0,
           prog LIKE d020t-prog,
           dynr LIKE d020t-dynr,
           lang LIKE d020t-lang,
           dtxt LIKE d020t-dtxt,
         END OF i_d020t.

* screen fields
  DATA  BEGIN OF i_dynp_fields OCCURS 20.
          INCLUDE STRUCTURE rsdcf.
  DATA  END OF i_dynp_fields.
* screen lines
  DATA  BEGIN OF i_lines OCCURS 20.
          INCLUDE STRUCTURE tline.
  DATA  END OF i_lines.

************************************************************************

* validate all the transaction codes
  SORT i_tcodes BY tcode.
* get the prognames & tcodes & screen number
* determine if the tcodes has associcated programs
  IF NOT i_tcodes[] IS INITIAL.
    SELECT tcode
           pgmna
           dypno
           FROM tstc
           INTO TABLE i_tstc1
           FOR ALL ENTRIES IN i_tcodes
           WHERE tcode EQ i_tcodes-tcode.
  ENDIF.

* validate the screen number with table d020t
* this is to confirm whether the screen number is present or not as SAP
* can remove screens
  SORT i_tstc1 BY tcode.
  IF NOT i_tstc1[] IS INITIAL.
    SELECT prog
           dynr
           lang
           dtxt
           FROM d020t
           INTO TABLE i_d020t
           FOR ALL ENTRIES IN i_tstc1
           WHERE prog EQ i_tstc1-pgmna
             AND lang = 'D'.
  ENDIF.
* get the screen fields info

  DATA : cnt_tcode TYPE i,
         l_text(70),
         l_tabix(6) TYPE c.
* process for all programs
  LOOP AT i_d020t.
    REFRESH i_dynp_fields.

*   show the gui status bar to avoid short dumps
    ADD 1 TO cnt_tcode.
    IF cnt_tcode GT 100.
      CLEAR : cnt_tcode.
      MOVE sy-tabix TO l_tabix.
      CONCATENATE 'Processing done for'
                    l_tabix
                    'Objects'
                    INTO l_text
                    SEPARATED BY space.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text   = l_text
        EXCEPTIONS
          OTHERS = 1.
      IF sy-batch = 'X'.
        MESSAGE i000(su) WITH l_text.
*      ELSE.
*        GET TIME FIELD v_time2.
*        v_time_diff = v_time2 - v_time1.
*        IF v_time_diff > 45 OR v_time_diff < 0.
*          COMMIT WORK.
*          GET TIME FIELD v_time1.
*        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR o_screens.
*   header record
    CLEAR i_tstc1.
    READ TABLE i_tstc1 WITH KEY pgmna = i_d020t-prog.
    IF sy-subrc EQ 0.
*     tcode
      o_screens-tcode = i_tstc1-tcode.
*     program name
      o_screens-programnm = i_d020t-prog.
      APPEND o_screens.
    ENDIF.
*   screen number
    o_screens-screen = i_d020t-dynr.
    APPEND o_screens.
*   pass the program name and screen number
    CALL FUNCTION 'DYNPRO_FIELD_GET'
      EXPORTING
        dynpro           = i_d020t-dynr
        program          = i_d020t-prog
      TABLES
        dynp_fields      = i_dynp_fields
        lines            = i_lines
      EXCEPTIONS
        dynpro_not_found = 1
        OTHERS           = 2.
    IF sy-subrc EQ 0.
      CLEAR o_screens.
      LOOP AT i_dynp_fields.
        IF i_dynp_fields-tabname <> ''.
*         create field records in the fields table
          CLEAR o_screens.
*         field name
          o_screens-tcode = i_tstc1-tcode.
          o_screens-screen = i_d020t-dynr.
          o_screens-programnm = i_d020t-prog.
          o_screens-fieldnm = i_dynp_fields-dynpro_fld.
*          if not i_dynp_fields-stxt1 CA '_'.
*           field text
*            o_screens-stext = i_dynp_fields-stxt1.
*          else.
*            o_screens-stext = i_dynp_fields-stxt2.
*          endif.
          APPEND o_screens.
        ENDIF.
      ENDLOOP.
*     get the bdc_okcode
      CLEAR o_screens.
      o_screens-tcode = i_tstc1-tcode.
      o_screens-screen = i_d020t-dynr.
      o_screens-programnm = i_d020t-prog.
      o_screens-fieldnm = 'BDC_OKCODE'.
      APPEND o_screens.
    ENDIF.
  ENDLOOP.




ENDFUNCTION.
