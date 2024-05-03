*&---------------------------------------------------------------------*
*& INCLUDE          ZRQM003_AS01
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Screen build
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk0 WITH FRAME TITLE TEXT-s00.

  SELECTION-SCREEN: FUNCTION KEY 1.

  PARAMETERS: p_werks LIKE t001l-werks DEFAULT '9110',
              p_lgort LIKE t001l-lgort DEFAULT '1211'.

  SELECTION-SCREEN SKIP 1.

  PARAMETERS: r1 RADIOBUTTON GROUP gp1 USER-COMMAND flag  DEFAULT 'X'.
  PARAMETERS: r2 RADIOBUTTON GROUP gp1.
  PARAMETERS: p_file LIKE rlgrap-filename.

  SELECTION-SCREEN SKIP 1.

  PARAMETERS: r3 RADIOBUTTON GROUP gp1.
  SELECT-OPTIONS: s_date  FOR sy-datum,
                  s_time  FOR sy-timlo,
                  s_matnr FOR ztqm003_m6_a-matnr,
                  s_zzm6  FOR ztqm003_m6_a-zzm6_no.

SELECTION-SCREEN END OF BLOCK blk0.

*--------------------------------------------------------------------*
* AT SELECTION-SCREEN
*--------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM download_template USING 'ZRQM003_A'.
  ENDCASE.

  CASE abap_true.
    WHEN r1 OR r2.
      CHECK sy-ucomm NE 'FC01'.
      IF p_file IS INITIAL.
        SET CURSOR FIELD 'P_FILE'.
        MESSAGE e398(00) WITH TEXT-e05.
      ENDIF.
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM get_file CHANGING p_file.


INITIALIZATION.

  CLEAR functxt.
  functxt-icon_id = icon_xxl.
  functxt-quickinfo = TEXT-m01.
  functxt-icon_text = TEXT-m01.
  sscrfields-functxt_01 = functxt.

  SELECT SINGLE id INTO gv_led_gray   FROM icon WHERE name = gc_white.
  SELECT SINGLE id INTO gv_led_green  FROM icon WHERE name = gc_green.
  SELECT SINGLE id INTO gv_led_red    FROM icon WHERE name = gc_red.
  SELECT SINGLE id INTO gv_led_yellow FROM icon WHERE name = gc_yellow.
