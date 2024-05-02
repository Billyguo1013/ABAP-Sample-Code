*&---------------------------------------------------------------------*
*& INCLUDE          ZRPUR032_AS01
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Screen build
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk0 WITH FRAME TITLE TEXT-s00.

  SELECTION-SCREEN: FUNCTION KEY 1.

  PARAMETERS: r1 RADIOBUTTON GROUP gp1 USER-COMMAND flag  DEFAULT 'X'.
  SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME.
    PARAMETERS: p_file   LIKE rlgrap-filename                        MODIF ID 01.
  SELECTION-SCREEN END OF BLOCK blk1.

  PARAMETERS: r2 RADIOBUTTON GROUP gp1 .
  SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME.
    SELECT-OPTIONS: s_lifnr   FOR ztpur032_poh_a-lifnr               MODIF ID 02,
                    s_matnr   FOR ztpur032_poi_a-matnr               MODIF ID 02,
                    s_unsez   FOR ztpur032_poh_a-unsez               MODIF ID 02,
                    s_ebeln   FOR ztpur032_poh_a-ebeln               MODIF ID 02,
                    s_ekorg   FOR ztpur032_poh_a-ekorg               MODIF ID 02,
                    s_ekgrp   FOR ztpur032_poh_a-ekgrp               MODIF ID 02,
                    s_bedat   FOR ztpur032_poh_a-bedat               MODIF ID 02.
  SELECTION-SCREEN END OF BLOCK blk2.

  PARAMETERS: r3 RADIOBUTTON GROUP gp1.
  SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME.
    SELECT-OPTIONS: s_file    FOR ztpur032_poh_a-filename            MODIF ID 03.
  SELECTION-SCREEN END OF BLOCK blk3.


SELECTION-SCREEN END OF BLOCK blk0.

*--------------------------------------------------------------------*
* AT SELECTION-SCREEN
*--------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM download_template USING 'ZRPUR032_A'.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

  CASE abap_true.

    WHEN r1.
      LOOP AT SCREEN.
        IF ( screen-group1 = '02' OR screen-group1 = '03' ).
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN r2.
      LOOP AT SCREEN.
        IF ( screen-group1 = '01' OR screen-group1 = '03' ).
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN r3.
      LOOP AT SCREEN.
        IF ( screen-group1 = '01' OR screen-group1 = '02' ).
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
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
