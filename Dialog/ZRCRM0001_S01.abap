*&---------------------------------------------------------------------*
*&  Include           ZRCRM0001_S01
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-s00.
SELECT-OPTIONS: s_erdat FOR  vbak-erdat OBLIGATORY.
SELECT-OPTIONS: s_vbeln FOR  vbak-vbeln.
PARAMETERS    : p_vkorg TYPE vbak-vkorg OBLIGATORY.
PARAMETERS    : p_suppl TYPE bukrs.
SELECT-OPTIONS: s_disti FOR  vbpa-kunnr.
PARAMETERS    : p_fkdat TYPE vbkd-fkdat .
SELECT-OPTIONS: s_fkdat FOR  vbkd-fkdat NO INTERVALS NO-EXTENSION NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK blk1.

***********************************************************************
*                     INITIALIZATION
***********************************************************************
INITIALIZATION.
*  PERFORM get_sales_org.

  CALL FUNCTION 'ZF_GET_APDATA_PATH'
    IMPORTING
      e_path = g_path.


************************************************************************
*                      AT SELECTION-SCREEN
***********************************************************************
AT SELECTION-SCREEN OUTPUT.


AT SELECTION-SCREEN.

*<< Authoirty check ?

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_suppl.

  PERFORM build_f4.
