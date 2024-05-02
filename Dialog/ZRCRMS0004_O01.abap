*----------------------------------------------------------------------*
***INCLUDE ZRCRMS0004_O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.

  DATA: lt_ucomm  TYPE TABLE OF sy-ucomm,
        ls_ucomm  LIKE LINE OF  lt_ucomm.

  REFRESH lt_ucomm.
  ls_ucomm = '&SUM'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = '&UMC'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = 'ZDETAIL'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = 'ZGROUPING'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = 'ZSAVE'.
  APPEND ls_ucomm TO lt_ucomm.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = 'ZCANCEL'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = 'ZNOTE'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = '&ETA'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = '&OUP'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = '&ODN'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = '&ALL'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = '&SAL'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = '&ILT'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = '&OL0'.
  APPEND ls_ucomm TO lt_ucomm.

  SET TITLEBAR '9001'.
  SET PF-STATUS 'STANDARD' EXCLUDING lt_ucomm.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.

  PERFORM display_alv.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
