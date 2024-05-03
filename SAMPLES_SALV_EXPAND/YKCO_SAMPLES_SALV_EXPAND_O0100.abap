*&---------------------------------------------------------------------*
*& Include          YKCO_SAMPLES_SALV_EXPAND_O0100
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
ENDMODULE.

MODULE alv OUTPUT.
  PERFORM alv_display.
ENDMODULE.
