*&---------------------------------------------------------------------*
*& Report YKCO_SAMPLES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT YKCO_SAMPLES_SALV_EXPAND.

INCLUDE ykco_samples_salv_expand_top.
INCLUDE ykco_samples_salv_expand_cl1.
INCLUDE ykco_samples_salv_expand_f01.
INCLUDE ykco_samples_salv_expand_o0100.
INCLUDE ykco_samples_salv_expand_i0100.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM data_get.

END-OF-SELECTION.
  PERFORM data_display.
