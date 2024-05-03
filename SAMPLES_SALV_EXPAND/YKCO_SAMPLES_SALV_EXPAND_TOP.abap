*&---------------------------------------------------------------------*
*& Include          YKCO_SAMPLES_SALV_EXPAND_TOP
*&---------------------------------------------------------------------*


CONSTANTS:
  BEGIN OF gcs_tadir_objects,
    doma TYPE trobjtype VALUE 'DOMA',
    dtel TYPE trobjtype VALUE 'DTEL',
    prog TYPE trobjtype VALUE 'PROG',
    ssfo TYPE trobjtype VALUE 'SHLP',
    form TYPE trobjtype VALUE 'TABL',
  END OF gcs_tadir_objects.

DATA:
  gt_tadir        TYPE ykco_samples_salv_tadir_t,           "#EC NEEDED
  gt_tadir_output TYPE ykco_samples_salv_tadir_outp_t.      "#EC NEEDED

DATA:
  go_salv_table TYPE REF TO cl_salv_table.                  "#EC NEEDED
