************************************************************************
* Program Name  : ZRPUR032_A
* Descriptions  : 
* Create By     : 
* Create Date   : 
* Tcode         : 
************************************************************************
* Modification Log
************************************************************************
*   Date     Ver.   Programmer   Descriptions
* ---------- ----   ------------ --------------------------------------
*
************************************************************************
REPORT zrpur032_a.

INCLUDE zrpur032_atop.  "Data declaration
INCLUDE zrpur032_acl1.  "Class
INCLUDE zrpur032_as01.  "Selection screen
INCLUDE zrpur032_af01.  "Function
INCLUDE zrpur032_ao01.  "PBO
INCLUDE zrpur032_ai01.  "PAI


************************************************************************
*                      START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

  PERFORM init.

  CASE abap_true.
    WHEN r1.
      IF p_file IS INITIAL.
        MESSAGE s398(00) WITH TEXT-e05 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      PERFORM upload_excel USING 'H'.      "Get header sheet from upload excel
      PERFORM upload_excel USING 'I'.      "Get item sheet from upload excel
      PERFORM check_data.                  "Checking upload data
      PERFORM show_test_run_report.        "Show test run report
      PERFORM create_po USING 'X'.         "Create PO (Test Run)
    WHEN r2.
      PERFORM retrieve_data.               "Get data
      PERFORM show_upload_result_report.   "Show ALV report
    WHEN r3.
      PERFORM get_error_data.              "Get error data
      PERFORM create_po USING 'X'.         "Create PO (Test Run)
  ENDCASE.

END-OF-SELECTION.
