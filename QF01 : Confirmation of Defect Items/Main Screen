************************************************************************
* Program Name  : ZRQM003_A
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
REPORT zrqm003_a.

INCLUDE zrqm003_atop.  "Data declaration
INCLUDE zrqm003_acl1.  "Class
INCLUDE zrqm003_as01.  "Selection screen
INCLUDE zrqm003_af01.  "Function
INCLUDE zrqm003_ao01.  "PBO
INCLUDE zrqm003_ai01.  "PAI

************************************************************************
*                      START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

  PERFORM init.

  CASE abap_true.

    WHEN r1.
      PERFORM upload_excel USING '1'.
      PERFORM check_data USING '1'.
      PERFORM create_goodsmvt.
      PERFORM show_upload_process_report.

    WHEN r2.
      PERFORM upload_excel USING '2'.
      PERFORM check_data USING '2'.
      PERFORM get_prueflos.  "取得檢驗批
      PERFORM qf01.
      PERFORM show_upload_process_report.

    WHEN r3.
      PERFORM retrieve_data.
      PERFORM show_alv.

  ENDCASE.

END-OF-SELECTION.
