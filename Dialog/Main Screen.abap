************************************************************************
* Program Name  : ZRCRM0001
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

REPORT  zrcrm0001  NO STANDARD PAGE HEADING
                   LINE-SIZE 255
                   LINE-COUNT 65.

INCLUDE zrcrm0001_top. "Data declaration
INCLUDE zrcrm0001_s01. "Selection screen
INCLUDE zrcrm0001_f01. "Function
INCLUDE zrcrm0001_o01. "PBO MODULE
INCLUDE zrcrm0001_i01. "PAI MODULE

*--------------------------------------------------------------------*
*START-OF-SELECTION.                                                 *
*--------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM check_sales.
  PERFORM retrieve_data.      "collect common data
  PERFORM build_summary_data. "build summary data
  PERFORM build_detail_data.  "build detail data
  PERFORM show_summary.

END-OF-SELECTION.
