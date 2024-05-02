*&---------------------------------------------------------------------*
*& INCLUDE          ZRPUR032_ATOP
*&---------------------------------------------------------------------*
TABLES: sscrfields.
TYPE-POOLS: slis,icon,truxs,abap.
DATA: functxt TYPE smp_dyntxt.

TABLES: ztpur032_poh_a, ztpur032_poi_a.

**********************************************************************
* Upload File
**********************************************************************
TYPES:
  BEGIN OF ty_excel,
    f01 TYPE string,
    f02 TYPE string,
    f03 TYPE string,
    f04 TYPE string,
    f05 TYPE string,
    f06 TYPE string,
    f07 TYPE string,
    f08 TYPE string,
    f09 TYPE string,
    f10 TYPE string,
    f11 TYPE string,
    f12 TYPE string,
    f13 TYPE string,
    f14 TYPE string,
    f15 TYPE string,
    f16 TYPE string,
    f17 TYPE string,
    f18 TYPE string,
    f19 TYPE string,
    f20 TYPE string,
    f21 TYPE string,
    f22 TYPE string,
    f23 TYPE string,
    f24 TYPE string,
    f25 TYPE string,
    f26 TYPE string,
  END OF ty_excel,
  tt_excel TYPE STANDARD TABLE OF ty_excel.

**********************************************************************
* Common Data
**********************************************************************
DATA:
  gt_poh TYPE TABLE OF ztpur032_poh_a,
  gs_poh LIKE LINE OF gt_poh,
  gt_poi TYPE TABLE OF ztpur032_poi_a,
  gs_poi LIKE LINE OF gt_poi.
FIELD-SYMBOLS: <gs_poh> LIKE LINE OF gt_poh,
               <gs_poi> LIKE LINE OF gt_poi.

DATA: gv_filename  TYPE  ztpur032_poh_a-filename,
      gv_error_msg TYPE string.


CONSTANTS: gc_name(6) TYPE c VALUE '採購單範本_'.

*DATA: gt_log TYPE TABLE OF ztmm006_log_a,
*      gs_log LIKE LINE OF gt_log.
*FIELD-SYMBOLS: <gfs_log> LIKE LINE OF gt_log.


**********************************************************************
* Global Data
**********************************************************************
DATA: g_msgid            TYPE string,
      g_status           TYPE char1,
      g_message          TYPE string,
      g_date             TYPE sy-datum,
      g_time             TYPE sy-uzeit,
      g_count            TYPE sy-tabix,
      g_do               TYPE sy-tabix,
      g_tbname           TYPE string,
      g_simu(1)          TYPE c,
      g_error(1)         TYPE c,
      g_no_process_bp(1) TYPE c,
      g_answer(1)        TYPE c.

**********************************************************************
* ALV
**********************************************************************
DATA:
  go_salv_table TYPE REF TO cl_salv_table.

DATA: gt_testrun TYPE zttpur032_alv_testrun_a,
      gs_testrun LIKE LINE OF gt_testrun.
FIELD-SYMBOLS: <gs_testrun> LIKE LINE OF gt_testrun.

DATA: gt_dfies_tab_h TYPE TABLE OF dfies,
      gt_dfies_tab_i TYPE TABLE OF dfies.

DATA: gt_alv LIKE gt_testrun,
      gs_alv LIKE LINE OF gt_alv.
FIELD-SYMBOLS: <alv> LIKE LINE OF gt_alv.


**********************************************************************
* SPLIT ALV
**********************************************************************
DATA: gr_salv_1      TYPE REF TO cl_salv_table,
      gr_salv_2      TYPE REF TO cl_salv_table,
      gr_splitter    TYPE REF TO cl_gui_splitter_container,
      gr_container_1 TYPE REF TO cl_gui_container,
      gr_container_2 TYPE REF TO cl_gui_container.

DATA: gt_alv_1 TYPE TABLE OF ztpur032_poh_a,
      gt_alv_2 TYPE TABLE OF ztpur032_poi_a.

FIELD-SYMBOLS: <gs_alv_1> LIKE LINE OF gt_alv_1,
               <gs_alv_2> LIKE LINE OF gt_alv_2.

**********************************************************************
* ICON
**********************************************************************
DATA: gv_led_gray   TYPE icon-id,
      gv_led_green  TYPE icon-id,
      gv_led_red    TYPE icon-id,
      gv_led_yellow TYPE icon-id.

CONSTANTS: gc_white  TYPE iconname VALUE 'ICON_LIGHT_OUT',
           gc_green  TYPE iconname VALUE 'ICON_LED_GREEN',
           gc_red    TYPE iconname VALUE 'ICON_LED_RED',
           gc_yellow TYPE iconname VALUE 'ICON_LED_YELLOW'.


**********************************************************************
* BAPI
**********************************************************************
CONSTANTS : c_x VALUE 'X'.

*Structures to hold PO header data
DATA: header  LIKE bapimepoheader,
      headerx LIKE bapimepoheaderx.

*Internal Tables to hold PO ITEM DATA
DATA: item  TYPE TABLE OF bapimepoitem,
      itemx TYPE TABLE OF bapimepoitemx.

DATA: poschedule  TYPE TABLE OF bapimeposchedule,
      poschedulex TYPE TABLE OF bapimeposchedulx.

DATA: poaccount  TYPE TABLE OF bapimepoaccount,
      poaccountx TYPE TABLE OF bapimepoaccountx.

DATA: popartner	 TYPE TABLE OF bapiekkop.

DATA: pocond  TYPE TABLE OF bapimepocond,
      pocondx	TYPE TABLE OF	bapimepocondx.

DATA: gv_testrun LIKE	bapiflag-bapiflag.
