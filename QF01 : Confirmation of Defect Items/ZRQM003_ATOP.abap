*&---------------------------------------------------------------------*
*& INCLUDE          ZRQM003_ATOP
*&---------------------------------------------------------------------*
TABLES: sscrfields.
TYPE-POOLS: slis,icon,truxs,abap.
DATA: functxt TYPE smp_dyntxt.

TABLES: ztqm003_m6_a,T001L.

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
*    f08 TYPE string,
*    f09 TYPE string,
*    f10 TYPE string,
*    f11 TYPE string,
*    f12 TYPE string,
*    f13 TYPE string,
*    f14 TYPE string,
*    f15 TYPE string,
*    f16 TYPE string,
*    f17 TYPE string,
*    f18 TYPE string,
*    f19 TYPE string,
*    f20 TYPE string,
*    f21 TYPE string,
*    f22 TYPE string,
*    f23 TYPE string,
*    f24 TYPE string,
*    f25 TYPE string,
*    f26 TYPE string,
  END OF ty_excel,
  tt_excel TYPE STANDARD TABLE OF ty_excel.

**********************************************************************
* Common Data
**********************************************************************
DATA:
  gt_data TYPE TABLE OF zsqm003_upload_a,
  gs_data LIKE LINE OF gt_data.
FIELD-SYMBOLS: <gs_data> LIKE LINE OF gt_data.

DATA:
  gt_qm003_m6 TYPE TABLE OF ztqm003_m6_a,
  gs_qm003_m6 LIKE LINE OF gt_qm003_m6.
FIELD-SYMBOLS: <gs_qm003_m6> LIKE LINE OF gt_qm003_m6.


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
DATA: gt_alv TYPE TABLE OF zsqm003_a,
      gs_alv LIKE LINE OF gt_alv.

FIELD-SYMBOLS: <gs_alv> LIKE LINE OF gt_alv.

DATA: gt_message LIKE gt_alv,
      gs_message LIKE LINE OF gt_message.
FIELD-SYMBOLS: <gs_message> LIKE LINE OF gt_message.

DATA:
  go_salv_table TYPE REF TO cl_salv_table.

DATA: gt_alv2 TYPE TABLE OF ZSQM003_2_A,
      gs_alv2 LIKE LINE OF gt_alv2.

FIELD-SYMBOLS: <gs_alv2> LIKE LINE OF gt_alv2.

**********************************************************************
* ALV Declarations
**********************************************************************
DATA: gt_fcat     TYPE lvc_t_fcat,
      gw_fieldtab LIKE LINE OF gt_fcat,
      gt_events   TYPE slis_t_event,
      gw_events   LIKE LINE OF gt_events,
      gs_layout   TYPE lvc_s_layo,
      g_repid     LIKE sy-repid,
      gs_style    TYPE lvc_s_styl,
      gt_style    TYPE lvc_t_styl,
      gt_sort     TYPE lvc_t_sort,
      gs_sort     LIKE LINE OF gt_sort.

DATA: g_html_height_top TYPE i.


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


***********************************************************************
** BAPI
***********************************************************************
*CONSTANTS : c_x VALUE 'X'.
*
**Structures to hold PO header data
*DATA: header  LIKE bapimepoheader,
*      headerx LIKE bapimepoheaderx.
*
**Internal Tables to hold PO ITEM DATA
*DATA: item  TYPE TABLE OF bapimepoitem,
*      itemx TYPE TABLE OF bapimepoitemx.
*
*DATA: poschedule  TYPE TABLE OF bapimeposchedule,
*      poschedulex TYPE TABLE OF bapimeposchedulx.
*
*DATA: poaccount  TYPE TABLE OF bapimepoaccount,
*      poaccountx TYPE TABLE OF bapimepoaccountx.
*
*DATA: gv_testrun LIKE  bapiflag-bapiflag.
