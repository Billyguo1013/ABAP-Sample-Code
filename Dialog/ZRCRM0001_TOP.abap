*&---------------------------------------------------------------------*
*&  Include           ZRCRM0001_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: esp1.

TABLES: vbak, vbkd, vbpa, vbap, t142t, vbfa,
        ztcrm0001, ztcrm0002, ztcrm0003.


TYPES: BEGIN OF ty_data,
         vbeln           TYPE vbeln_va,
         vkorg           TYPE vkorg,
         supplier        TYPE bukrs,
         disti           TYPE kunnr,
         orig_credit_amt TYPE kbetr,
         chg_back_amt    TYPE kbetr,
         waers           TYPE waers,
         grouping_no     TYPE zgrouping_no,
       END OF ty_data,

       BEGIN OF ty_group,
         supplier    TYPE bukrs,
         disti       TYPE kunnr,
         waers       TYPE waers,
         snro        TYPE char05,
         grouping_no TYPE zgrouping_no,
       END OF ty_group.

DATA: gt_data       TYPE TABLE OF ty_data,
      gs_data       TYPE ty_data,
      gt_group      TYPE TABLE OF ty_group,
      gs_group      TYPE ty_group,
      gt_vbak       TYPE TABLE OF vbak,
      gs_vbak       TYPE vbak,
      gt_vbkd       TYPE TABLE OF vbkd,
      gs_vbkd       TYPE vbkd,
      gt_vbfa       TYPE TABLE OF vbfa,
      gs_vbfa       TYPE vbfa,
      gt_vbap       TYPE TABLE OF vbap,
      gs_vbap       TYPE vbap,
      gt_konv       TYPE TABLE OF konv,
      gs_konv       TYPE konv,
      gt_mara       TYPE TABLE OF mara,
      gs_mara       TYPE mara,
      gt_vbrp       TYPE TABLE OF vbrp,
      gs_vbrp       TYPE vbrp,
      gt_ztcrm0001  TYPE TABLE OF ztcrm0001,
      gs_ztcrm0001  TYPE ztcrm0001,
      gt_ztcrm0002  TYPE TABLE OF ztcrm0002,
      gs_ztcrm0002  TYPE ztcrm0002,
      gt_ztcrm0003  TYPE TABLE OF ztcrm0003,
      gs_ztcrm0003  TYPE ztcrm0003.

DATA: g_raube TYPE raube,
      g_sales TYPE kunnr.

FIELD-SYMBOLS: <vbak>     LIKE LINE OF gt_vbak,
               <data>     LIKE LINE OF gt_data,
               <crm0001>  LIKE LINE OF gt_ztcrm0001,
               <crm0002>  LIKE LINE OF gt_ztcrm0002,
               <crm0003>  LIKE LINE OF gt_ztcrm0003.


**********************************************************************
* ALV Declarations
**********************************************************************
TYPE-POOLS: slis.

DATA: gt_fcat     TYPE slis_t_fieldcat_alv,
      gw_fieldtab LIKE LINE OF gt_fcat,
      gt_events   TYPE slis_t_event,
      gs_layout   TYPE slis_layout_alv,
      g_repid     LIKE sy-repid.

**********************************************************************
* ALV Declarations For Summary data
**********************************************************************
TYPES: BEGIN OF ty_summary,
          sel(1)          TYPE c,
          vkorg           TYPE vkorg,
          supplier        TYPE bukrs,
          disti           TYPE kunnr,
          orig_credit_amt TYPE kbetr,
          chg_back_amt    TYPE kbetr,
          waers           TYPE waers,
          grouping_no     TYPE zgrouping_no,
          group_flag      TYPE flag,
       END OF ty_summary.

DATA: gt_summary  TYPE TABLE OF ty_summary,
      gs_summary  TYPE ty_summary.
FIELD-SYMBOLS: <summary>  LIKE LINE OF gt_summary.


**********************************************************************
* ALV Declarations For Detail data
**********************************************************************
TYPES: BEGIN OF ty_detail,
       sel(1)           TYPE c,
       grouping_no      TYPE zgrouping_no, "Grouping number
       disti            TYPE kunnr,        "Disti
       vbeln            TYPE vbeln_va,     "Credit memo request number
       posnr            TYPE posnr_va,     "Credit memo request item
       zzpartno         LIKE mara-zzpartno,"Material[PART NUMBER]
       supplier         TYPE bukrs,        "Supplier
       amount           TYPE kbetr,        "Actual Claimed Amount
       chg_back_amt     TYPE kbetr,        "Charge-back amount
       waers            TYPE waers,        "Currency
       unit_price       LIKE ztcrm0003-unit_price, "Unit Price
       discount_rate    TYPE kbetr,        "Discount rate
       orig_disti_vbeln LIKE vbap-zzvgbel, "Original disti billing document
       orig_disti_posnr LIKE vbap-zzvgpos, "Original disti billing document item
       orig_disti_date  TYPE datum,        "Original disti billing date
       belnr            TYPE belnr_d,      "Accounting document number
       buzei            TYPE buzei,        "Accounting document item
    END OF ty_detail.

DATA: gt_detail       TYPE TABLE OF ty_detail,
      gt_detail_alv   TYPE TABLE OF ty_detail,
      gs_detail       TYPE ty_detail.
FIELD-SYMBOLS: <detail>     LIKE LINE OF gt_detail,
               <detail_alv> LIKE LINE OF gt_detail_alv.

DATA: g_container           TYPE scrfname VALUE 'CONTAINER1',
      go_grid1              TYPE REF TO cl_gui_alv_grid,
      gs_print              TYPE lvc_s_prnt,
      gs_layout2            TYPE lvc_s_layo,
      go_custom_container   TYPE REF TO cl_gui_custom_container,
      g_fcat                TYPE lvc_t_fcat.

DATA: ok_code LIKE sy-ucomm,
      save_ok LIKE sy-ucomm.

**********************************************************************
* Smartforms
**********************************************************************
DATA: g_path TYPE string.



**********************************************************************
* Macros
**********************************************************************
DEFINE field_desc_i.
  clear gw_fieldtab.
  gw_fieldtab-fieldname    = &1.
  gw_fieldtab-seltext_s    = &2.
  gw_fieldtab-seltext_m    = &3.
  gw_fieldtab-seltext_l    = &4.
  gw_fieldtab-reptext_ddic = &4.
  if gw_fieldtab-fieldname = 'SEL'.
    gw_fieldtab-tech = 'X'.
  endif.
  if gw_fieldtab-fieldname = 'ORIG_CREDIT_AMT'.
    gw_fieldtab-no_out = 'X'.
  endif.
  append gw_fieldtab to gt_fcat.
END-OF-DEFINITION.
