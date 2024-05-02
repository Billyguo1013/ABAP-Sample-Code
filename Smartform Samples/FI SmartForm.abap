************************************************************************
* Program Name  : ZRAR0054
* Descriptions  : AR Statement to Customer
* Create By     : V_BILLY
* Create Date   : 2022/03/11
* Tcode         :
************************************************************************
* Modification Log
************************************************************************
*   Date     Ver.   Programmer   Descriptions
* ---------- ----   ------------ --------------------------------------
*
************************************************************************

REPORT  zrar0054 MESSAGE-ID 00.

INCLUDE zrar0054_top. "Global variable
INCLUDE zrar0054_scn. "Screen build
INCLUDE zrar0054_f01. "Subroutines
INCLUDE zfunction.    "Common function

START-OF-SELECTION.

  PERFORM get_item.      "Get Item Data from TCODE : FBL5N
  PERFORM retrieve_data. "Get company & customer information
  PERFORM do_processing. "Generate Smartforms
  PERFORM show_alv.      "ALV Report

*&---------------------------------------------------------------------*
*&  Include           ZRAR0054_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS: rsds, slis, tpit.
TYPE-POOLS: icon.
TYPE-POOLS: sym.
TYPE-POOLS: line.
TYPE-POOLS: col.

TABLES : bsid, rfpdo1.

TYPES: BEGIN OF ty_company_info,
          bukrs       TYPE  t001-bukrs,
          adrnr       TYPE  t001-adrnr,
          stceg       TYPE  t001-stceg,
          name1       TYPE  adrc-name1,
          street      TYPE  adrc-street,
          country     TYPE  adrc-country,
          post_code1  TYPE  adrc-post_code1,
          tel_number  TYPE  adrc-tel_number,
          fax_number  TYPE  adrc-fax_number,
          landx       TYPE  t005t-landx,
          soa_date    TYPE  datum,
       END OF ty_company_info,

       BEGIN OF ty_customer_info,
          kunnr       TYPE  kna1-kunnr,
          adrnr       TYPE  kna1-adrnr,
          name1       TYPE  adrc-name1,
          name3       TYPE  adrc-name3,
          street      TYPE  adrc-street,
          str_suppl1  TYPE  adrc-str_suppl1,
          str_suppl2  TYPE  adrc-str_suppl2,
          str_suppl3  TYPE  adrc-str_suppl3,
          city1       TYPE  adrc-city1,
          post_code1  TYPE  adrc-post_code1,
          country     TYPE  adrc-country,
          landx       TYPE  t005t-landx,
          attention   TYPE  adrt-remark,
       END OF ty_customer_info,

       BEGIN OF ty_customer_email,
          kunnr       TYPE  kna1-kunnr,
          addrnumber  TYPE  adr6-addrnumber,
          persnumber  TYPE  adr6-persnumber,
          smtp_addr   TYPE  adr6-smtp_addr,
       END OF ty_customer_email.

DATA: gs_company_info   TYPE ty_company_info,
      gt_customer_info  TYPE TABLE OF ty_customer_info,
      gs_customer_info  TYPE ty_customer_info,
      gt_customer_email TYPE TABLE OF ty_customer_email,
      gs_customer_email TYPE ty_customer_email.

FIELD-SYMBOLS: <customer_email> LIKE LINE OF gt_customer_email,
               <customer_info>  LIKE LINE OF gt_customer_info.

DATA: gt_t001        TYPE TABLE OF t001,
      gs_t001        TYPE t001,
      gt_adrc        TYPE TABLE OF adrc,
      gs_adrc        TYPE adrc,
      gt_t005t       TYPE TABLE OF t005t,
      gs_t005t       TYPE t005t,
      gt_kna1        TYPE TABLE OF kna1,
      gs_kna1        TYPE kna1.


DATA: BEGIN OF ifaedt OCCURS 0, " Due date
            field1(10) TYPE c,
            field2(10) TYPE c,
            field3(50) TYPE c,
            day1 TYPE i ,
            day2 TYPE i ,
      END OF ifaedt.

DATA: gt_item TYPE TABLE OF rfposxext,
      gs_item TYPE rfposxext,
      gt_ar   TYPE TABLE OF rfposxext.
FIELD-SYMBOLS: <gfs_item> LIKE LINE OF gt_item,
               <ar>       LIKE LINE OF gt_ar.

TYPES: BEGIN OF ty_col,
          bukrs  TYPE rfposxext-bukrs, "Company Code
          konto	 TYPE rfposxext-konto, "Account Number(Customer)
          waers	 TYPE rfposxext-waers, "Document Currency (Key)
          augbl	 TYPE rfposxext-augbl, "Document Number of the Clearing Document
          rebzg	 TYPE rfposxext-rebzg, "Inv. Ref
          wrshb	 TYPE rfposxext-wrshb, "Amount in Document Currency (Foreign Currency)
       END OF ty_col,

       BEGIN OF ty_report,
          bukrs       TYPE rfposxext-bukrs,   "Company Code
          waers       TYPE rfposxext-waers,   "Curr.Code
          konto	      TYPE rfposxext-konto,   "Account Number(Customer)
          bldat	      TYPE rfposxext-bldat,   "Doc date
          belnr	      TYPE rfposxext-belnr,   "Doc No
          xblnr       TYPE rfposxext-xblnr,   "Ref No
          rebzg	      TYPE rfposxext-rebzg,   "Inv. Ref
          augbl	      TYPE rfposxext-augbl,   "Clrg.Doc
          faedt       TYPE rfposxext-faedt,   "Due Date
          wrshb_bf    TYPE rfposxext-wrshb,   "Balance B/F
          fwbas	      TYPE bset-fwbas,        "Sales
          fwste       TYPE bset-fwste,        "GST
          wrshb_c	    TYPE rfposxext-wrshb,   "Collection
          wrshb_cf    TYPE rfposxext-wrshb,   "Balance C/F
          days        TYPE i,                 "OverDueDays
          page_break  TYPE flag,
          total_flag  TYPE flag,
          totl_cust   TYPE flag,
      END OF ty_report,

      BEGIN OF ty_aging_total,
          waers       TYPE rfposxext-waers,   "Curr.Code
          current     TYPE rfposxext-wrshb,
          01_30       TYPE rfposxext-wrshb,
          31_60       TYPE rfposxext-wrshb,
          61_90       TYPE rfposxext-wrshb,
          91_120      TYPE rfposxext-wrshb,
          over_days   TYPE rfposxext-wrshb,
          date1_f     TYPE char3, "Add 2022/10/28
          date1_t     TYPE char3, "Add 2022/10/28
          date2_f     TYPE char3, "Add 2022/10/28
          date2_t     TYPE char3, "Add 2022/10/28
          date3_f     TYPE char3, "Add 2022/10/28
          date3_t     TYPE char3, "Add 2022/10/28
          date4_f     TYPE char3, "Add 2022/10/28
          date4_t     TYPE char3, "Add 2022/10/28
          date5_f     TYPE char3, "Add 2022/10/28
      END OF  ty_aging_total.

*Begin Add 2022/10/28
DATA: g_date1_f(3) TYPE i,
      g_date1_t(3) TYPE i,
      g_date2_f(3) TYPE i,
      g_date2_t(3) TYPE i,
      g_date3_f(3) TYPE i,
      g_date3_t(3) TYPE i,
      g_date4_f(3) TYPE i,
      g_date4_t(3) TYPE i,
      g_date5_f(3) TYPE i.
*End   Add 2022/10/28

DATA: gt_col          TYPE TABLE OF ty_col,
      gs_col          TYPE ty_col,
      gt_report       TYPE TABLE OF ty_report,
      gs_report       TYPE ty_report,
      gt_total        TYPE TABLE OF ty_report,
      gs_total        TYPE ty_report,
      gt_total_cust   TYPE TABLE OF ty_report,
      gs_total_cust   TYPE ty_report,
      gt_aging_total  TYPE TABLE OF ty_aging_total,
      gs_aging_total  TYPE ty_aging_total.

DATA: g_first_date  TYPE sy-datum.

FIELD-SYMBOLS: <col>    LIKE LINE OF gt_col,
               <report> LIKE LINE OF gt_report.

DATA: rspar LIKE rsparams OCCURS 10 WITH HEADER LINE.

DATA: i247 LIKE t247 OCCURS 0 WITH HEADER LINE.
DATA: texpr TYPE rsds_texpr.
DATA: gt_dyn_trange  TYPE rsds_trange.
DATA: w_dyn_range    TYPE rsds_range.
DATA: w_ranges       TYPE rsds_frange.
DATA: w_selopt       TYPE rsdsselopt.

FIELD-SYMBOLS : <gt_1> TYPE table ,
                <gs_1> TYPE ANY ,
                <f_1> TYPE ANY,
                <l_table> TYPE STANDARD TABLE,
                <l_line>  TYPE ANY,
                <l_field> TYPE ANY.
DATA: l_field1 TYPE string.

CONSTANTS: gc_formname TYPE tdsfname VALUE 'ZRAR0054'.

DATA: g_server_mode(1)  TYPE  c,
      g_filename        LIKE rlgrap-filename,
      g_filename_local  TYPE rlgrap-filename.


DATA: w_to  LIKE zswf0005 OCCURS 0 WITH HEADER LINE,
      w_subject TYPE so_obj_des,
      w_doc_type LIKE zswf0000-doc_type,
      w_mtype LIKE zswf0000-mtype,
      s_ret_msg LIKE swr_messag,
      w_body   TYPE bcsy_text,
      g_send_request TYPE REF TO cl_bcs,
      g_sender    TYPE REF TO if_sender_bcs,    " SENDER ADDRESS
      g_recipient TYPE REF TO if_recipient_bcs.  " RECIPIENT

DATA: gt_spoolids TYPE tsfspoolid,
      gs_spool    LIKE LINE OF gt_spoolids.

DATA: big_string TYPE xstring ,
      it_solix   TYPE solix_tab,
      gt_soli    TYPE soli_tab,
      gs_soli    TYPE soli.

CONSTANTS: g_memory(30) VALUE 'ZRAR0053'.

DATA: BEGIN OF obsid_o OCCURS 0,
            gjahr LIKE rfposxext-gjahr,     "Fiscal year
            bukrs LIKE rfposxext-bukrs,     "Company code
            kunnr LIKE rfposxext-u_kunnr,   "Customer
            name1 LIKE rfposxext-zcust_name,"Customer name
            bldat LIKE rfposxext-bldat,  "Date in Document
            budat LIKE rfposxext-budat,  "Posting Date in Document (1)
            faedt LIKE rfposxext-faedt,  "Net Due Date(2)
            xblnr LIKE rfposxext-xblnr,  "Reference Document Number(invoice no)(3-1)
            belnr LIKE rfposxext-belnr,  "Accounting Document Number(3-2)
            augbl LIKE rfposxext-augbl,  "Document Number of the Clearing Document(7)
            waers LIKE rfposxext-waers,  "Document Currency (Key)(8-1)
            wrbtr LIKE rfposxext-wrshb,  "Amount in Document Currency (Foreign Currency)-Balance B/F(9)
            wrbtr_di LIKE rfposxext-wrshb,  "Amount in Document Currency-invoice(10)
            wrbtr_dg LIKE rfposxext-wrshb,  "Amount in Document Currency-GST(11)
            wrbtr_dp LIKE rfposxext-wrshb,  "Amount in Document Currency-payment(12)
            wrbtr_d LIKE rfposxext-wrshb,   "Amount in Document Currency-Balance C/F(13)
            hwaer LIKE rfposxext-hwaer,     "Local Currency
            dmbtr LIKE rfposxext-dmshb,     "Amount in Local Currency with +/- Signs
            dmbtr_li LIKE rfposxext-dmshb,  "Amount in Local Currency-invoice
            dmbtr_lg LIKE rfposxext-dmshb,  "Amount in Local Currency-GST
            dmbtr_lp LIKE rfposxext-dmshb,  "Amount in Local Currency-payment
            dmbtr_l LIKE rfposxext-dmshb,   "Amount in Local Currency-Balance C/F
            exchd LIKE rfposxext-bwwrt,     "Exchange Rate diff Val Curr
            bwwrt LIKE rfposxext-bwwrt,     "Valuated Amount in Local Currency
            hkont LIKE rfposxext-hkont,     "General Ledger Account
            tabix LIKE sy-tabix,
            color_line(4) TYPE c,
            bukrs_d LIKE rfposxext-bukrs,     "Company code
      END OF obsid_o.

DATA: gt_result LIKE TABLE OF obsid_o,
      gs_result LIKE LINE OF gt_result.

**--------------------------------------------------------------------*
** ALV Declare
**--------------------------------------------------------------------*
TYPES: BEGIN OF ty_alv,
          kunnr     LIKE rfposxext-u_kunnr,     "Customer
          name1     LIKE rfposxext-zcust_name,  "Customer name
          rspoid    TYPE rspoid,                "Spool request number
          mail(1)   TYPE c,
          status    TYPE symsgv,                "Message Variable
       END OF ty_alv.

DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.

FIELD-SYMBOLS: <alv> LIKE LINE OF gt_alv.


DATA : BEGIN OF iwaers OCCURS 0 ,
        waers LIKE bsik-waers,
       END OF iwaers .

DATA:  gt_waers       LIKE TABLE OF iwaers,
       gs_waers       LIKE LINE OF gt_waers,
       gt_waers_local LIKE TABLE OF iwaers,
       gs_waers_local LIKE LINE OF gt_waers_local.


TYPES: BEGIN OF ty_doc_curr,
          kunnr     LIKE rfposxext-u_kunnr, "Customer
          waers     TYPE rfposxext-waers,   "Curr.Code
          wrshb_cf  TYPE rfposxext-wrshb,   "Balance C/F
        END OF ty_doc_curr,

        BEGIN OF ty_local_curr,
          kunnr     LIKE rfposxext-u_kunnr, "Customer
          hwaer     LIKE rfposxext-hwaer,   "Local Currency
          dmbtr_l   LIKE rfposxext-dmshb,   "Amount in Local Currency-Balance C/F
        END OF ty_local_curr.

DATA: gt_doc_curr   TYPE TABLE OF ty_doc_curr,
      gs_doc_curr   TYPE ty_doc_curr,
      gt_local_curr TYPE TABLE OF ty_local_curr,
      gs_local_curr TYPE ty_local_curr.


DATA : new_table TYPE REF TO data,
       new_line  TYPE REF TO data,
       is_lvc_cat TYPE lvc_s_fcat,
       it_lvc_cat TYPE lvc_t_fcat,
       t_week LIKE scal-week,
       ls_fieldcat TYPE slis_fieldcat_alv,
       lt_fieldcat TYPE slis_t_fieldcat_alv,
       is_layout TYPE slis_layout_alv ,
       gt_events    TYPE slis_t_event,
       l_layout     TYPE slis_layout_alv,
       l_linsz(5).

DATA : g_tabix LIKE sy-tabix.

DATA : p_tabnam TYPE tabname,
       lt_where TYPE TABLE OF edpline.
DATA : BEGIN OF t_dhead OCCURS 0,
             line(1000)  TYPE c    ,
      END OF t_dhead.

DATA: gt_tcurx TYPE TABLE OF tcurx.

*&---------------------------------------------------------------------*
*&  Include           ZRAR0054_SCN
*&---------------------------------------------------------------------*

************************************************************************
* Selection-Criteria
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-s01.

PARAMETERS: p_bukrs TYPE bsid-bukrs DEFAULT 'NES2' OBLIGATORY,
            p_budat TYPE bsid-budat OBLIGATORY.

SELECT-OPTIONS: s_kunnr FOR bsid-kunnr,
                s_waers FOR bsid-waers,
                s_hkont FOR bsid-hkont.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(31) text-s02 FOR FIELD rastbis1.
PARAMETERS: rastbis1 LIKE rfpdo1-allgrogr DEFAULT 000.
PARAMETERS: rastbis2 LIKE rfpdo1-allgrogr DEFAULT 030.
PARAMETERS: rastbis3 LIKE rfpdo1-allgrogr DEFAULT 060.
PARAMETERS: rastbis4 LIKE rfpdo1-allgrogr DEFAULT 090.
PARAMETERS: rastbis5 LIKE rfpdo1-allgrogr DEFAULT 120.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_send TYPE flag AS CHECKBOX DEFAULT '',
            x_norm LIKE itemset-xnorm AS CHECKBOX DEFAULT 'X',
            x_shbv LIKE itemset-xshbv AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK blk1.

*--------------------------------------------------------------------*
*INITIALIZATION                                                      *
*--------------------------------------------------------------------*
INITIALIZATION.

*--------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT                                          *
*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'RASTBIS1' OR screen-name = 'RASTBIS2'
    OR screen-name = 'RASTBIS3' OR screen-name = 'RASTBIS4'
    OR screen-name = 'RASTBIS5'.

      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*&  Include           ZRAR0054_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  RETRIEVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM retrieve_data .


  DATA:
        returncode        TYPE ad_retcode,
        it_selection_tab  TYPE TABLE OF	addr_addr_pers_line,
        is_selection_tab  LIKE LINE OF it_selection_tab,
        et_adr6	          TYPE TABLE OF	adr6,
        es_adr6	          TYPE adr6,
        et_error_table    TYPE TABLE OF addr_error.

  DATA: l_remark          TYPE ad_remark2.

*--------------------------------------------------------------------*
* "Get Company information
*--------------------------------------------------------------------*
  CLEAR : gs_t001, gs_adrc, gs_t005t, gs_kna1.

  SELECT SINGLE * FROM t001 INTO gs_t001
    WHERE bukrs = p_bukrs.

  SELECT SINGLE * FROM adrc INTO gs_adrc
    WHERE addrnumber = gs_t001-adrnr
    AND nation = ' '.

  SELECT SINGLE * FROM t005t INTO gs_t005t
    WHERE spras = sy-langu
    AND   land1 = gs_adrc-country.

  gs_company_info-bukrs      = gs_t001-bukrs.
  gs_company_info-adrnr      = gs_t001-adrnr.
  gs_company_info-stceg      = gs_t001-stceg.
  gs_company_info-name1      = gs_adrc-name1.
  gs_company_info-street     = gs_adrc-street.
  gs_company_info-country    = gs_adrc-country.
  gs_company_info-post_code1 = gs_adrc-post_code1.
  gs_company_info-landx      = gs_t005t-landx.
  gs_company_info-tel_number = gs_adrc-tel_number.
  gs_company_info-fax_number = gs_adrc-fax_number.
  gs_company_info-soa_date   = p_budat.
  CONDENSE gs_company_info-tel_number NO-GAPS.
  CONDENSE gs_company_info-fax_number NO-GAPS.

*--------------------------------------------------------------------*
* "Get Customer information
*--------------------------------------------------------------------*
  LOOP AT gt_report INTO gs_report.

    CLEAR gs_kna1.
    SELECT SINGLE * INTO gs_kna1 FROM kna1
      WHERE kunnr = gs_report-konto.

    CLEAR : gs_customer_info.
    CLEAR : gs_adrc, gs_t005t.

    SELECT SINGLE * FROM adrc INTO gs_adrc
      WHERE addrnumber = gs_kna1-adrnr
      AND nation = ' '.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM adrc INTO gs_adrc
        WHERE addrnumber = gs_kna1-adrnr
        AND nation = 'I'.
    ENDIF.

    SELECT SINGLE * FROM t005t INTO gs_t005t
      WHERE spras = sy-langu
      AND   land1 = gs_adrc-country.

    APPEND INITIAL LINE TO gt_customer_info ASSIGNING <customer_info>.
    <customer_info>-kunnr      = gs_kna1-kunnr.
    <customer_info>-adrnr      = gs_kna1-adrnr.
    <customer_info>-name1      = gs_adrc-name1.
    <customer_info>-name3      = gs_adrc-name3.
    <customer_info>-street     = gs_adrc-street.
    <customer_info>-str_suppl1 = gs_adrc-str_suppl1.
    <customer_info>-str_suppl2 = gs_adrc-str_suppl2.
    <customer_info>-str_suppl3 = gs_adrc-str_suppl3.
    <customer_info>-city1      = gs_adrc-city1.
    <customer_info>-post_code1 = gs_adrc-post_code1.
    <customer_info>-country    = gs_adrc-country.
    <customer_info>-landx      = gs_t005t-landx.

    REFRESH it_selection_tab.
    CLEAR is_selection_tab.
    is_selection_tab-addrnumber = gs_adrc-addrnumber.
    APPEND is_selection_tab TO it_selection_tab.

    CALL FUNCTION 'ADDR_SELECT_ADR6_ARRAY'
      IMPORTING
        returncode       = returncode
      TABLES
        it_selection_tab = it_selection_tab
        et_adr6          = et_adr6
        et_error_table   = et_error_table
      EXCEPTIONS
        no_entries_found = 1
        parameter_error  = 2
        internal_error   = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
    ELSE.
      LOOP AT et_adr6 INTO es_adr6 WHERE flg_nouse = ''.
        APPEND INITIAL LINE TO gt_customer_email ASSIGNING <customer_email>.
        <customer_email>-kunnr       =  gs_kna1-kunnr.
        <customer_email>-addrnumber  =  es_adr6-addrnumber.
        <customer_email>-persnumber  =  es_adr6-persnumber.
        <customer_email>-smtp_addr   =  es_adr6-smtp_addr.

        "Get Notes information for Attention
        SELECT SINGLE remark
          INTO l_remark
          FROM adrt
        WHERE addrnumber = gs_adrc-addrnumber
          AND consnumber = es_adr6-consnumber
          AND langu      = sy-langu.
*        <customer_info>-attention  = l_remark.
      ENDLOOP.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO gt_customer_email ASSIGNING <customer_email>.
        <customer_email>-kunnr       =  gs_kna1-kunnr.
        <customer_email>-addrnumber  =  space.
        <customer_email>-persnumber  =  space.
        <customer_email>-smtp_addr   =  space.
      ENDIF.

    ENDIF.
  ENDLOOP.

  SORT gt_customer_info.
  DELETE ADJACENT DUPLICATES FROM gt_customer_info COMPARING ALL FIELDS.

  SORT gt_customer_email.
  DELETE ADJACENT DUPLICATES FROM gt_customer_email COMPARING ALL FIELDS.


ENDFORM.                    " RETRIEVE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DUE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_due_date .
  DATA : t_items TYPE i ,
         l_day1 TYPE i,
         l_day2 TYPE i,
         l_field1 TYPE string,
         l_field2 TYPE string,
         l_field3 TYPE string,
         l_field4 TYPE string,
         t_i1(1) TYPE n,
         t_i2(1) TYPE n,
         t_day1 TYPE string,
         t_day2 TYPE string,
         t_str  TYPE string,
         t_from TYPE string,
         t_to   TYPE string,
         t_field TYPE string.
  FIELD-SYMBOLS: <f1> TYPE ANY.
  FIELD-SYMBOLS: <f2> TYPE ANY.
  t_items = 6 .
  t_i1 = 1 .
  DO t_items TIMES .
    CONCATENATE 'RASTBIS' t_i1 INTO l_field1.
    ASSIGN (l_field1) TO <f1>.
    IF t_i1 = 1 .
      CONDENSE t_str.
      CONCATENATE 'F' <f1> INTO t_from.
      CONCATENATE 'T' <f1> INTO t_to.
      l_day1 = -999.
      l_day2 = <f1>.
      t_day1 = ''.
      t_day2 = l_day2.
    ELSE.
      t_i2 = t_i1 - 1 .
      CONCATENATE 'RASTBIS' t_i2 INTO l_field4.
      ASSIGN (l_field4) TO <f2>.
      l_day1 = <f2> + 1.
      CONCATENATE 'F' <f2> INTO t_from.
      IF t_i1 =  6 .
        l_day2 = 9999 .
        t_to = 'T9999' .
        t_day1 = l_day1.
        t_day2 = ''.
      ELSE.
        l_day2 = <f1> .
        CONCATENATE 'T' <f1> INTO t_to.
        t_day1 = l_day1.
        t_day2 = l_day2.
      ENDIF.
    ENDIF.
    CONDENSE t_from.
    CONDENSE t_day1.
    CONDENSE t_day2.
    CONCATENATE t_from  t_to INTO l_field2.
*      CONCATENATE T_DAY1 ' ~ ' T_DAY2 INTO L_FIELD3  SEPARATED BY ' '.
    CONCATENATE t_day1 ' ~ ' t_day2 INTO l_field3.
    ifaedt-field1 = l_field1.
    ifaedt-field2 = l_field2.
    ifaedt-field3 = l_field3.
    ifaedt-day1 = l_day1.
    ifaedt-day2 = l_day2.
    APPEND ifaedt.
    t_i1 = t_i1 + 1 .
    CLEAR : ifaedt,t_from,t_to,l_field1,l_field2,l_field3,l_field4.
  ENDDO.
ENDFORM.                    " GET_DUE_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_item.

**  "Initialize
*  PERFORM init.
*
*  "Previous period - open items
*  PERFORM get_previous_open_items.
*
*  "Previous period - open and cleared items in current period
*  PERFORM get_cleared_items.
*
*  "Current period-open items
*  PERFORM get_current_open_items.


  DATA: text       TYPE c LENGTH 10,
        rspar_tab  TYPE TABLE OF rsparams,
        rspar_line LIKE LINE OF rspar_tab,
        range_tab  LIKE RANGE OF text,
        range_line LIKE LINE OF range_tab.

  FIELD-SYMBOLS:<fs_table> TYPE ANY TABLE,
                <fs_any>   TYPE ANY. "LIKE LINE OF  it_tab .
  DATA:gr_data TYPE REF TO data.

*  cl_salv_bs_runtime_info=>clear_all( ).
*  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
*                                          metadata = abap_false
*                                          data     = abap_true ).

  CLEAR rspar_line.
  rspar_line-selname = 'P_BUKRS'.
  rspar_line-kind    = 'P'.
  rspar_line-sign    = 'I'.
  rspar_line-option  = 'EQ'.
  rspar_line-low     = p_bukrs.
  APPEND rspar_line TO rspar_tab.

  CLEAR rspar_line.
  rspar_line-selname = 'P_STID2'.
  rspar_line-kind    = 'P'.
  rspar_line-sign    = 'I'.
  rspar_line-option  = 'EQ'.
  rspar_line-low     = p_budat.
  APPEND rspar_line TO rspar_tab.

  CLEAR rspar_line.
  IF NOT s_kunnr[] IS INITIAL.
    rspar_line-selname = 'S_KUNNR'.
    rspar_line-kind    = 'S'.
    LOOP AT s_kunnr.
      MOVE-CORRESPONDING s_kunnr TO rspar_line.
      APPEND rspar_line TO rspar_tab.
    ENDLOOP.
    CLEAR rspar_line.
  ELSE.
    rspar_line-selname = 'S_KUNNR'.
    rspar_line-kind    = 'S'.
    APPEND rspar_line TO rspar_tab.
    CLEAR rspar_line.
  ENDIF.

  CLEAR rspar_line.
  IF NOT s_hkont[] IS INITIAL.
    rspar_line-selname = 'S_HKONT'.
    rspar_line-kind    = 'S'.
    LOOP AT s_hkont.
      MOVE-CORRESPONDING s_hkont TO rspar_line.
      APPEND rspar_line TO rspar_tab.
    ENDLOOP.
    CLEAR rspar_line.
  ELSE.
    rspar_line-selname = 'S_HKONT'.
    rspar_line-kind    = 'S'.
    APPEND rspar_line TO rspar_tab.
    CLEAR rspar_line.
  ENDIF.

  CLEAR rspar_line.
  rspar_line-selname = 'X_NORM'.
  rspar_line-kind    = 'P'.
  rspar_line-sign    = 'I'.
  rspar_line-option  = 'EQ'.
  rspar_line-low     = x_norm.
  APPEND rspar_line TO rspar_tab.

  CLEAR rspar_line.
  rspar_line-selname = 'X_SHBV'.
  rspar_line-kind    = 'P'.
  rspar_line-sign    = 'I'.
  rspar_line-option  = 'EQ'.
  rspar_line-low     = x_shbv.
  APPEND rspar_line TO rspar_tab.

*--------------------------------------------------------------------*
* Sunbit ZRAR0053 to get Item Result
*--------------------------------------------------------------------*
  SUBMIT zrar0053
  EXPORTING LIST TO MEMORY
  WITH SELECTION-TABLE rspar_tab[]
                       AND RETURN.

  IMPORT gt_result TO gt_result FROM MEMORY ID g_memory.
  FREE MEMORY ID g_memory.

  IF gt_result IS NOT INITIAL.
    LOOP AT gt_result INTO gs_result.
      APPEND INITIAL LINE TO gt_report ASSIGNING <report>.
      <report>-bukrs      = gs_result-bukrs.    "Company Code
      <report>-waers      = gs_result-waers.    "Curr.Code
      <report>-konto      = gs_result-kunnr.    "Account Number(Customer)
      <report>-bldat      = gs_result-bldat.    "Doc date
      <report>-belnr      = gs_result-belnr.    "Doc No
      <report>-xblnr      = gs_result-xblnr.    "Ref No
      <report>-rebzg      = gs_result-belnr.    "Inv. Ref
      <report>-augbl      = gs_result-augbl.    "Clrg.Doc
      <report>-faedt      = gs_result-faedt.    "Due Date
      <report>-wrshb_bf   = gs_result-wrbtr.    "Balance B/F
      <report>-fwbas      = gs_result-wrbtr_di. "Sales  (BSET-FWBAS Sales)
      <report>-fwste      = gs_result-wrbtr_dg. "GST
      <report>-wrshb_c    = gs_result-wrbtr_dp. "Collection
      <report>-wrshb_cf   = gs_result-wrbtr_d.  "Balance C/F

      "(Report Date – a.FAEDT )-1 as Overdue_days
      PERFORM get_days USING gs_result-faedt <report>-days.

*--------------------------------------------------------------------*
*     For ALV use ,Begin
*--------------------------------------------------------------------*
      IF s_waers[] IS NOT INITIAL.
        CHECK <report>-waers IN s_waers[].
      ENDIF.

      CLEAR gs_waers.
      gs_waers-waers = <report>-waers.
      COLLECT gs_waers INTO gt_waers.

      CLEAR gs_waers_local.
      gs_waers_local-waers = gs_result-hwaer. "Local Currency
      COLLECT gs_waers_local INTO gt_waers_local.

      CLEAR gs_doc_curr.
      gs_doc_curr-kunnr    =  <report>-konto.
      gs_doc_curr-waers    =  <report>-waers.
      gs_doc_curr-wrshb_cf = <report>-wrshb_cf.
      PERFORM amount_display_o_sap USING gs_doc_curr-waers
                                         gs_doc_curr-wrshb_cf.
      COLLECT gs_doc_curr INTO gt_doc_curr.

      CLEAR gs_local_curr.
      gs_local_curr-kunnr    =  <report>-konto.
      gs_local_curr-hwaer   = gs_result-hwaer.
      gs_local_curr-dmbtr_l =  gs_result-dmbtr_l.
      PERFORM amount_display_o_sap USING gs_local_curr-hwaer
                                         gs_local_curr-dmbtr_l.
      COLLECT gs_local_curr INTO gt_local_curr.
*--------------------------------------------------------------------*
*     For ALV use ,End
*--------------------------------------------------------------------*


    ENDLOOP.
  ELSE.
    MESSAGE text-e01 TYPE 'E'.
  ENDIF.

  "Currency filter
  IF s_waers[] IS NOT INITIAL.
    DELETE gt_report WHERE waers NOT IN s_waers[].
  ENDIF.

  IF gt_report IS INITIAL.
    MESSAGE text-e01 TYPE 'E'.
  ENDIF.

ENDFORM.                    " GET_ITEM
*&---------------------------------------------------------------------*
*&      Form  GET_PREVIOUS_OPEN_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_previous_open_items .

  PERFORM append_fbl5n_rspar_pre.
  PERFORM append_dynamic_selection.
  PERFORM submit_fbl5n_to_intab_pre.

ENDFORM.                    " GET_PREVIOUS_OPEN_ITEMS
*&---------------------------------------------------------------------*
*&      Form  GET_CLEARED_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cleared_items .

  PERFORM append_fbl5n_rspar_clear.
  PERFORM append_dynamic_selection.
  PERFORM submit_fbl5n_to_intab_clear.

ENDFORM.                    " GET_CLEARED_ITEMS
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_OPEN_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_current_open_items .

  PERFORM append_fbl5n_rspar_current.
  PERFORM append_dynamic_selection.
  PERFORM submit_fbl5n_to_intab_current.

ENDFORM.                    " GET_CURRENT_OPEN_ITEMS
*&---------------------------------------------------------------------*
*&      Form  APPEND_FBL5N_RSPAR_PRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_fbl5n_rspar_pre.
  CLEAR rspar[].
* FOR COMPANY CODE
  rspar-selname = 'KD_BUKRS'.
  rspar-kind    = 'S'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = p_bukrs.
  APPEND rspar.
  CLEAR : rspar.
  IF NOT s_kunnr[] IS INITIAL  .
* FOR Customer account
    rspar-selname = 'DD_KUNNR'.
    rspar-kind    = 'S'.
    LOOP AT s_kunnr.
      MOVE-CORRESPONDING s_kunnr TO rspar.
      APPEND rspar.
    ENDLOOP.
    CLEAR : rspar.
  ELSE.
    rspar-selname = 'DD_KUNNR'.
    rspar-kind    = 'S'.
    APPEND rspar.
    CLEAR : rspar.
  ENDIF.
* FOR DISPLAYING ALL ITEMS, DESELECT THE RADIOBUTTON FOR OPEN ITEMS / CLEARED ITEMS.
  rspar-selname = 'X_OPSEL'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = 'X'.
  APPEND rspar.
  CLEAR : rspar.
  rspar-selname = 'X_CLSEL'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = ' '.
  APPEND rspar.
  CLEAR : rspar.
* SELECT RADIOBUTTON FOR ALL ITEMS
  rspar-selname = 'X_AISEL'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = ' '.
  APPEND rspar.
  CLEAR : rspar.

* FOR Open at key date (Report - 1 month)
  DATA: date      TYPE  p0001-begda,
        calc_date TYPE  p0001-begda.

  CLEAR: date , calc_date.
  date = p_budat.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = date
      days      = '00'
      months    = '01'
      signum    = '-'
      years     = '00'
    IMPORTING
      calc_date = calc_date.

  rspar-selname = 'PA_STIDA'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = calc_date.
  APPEND rspar.
  CLEAR : rspar.

ENDFORM.                    " APPEND_FBL1N_RSPAR
*&---------------------------------------------------------------------*
*&      Form  APPEND_DYNAMIC_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_dynamic_selection .
  CLEAR : w_selopt,w_ranges , w_dyn_range.
  CLEAR : gt_dyn_trange[].
*Dynamic Selection Start
* G/L Account
  w_dyn_range-tablename = 'BSIK'.
  w_ranges-fieldname    = 'HKONT'.

  LOOP AT s_hkont.
    MOVE-CORRESPONDING s_hkont TO w_selopt .
    APPEND w_selopt    TO w_ranges-selopt_t.

    CLEAR : w_selopt.
  ENDLOOP.
  APPEND w_ranges    TO w_dyn_range-frange_t.
  APPEND w_dyn_range TO gt_dyn_trange.

  CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
    EXPORTING
      field_ranges = gt_dyn_trange
    IMPORTING
      expressions  = texpr.
ENDFORM.                    " APPEND_DYNAMIC_SELECTION
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_FBL5N_TO_INTAB_PRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM submit_fbl5n_to_intab_pre.
  DATA: ls_data TYPE REF TO data,
        t_dmbtr LIKE glt0-hslvt,
        t_objct(30) TYPE c,
        t_month(2) TYPE n,
        t_date LIKE anlb-afabg.

  DATA: l_gjahr TYPE gjahr,
        l_buzei TYPE buzei.

  FIELD-SYMBOLS: <lt_data> TYPE table.

  REFRESH: gt_item,gt_col.

  cl_salv_bs_runtime_info=>set(
          display = abap_false
          metadata = abap_false
          data = abap_true ).

  SUBMIT rfitemar
         WITH SELECTION-TABLE rspar[]
         WITH FREE SELECTIONS texpr AND RETURN.

  TRY.

      cl_salv_bs_runtime_info=>get_data_ref(
              IMPORTING r_data = ls_data ).
      ASSIGN ls_data->* TO <lt_data>.

    CATCH cx_salv_bs_sc_runtime_info.
      MESSAGE `Unable to retrieve ALV data` TYPE 'E'.
  ENDTRY.

*  BREAK-POINT.

  IF sy-subrc = 0 .
    LOOP AT <lt_data> ASSIGNING <gs_1>.
      APPEND INITIAL LINE TO gt_item ASSIGNING <gfs_item>.
      MOVE-CORRESPONDING <gs_1> TO <gfs_item>.
    ENDLOOP.

    DELETE gt_item WHERE augbl <> ''.
  ELSE.
  ENDIF.
  cl_salv_bs_runtime_info=>clear_all( ).

  CHECK gt_item IS NOT INITIAL.

  SORT gt_item BY bldat. "Doc date

  "Find Collection information (partial payment) "Group by BUKRS, KONTO, REBZG, WAERS
  LOOP AT gt_item ASSIGNING <gfs_item> WHERE xzahl = 'X'.
    CLEAR gs_col.
    gs_col-bukrs = gs_item-bukrs.
    gs_col-konto = gs_item-konto.
    gs_col-waers = gs_item-waers.
    gs_col-rebzg = gs_item-rebzg.
    gs_col-wrshb = gs_item-wrshb.
    COLLECT gs_col INTO gt_col.
  ENDLOOP.

  "Find AR information (DOC:1600000003/1600000009/1600000010)
  LOOP AT gt_item ASSIGNING <gfs_item> WHERE xzahl = ''.

    CHECK <gfs_item>-belnr = <gfs_item>-rebzg. "Doc No = InvRef

    APPEND INITIAL LINE TO gt_report ASSIGNING <report>.
    <report>-bukrs = <gfs_item>-bukrs.  "Company Code
    <report>-konto = <gfs_item>-konto.  "Account Number(Customer)
    <report>-bldat = <gfs_item>-bldat.  "Doc date
    <report>-belnr = <gfs_item>-belnr.  "Doc No
    <report>-xblnr = <gfs_item>-xblnr.  "Ref No
    <report>-rebzg = <gfs_item>-rebzg.  "Inv. Ref
    <report>-augbl = <gfs_item>-augbl.  "Clrg.Doc
    <report>-faedt = <gfs_item>-faedt.  "Due Date
    <report>-wrshb_bf = <gfs_item>-wrshb. "Balance B/F
    <report>-waers = <gfs_item>-waers.  "Curr.Code

    "(Report Date – a.FAEDT )-1 as Overdue_days
    PERFORM get_days USING <gfs_item>-faedt <report>-days.

    READ TABLE gt_col INTO gs_col WITH KEY bukrs = gs_col-bukrs
                                           konto = gs_col-konto
                                           rebzg = gs_col-rebzg
                                           waers = gs_col-waers.
    IF sy-subrc = 0.
      <report>-wrshb_c = gs_col-wrshb.
    ENDIF.

  ENDLOOP.




ENDFORM.                    " SUBMIT_FBL5N_TO_INTAB
*&---------------------------------------------------------------------*
*&      Form  GET_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OBSIK_FAEDT  text
*      -->P_OBSIK_DAYS  text
*----------------------------------------------------------------------*
FORM get_days  USING  u_faedt
                      u_days.
  CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
    EXPORTING
      begda    = u_faedt
      endda    = p_budat
      tab_mode = ' '
    IMPORTING
      days     = u_days.
  u_days = u_days - 1 .
ENDFORM.                      " GET_DAYS
*&---------------------------------------------------------------------*
*&      Form  GET_BSET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<COL>_BUKRS  text
*      -->P_<REPORT>_BELNR  text
*      -->P_L_GJAHR  text
*      -->P_L_BUZEI  text
*      <--P_<REPORT>_FWBAS  text
*      <--P_<REPORT>_FWSTE  text
*----------------------------------------------------------------------*
FORM get_bset  USING    p_bukrs
                        p_belnr
                        p_gjahr
                        p_buzei
               CHANGING c_fwbas
                        c_fwste.

  DATA: ls_bset TYPE bset.

  CLEAR ls_bset.

  SELECT SINGLE * INTO ls_bset FROM bset
    WHERE bukrs = p_bukrs
    AND   belnr = p_belnr
    AND   gjahr = p_gjahr
    AND   buzei = p_buzei.

  c_fwbas = ls_bset-fwbas.
  c_fwste = ls_bset-fwste.

ENDFORM.                    " GET_BSET
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .

ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  DO_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_processing .

  DATA: fm_name           TYPE rs38l_fnam.
  DATA:
    lv_control_parameters TYPE ssfctrlop,
    ls_job_options        TYPE ssfcresop,
    ls_document           TYPE ssfcrespd,
    ls_job_info           TYPE ssfcrescl.

  DATA:l_output_options TYPE ssfcompop.

  DATA: lv_count  TYPE i,
        lv_tabix  TYPE sy-tabix.

  DATA: lt_item   TYPE TABLE OF zsar0054_item, "smartform item
        ls_item   LIKE LINE OF lt_item.

  FIELD-SYMBOLS: <item> LIKE LINE OF lt_item.

  DATA: l_new_curr    TYPE flag,
        l_waers       TYPE rfposxext-waers,  "Curr.Code
        l_page_break  TYPE flag,
        l_total_flag  TYPE flag.

  "initization
  REFRESH gt_alv.

  lv_count = LINES( gt_customer_info ).

  PERFORM determine_duedate_sort. "<< Add DNRK944687 2022/10/28

*每個Customer打印一張
*每張以 currency 拆分
  LOOP AT gt_customer_info INTO gs_customer_info.

    "initialize global
    REFRESH: gt_total,
             gt_total_cust,
             gt_aging_total.

    CLEAR: l_waers.

    REFRESH lt_item.
    lt_item[] = gt_report.
    DELETE lt_item[] WHERE konto <> gs_customer_info-kunnr.

    SORT lt_item BY waers
                    bldat ASCENDING.

* one customer -> one alv result
    APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
    MOVE-CORRESPONDING gs_customer_info TO <alv>.

    LOOP AT lt_item ASSIGNING <item>.

      lv_tabix = sy-tabix.

      "TOTAL:
      "After printing detailed items by currency
      "then do the total of "Balance B/F" and "Sales" and "GST" and "Collection" and "Balance C/F"
      CLEAR gs_total.
      gs_total-waers    = <item>-waers.
      gs_total-wrshb_bf = <item>-wrshb_bf.    "Balance B/F
      gs_total-fwbas    = <item>-fwbas.       "Sales
      gs_total-fwste    = <item>-fwste.       "GST
      gs_total-wrshb_c  = <item>-wrshb_c.     "Collection
      gs_total-wrshb_cf = <item>-wrshb_cf.    "Balance C/F
      COLLECT gs_total INTO gt_total.

      "AGING TOTAL:
      CLEAR gs_aging_total.
      gs_aging_total-waers = <item>-waers.
*      IF <item>-days < 1.
*        gs_aging_total-current = <item>-wrshb_cf.
*      ELSEIF ( <item>-days >= 1 AND <item>-days <= 30 ).
*        gs_aging_total-01_30   = <item>-wrshb_cf.
*      ELSEIF ( <item>-days >= 31 AND <item>-days <= 60 ).
*        gs_aging_total-31_60   = <item>-wrshb_cf.
*      ELSEIF ( <item>-days >= 61 AND <item>-days <= 90 ).
*        gs_aging_total-61_90   = <item>-wrshb_cf.
*      ELSEIF ( <item>-days >= 91 AND <item>-days <= 120 ).
*        gs_aging_total-91_120   = <item>-wrshb_cf.
*      ELSEIF <item>-days > 120.
*        gs_aging_total-over_days = <item>-wrshb_cf.
*      ENDIF.
      IF <item>-days < 1.
        gs_aging_total-current = <item>-wrshb_cf.
      ELSEIF ( <item>-days >= g_date1_f AND <item>-days <= g_date1_t ).

        IF g_date1_f NE 0.
          gs_aging_total-01_30   = <item>-wrshb_cf.
        ENDIF.

      ELSEIF ( <item>-days >= g_date2_f AND <item>-days <= g_date2_t ).

        IF g_date2_f NE 0.
          gs_aging_total-31_60   = <item>-wrshb_cf.
        ENDIF.

      ELSEIF ( <item>-days >= g_date3_f AND <item>-days <= g_date3_t ).

        IF g_date3_f NE 0.
          gs_aging_total-61_90   = <item>-wrshb_cf.
        ENDIF.

      ELSEIF ( <item>-days >= g_date4_f AND <item>-days <= g_date4_t ).

        IF g_date4_f NE 0.
          gs_aging_total-91_120   = <item>-wrshb_cf.
        ENDIF.

      ELSEIF <item>-days > g_date5_f.

        IF g_date5_f NE 0.
          gs_aging_total-over_days = <item>-wrshb_cf.
        ENDIF.

      ENDIF.

*<< For Smartform diplay date text
      WRITE : g_date1_f TO gs_aging_total-date1_f LEFT-JUSTIFIED,
              g_date1_t TO gs_aging_total-date1_t LEFT-JUSTIFIED,
              g_date2_f TO gs_aging_total-date2_f LEFT-JUSTIFIED,
              g_date2_t TO gs_aging_total-date2_t LEFT-JUSTIFIED,
              g_date3_f TO gs_aging_total-date3_f LEFT-JUSTIFIED,
              g_date3_t TO gs_aging_total-date3_t LEFT-JUSTIFIED,
              g_date4_f TO gs_aging_total-date4_f LEFT-JUSTIFIED,
              g_date4_t TO gs_aging_total-date4_t LEFT-JUSTIFIED,
              g_date5_f TO gs_aging_total-date5_f LEFT-JUSTIFIED.

      COLLECT gs_aging_total INTO gt_aging_total.

*<< 分頁控制
      CLEAR l_page_break.
      IF l_waers IS INITIAL.
        l_waers = <item>-waers.
      ELSE.
        IF l_waers = <item>-waers.
          "Do nothing
        ELSE.
          l_page_break = 'X'.
          l_waers = <item>-waers.
        ENDIF.
      ENDIF.
      <item>-page_break =  l_page_break.

*<< 小計控制
      CLEAR l_total_flag.
      AT END OF waers.
        l_total_flag = 'X'.
      ENDAT.
      <item>-total_flag =  l_total_flag.

    ENDLOOP.

    "CUSTOMER TOTAL
    LOOP AT gt_total INTO gs_total.
      CLEAR gs_total_cust.
      MOVE-CORRESPONDING gs_total TO gs_total_cust.

*<< 總計控制
      IF sy-tabix = 1.
        gs_total_cust-totl_cust = 'X'.
      ENDIF.

      APPEND gs_total_cust TO gt_total_cust.
    ENDLOOP.

**********************************************************
*  Smartform                                             *
**********************************************************
    lv_control_parameters-no_open  = space.
    lv_control_parameters-no_close = space.

*    CASE lv_count.
*      WHEN 1.
*        lv_control_parameters-no_open  = space.
*        lv_control_parameters-no_close = space.
*      WHEN 2.
*        CASE lv_tabix.
*          WHEN 1.
*            lv_control_parameters-no_open  = space.
*            lv_control_parameters-no_close = 'X'.
*          WHEN 2.
*            lv_control_parameters-no_open  = 'X'.
*            lv_control_parameters-no_close = space.
*        ENDCASE.
*      WHEN OTHERS.
*        CASE lv_tabix.
*          WHEN 1.
*            lv_control_parameters-no_open  = space.
*            lv_control_parameters-no_close = 'X'.
*          WHEN lv_count.
*            lv_control_parameters-no_open  = 'X'.
*            lv_control_parameters-no_close = space.
*          WHEN OTHERS.
*            lv_control_parameters-no_open  = 'X'.
*            lv_control_parameters-no_close = 'X'.
*        ENDCASE.
*    ENDCASE.

    lv_control_parameters-no_dialog  = 'X'.
    lv_control_parameters-langu      = 'E'.

    ls_job_options-tddest            = 'LOCL'.
    l_output_options-tddest          = 'LOCL'.

*<< 預覽列印控制
    IF p_send = ''.
      lv_control_parameters-preview  = 'X'.
      lv_control_parameters-getotf   = ''.
      l_output_options-tdnewid       = ''.
    ELSE.
      lv_control_parameters-preview  = ''.
      lv_control_parameters-getotf   = ''.
      l_output_options-tdnewid       = 'X'. "Get Spool id
    ENDIF.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = gc_formname
      IMPORTING
        fm_name            = fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    CALL FUNCTION fm_name
      EXPORTING
        control_parameters   = lv_control_parameters
        output_options       = l_output_options
        user_settings        = ' '
        is_company           = gs_company_info
        is_customer          = gs_customer_info
      IMPORTING
        document_output_info = ls_document
        job_output_info      = ls_job_info
        job_output_option    = ls_job_options
      TABLES
        it_item              = lt_item
        it_total             = gt_total
        it_total_cust        = gt_total_cust
        it_aging_total       = gt_aging_total
      EXCEPTIONS
        formatting_error     = 1
        internal_error       = 2
        send_error           = 3
        user_canceled        = 4
        OTHERS               = 5.
    CASE sy-subrc.
      WHEN 0.
        IF p_send = 'X'.

          CLEAR lv_count.
          DO 2 TIMES.
            lv_count = lv_count + 1.
            CASE lv_count.
*--------------------------------------------------------------------*
* Send email with PDF
*--------------------------------------------------------------------*
              WHEN 1.
                REFRESH gt_spoolids.
                MOVE ls_job_info-spoolids TO gt_spoolids.
                IF gt_spoolids IS NOT INITIAL.
                  READ TABLE gt_spoolids INTO gs_spool INDEX 1.
                  IF sy-subrc = 0.
                    <alv>-rspoid = gs_spool.
                  ENDIF.

                  PERFORM send_mail_with_pdf.
                ENDIF.
*--------------------------------------------------------------------*
* Download PDF to local folder
*--------------------------------------------------------------------*
              WHEN 2.
                "Note: Cannot get Spool ID and OTF Data at the same time,
                "      so try to on OTF flag in second time and call SMF function again.
                lv_control_parameters-getotf   = 'X'. "Get otf
                l_output_options-tdnewid       = ''.

                CALL FUNCTION fm_name
                  EXPORTING
                    control_parameters   = lv_control_parameters
                    output_options       = l_output_options
                    user_settings        = ' '
                    is_company           = gs_company_info
                    is_customer          = gs_customer_info
                  IMPORTING
                    document_output_info = ls_document
                    job_output_info      = ls_job_info
                    job_output_option    = ls_job_options
                  TABLES
                    it_item              = lt_item
                    it_total             = gt_total
                    it_total_cust        = gt_total_cust
                    it_aging_total       = gt_aging_total
                  EXCEPTIONS
                    formatting_error     = 1
                    internal_error       = 2
                    send_error           = 3
                    user_canceled        = 4
                    OTHERS               = 5.

                PERFORM download_local USING ls_job_info-otfdata.

            ENDCASE.
          ENDDO.

        ENDIF.
      WHEN 1.
        MESSAGE e001 WITH 'FORMATTING_ERROR'.
      WHEN 2.
        MESSAGE e001 WITH 'INTERNAL_ERROR'.
      WHEN 3.
        MESSAGE e001 WITH 'SEND_ERROR'.
      WHEN 4.
        MESSAGE e001 WITH 'USER_CANCELED'.
      WHEN OTHERS.
        MESSAGE e001 WITH 'OTHERS_ERROR'.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " DO_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  APPEND_FBL5N_RSPAR_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_fbl5n_rspar_clear .
  CLEAR rspar[].
* FOR COMPANY CODE
  rspar-selname = 'KD_BUKRS'.
  rspar-kind    = 'S'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = p_bukrs.
  APPEND rspar.
  CLEAR : rspar.
  IF NOT s_kunnr[] IS INITIAL  .
* FOR Customer account
    rspar-selname = 'DD_KUNNR'.
    rspar-kind    = 'S'.
    LOOP AT s_kunnr.
      MOVE-CORRESPONDING s_kunnr TO rspar.
      APPEND rspar.
    ENDLOOP.
    CLEAR : rspar.
  ELSE.
    rspar-selname = 'DD_KUNNR'.
    rspar-kind    = 'S'.
    APPEND rspar.
    CLEAR : rspar.
  ENDIF.
* FOR DISPLAYING ALL ITEMS, DESELECT THE RADIOBUTTON FOR OPEN ITEMS / CLEARED ITEMS.
  rspar-selname = 'X_OPSEL'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = ' '.
  APPEND rspar.
  CLEAR : rspar.
  rspar-selname = 'X_CLSEL'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = 'X'.
  APPEND rspar.
  CLEAR : rspar.
* SELECT RADIOBUTTON FOR ALL ITEMS
  rspar-selname = 'X_AISEL'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = ' '.
  APPEND rspar.
  CLEAR : rspar.

* FOR Clearing date ( Begin of Report Date's month ~ Report Date )
  CONCATENATE p_budat+0(6) '01' INTO g_first_date.
  rspar-selname = 'SO_AUGDT'.
  rspar-kind    = 'S'.
  rspar-sign    = 'I'.
  rspar-option  = 'BT'.
  rspar-low     = g_first_date.
  rspar-high    = p_budat. "Report date
  APPEND rspar.
  CLEAR : rspar.

ENDFORM.                    " APPEND_FBL5N_RSPAR_CLEAR
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_FBL5N_TO_INTAB_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM submit_fbl5n_to_intab_clear .
  DATA: ls_data TYPE REF TO data,
        t_dmbtr LIKE glt0-hslvt,
        t_objct(30) TYPE c,
        t_month(2) TYPE n,
        t_date LIKE anlb-afabg.

  DATA: l_gjahr TYPE gjahr,
        l_buzei TYPE buzei.

  FIELD-SYMBOLS: <lt_data> TYPE table.

  REFRESH: gt_item,gt_col.

  cl_salv_bs_runtime_info=>set(
          display = abap_false
          metadata = abap_false
          data = abap_true ).

  SUBMIT rfitemar
         WITH SELECTION-TABLE rspar[]
         WITH FREE SELECTIONS texpr AND RETURN.

  TRY.

      cl_salv_bs_runtime_info=>get_data_ref(
              IMPORTING r_data = ls_data ).
      ASSIGN ls_data->* TO <lt_data>.

    CATCH cx_salv_bs_sc_runtime_info.
      MESSAGE `Unable to retrieve ALV data` TYPE 'E'.
  ENDTRY.

*  BREAK-POINT.

  IF sy-subrc = 0 .
    LOOP AT <lt_data> ASSIGNING <gs_1>.
      APPEND INITIAL LINE TO gt_item ASSIGNING <gfs_item>.
      MOVE-CORRESPONDING <gs_1> TO <gfs_item>.
    ENDLOOP.
  ELSE.
  ENDIF.
  cl_salv_bs_runtime_info=>clear_all( ).

  CHECK gt_item IS NOT INITIAL.

*  SORT gt_item BY bldat. "Doc date

  "Divide <lt_data> into two parts, one part is the collection information the other part is AR information
  DATA: l_augbl TYPE flag.
  LOOP AT gt_item INTO gs_item.

    IF gs_item-xzahl = '' AND ( gs_item-belnr = gs_item-rebzg ) AND ( gs_item-belnr <> gs_item-augbl ).
      "AR
      APPEND INITIAL LINE TO gt_ar ASSIGNING <ar>.
      MOVE-CORRESPONDING gs_item TO <ar>.
    ELSE.

      "Sum(WRSHB) group by AUGBL
      CLEAR gs_col.
      gs_col-bukrs = gs_item-bukrs.
      gs_col-konto = gs_item-konto.
      gs_col-augbl = gs_item-augbl.
      gs_col-wrshb = gs_item-wrshb .
      COLLECT gs_col INTO gt_col.

*      CLEAR l_augbl.
*      AT END OF augbl.
*        SUM.
*        l_augbl = 'X'.
*      ENDAT.
*      IF l_augbl = 'X'.
*        MOVE-CORRESPONDING gs_item TO gs_col.
*        COLLECT gs_col INTO gt_col.
**        APPEND gs_col TO gt_col.
*      ENDIF.

    ENDIF.
  ENDLOOP.

  "Find AR document---last period
  LOOP AT gt_ar ASSIGNING <ar>  WHERE bldat < g_first_date
                                AND   ( augdt BETWEEN g_first_date AND p_budat ).

    APPEND INITIAL LINE TO gt_report ASSIGNING <report>.
    <report>-bukrs = <ar>-bukrs.  "Company Code
    <report>-konto = <ar>-konto.  "Account Number(Customer)
    <report>-bldat = <ar>-bldat.  "Doc date
    <report>-belnr = <ar>-belnr.  "Doc No
    <report>-xblnr = <ar>-xblnr.  "Ref No
    <report>-rebzg = <ar>-rebzg.  "Inv. Ref
    <report>-augbl = <gfs_item>-augbl.  "Clrg.Doc
    <report>-faedt = <ar>-faedt.  "Due Date
    <report>-waers = <ar>-waers.  "Curr.Code
    READ TABLE gt_col INTO gs_col WITH KEY augbl = <ar>-augbl.
    IF sy-subrc = 0.
      <report>-wrshb_bf  = <ar>-wrshb - gs_col-wrshb.
    ENDIF.
  ENDLOOP.

  "Find AR document---current
  DATA: l_fwbas TYPE bset-fwbas,
        l_fwste TYPE bset-fwste.

  LOOP AT gt_ar ASSIGNING <ar> WHERE ( bldat BETWEEN g_first_date AND p_budat )
                               AND   ( augdt BETWEEN g_first_date AND p_budat ).

    APPEND INITIAL LINE TO gt_report ASSIGNING <report>.
    <report>-bukrs = <ar>-bukrs.  "Company Code
    <report>-konto = <ar>-konto.  "Account Number(Customer)
    <report>-bldat = <ar>-bldat.  "Doc date
    <report>-belnr = <ar>-belnr.  "Doc No
    <report>-xblnr = <ar>-xblnr.  "Ref No
    <report>-rebzg = <ar>-rebzg.  "Inv. Ref
    <report>-augbl = <ar>-augbl.  "Clrg.Doc
    <report>-faedt = <ar>-faedt.  "Due Date
    <report>-waers = <ar>-waers.  "Curr.Code

    PERFORM get_bset USING <ar>-bukrs
                           <ar>-belnr
                           <ar>-gjahr
                           <ar>-buzei
                     CHANGING l_fwbas
                              l_fwste.

    <report>-fwbas  = l_fwbas. "Sales
    <report>-fwste  = l_fwste. "GST


    READ TABLE gt_col INTO gs_col WITH KEY bukrs = <ar>-bukrs
                                           konto = <ar>-konto
*                                           rebzg = <ar>-rebzg.
                                           augbl = <ar>-augbl.
    IF sy-subrc = 0.
      "Collection
      <report>-wrshb_c = gs_col-wrshb.

      "(c.FWBAS + c.FWSTE) - b.Collection as Balance_CF
*      <report>-wrshb_cf  = ( l_fwbas + l_fwste ) - gs_col-wrshb.
      <report>-wrshb_cf  = ( l_fwbas + l_fwste ) + gs_col-wrshb.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " SUBMIT_FBL5N_TO_INTAB_CLEAR
*&---------------------------------------------------------------------*
*&      Form  APPEND_FBL5N_RSPAR_CURRENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_fbl5n_rspar_current .
  CLEAR rspar[].
* FOR COMPANY CODE
  rspar-selname = 'KD_BUKRS'.
  rspar-kind    = 'S'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = p_bukrs.
  APPEND rspar.
  CLEAR : rspar.
  IF NOT s_kunnr[] IS INITIAL  .
* FOR Customer account
    rspar-selname = 'DD_KUNNR'.
    rspar-kind    = 'S'.
    LOOP AT s_kunnr.
      MOVE-CORRESPONDING s_kunnr TO rspar.
      APPEND rspar.
    ENDLOOP.
    CLEAR : rspar.
  ELSE.
    rspar-selname = 'DD_KUNNR'.
    rspar-kind    = 'S'.
    APPEND rspar.
    CLEAR : rspar.
  ENDIF.
* FOR DISPLAYING ALL ITEMS, DESELECT THE RADIOBUTTON FOR OPEN ITEMS / CLEARED ITEMS.
  rspar-selname = 'X_OPSEL'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = 'X'.
  APPEND rspar.
  CLEAR : rspar.
  rspar-selname = 'X_CLSEL'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = ' '.
  APPEND rspar.
  CLEAR : rspar.
* SELECT RADIOBUTTON FOR ALL ITEMS
  rspar-selname = 'X_AISEL'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = ' '.
  APPEND rspar.
  CLEAR : rspar.

* FOR Open at key date (Current date = Report date)
  rspar-selname = 'PA_STIDA'.
  rspar-kind    = 'P'.
  rspar-sign    = 'I'.
  rspar-option  = 'EQ'.
  rspar-low     = p_budat.
  APPEND rspar.
  CLEAR : rspar.

ENDFORM.                    " APPEND_FBL5N_RSPAR_CURRENT
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_FBL5N_TO_INTAB_CURRENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM submit_fbl5n_to_intab_current .
  DATA: ls_data TYPE REF TO data,
        t_dmbtr LIKE glt0-hslvt,
        t_objct(30) TYPE c,
        t_month(2) TYPE n,
        t_date LIKE anlb-afabg.

  DATA: l_gjahr TYPE gjahr,
        l_buzei TYPE buzei.

  FIELD-SYMBOLS: <lt_data> TYPE table.

  REFRESH: gt_item,gt_col.

  cl_salv_bs_runtime_info=>set(
          display = abap_false
          metadata = abap_false
          data = abap_true ).

  SUBMIT rfitemar
         WITH SELECTION-TABLE rspar[]
         WITH FREE SELECTIONS texpr AND RETURN.

  TRY.

      cl_salv_bs_runtime_info=>get_data_ref(
              IMPORTING r_data = ls_data ).
      ASSIGN ls_data->* TO <lt_data>.

    CATCH cx_salv_bs_sc_runtime_info.
      MESSAGE `Unable to retrieve ALV data` TYPE 'E'.
  ENDTRY.

*  BREAK-POINT.

  IF sy-subrc = 0 .
    LOOP AT <lt_data> ASSIGNING <gs_1>.
      APPEND INITIAL LINE TO gt_item ASSIGNING <gfs_item>.
      MOVE-CORRESPONDING <gs_1> TO <gfs_item>.
    ENDLOOP.

    DELETE gt_item WHERE augbl <> ''.
  ELSE.
  ENDIF.
  cl_salv_bs_runtime_info=>clear_all( ).

  CHECK gt_item IS NOT INITIAL.

  "Find Collection information (partial payment) => group by BUKRS, KONTO, REBZG
  LOOP AT gt_item INTO gs_item WHERE xzahl = 'X'.
    CLEAR gs_col.
    gs_col-bukrs = gs_item-bukrs.
    gs_col-konto = gs_item-konto.
    gs_col-waers = gs_item-waers.
    gs_col-rebzg = gs_item-rebzg.
    gs_col-wrshb = gs_item-wrshb.
    COLLECT gs_col INTO gt_col.
  ENDLOOP.

  "Find AR information---last
  LOOP AT gt_item INTO gs_item WHERE xzahl = ''
                               AND   bldat < g_first_date.

    CHECK gs_item-belnr = gs_item-rebzg.

    APPEND INITIAL LINE TO gt_report ASSIGNING <report>.
    <report>-bukrs = gs_item-bukrs.  "Company Code
    <report>-konto = gs_item-konto.  "Account Number(Customer)
    <report>-bldat = gs_item-bldat.  "Doc date
    <report>-belnr = gs_item-belnr.  "Doc No
    <report>-xblnr = gs_item-xblnr.  "Ref No
    <report>-rebzg = gs_item-rebzg.  "Inv. Ref
    <report>-augbl = gs_item-augbl.  "Clrg.Doc
    <report>-faedt = gs_item-faedt.  "Due Date
    <report>-waers = gs_item-waers.  "Curr.Code

    READ TABLE gt_col INTO gs_col WITH KEY bukrs = gs_item-bukrs
                                           konto = gs_item-konto
                                           rebzg = gs_item-rebzg
                                           waers = gs_item-waers.
    IF sy-subrc = 0.
      <report>-wrshb_c = gs_col-wrshb. "Collection
      <report>-wrshb_cf = gs_item-wrshb - gs_col-wrshb. "a.WRSHB - b.Collection as Balance_CF
    ENDIF.

  ENDLOOP.

  "Find AR information---current
  LOOP AT gt_item INTO gs_item WHERE xzahl = ''
                               AND  ( bldat BETWEEN g_first_date AND p_budat ).

    CHECK gs_item-belnr = gs_item-rebzg.

    APPEND INITIAL LINE TO gt_report ASSIGNING <report>.
    <report>-bukrs = gs_item-bukrs.  "Company Code
    <report>-konto = gs_item-konto.  "Account Number(Customer)
    <report>-bldat = gs_item-bldat.  "Doc date
    <report>-belnr = gs_item-belnr.  "Doc No
    <report>-xblnr = gs_item-xblnr.  "Ref No
    <report>-rebzg = gs_item-rebzg.  "Inv. Ref
    <report>-augbl = gs_item-augbl.  "Clrg.Doc
    <report>-faedt = gs_item-faedt.  "Due Date
    <report>-waers = gs_item-waers.  "Curr.Code

    PERFORM get_bset USING gs_item-bukrs
                           gs_item-belnr
                           gs_item-gjahr
                           gs_item-buzei
                     CHANGING <report>-fwbas
                              <report>-fwste.

    READ TABLE gt_col INTO gs_col WITH KEY bukrs = gs_item-bukrs
                                           konto = gs_item-konto
                                           rebzg = gs_item-rebzg
                                           waers = gs_item-waers.
    IF sy-subrc = 0.
      <report>-wrshb_c = gs_col-wrshb. "Collection
      "(c.FWBAS + c.FWSTE) - b.Collection  as Balance CF
      <report>-wrshb_cf = <report>-fwbas + <report>-fwste - gs_col-wrshb.
    ENDIF.

    "(Report Date – a.FAEDT )-1 as Overdue_days
    PERFORM get_days USING gs_item-faedt <report>-days.

  ENDLOOP.


ENDFORM.                    " SUBMIT_FBL5N_TO_INTAB_CURRENT
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL_WITH_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail_with_pdf."USING  pv_otfdata  TYPE ssfcrescl-otfdata.

  DATA:
    l_document    TYPE REF TO cl_document_bcs,
    l_attsubj     TYPE sood-objdes,
    l_attach_size LIKE sood-objlen,
    bcs_exception TYPE REF TO cx_bcs,
    sent_to_all   TYPE os_boolean,
    mail_address  TYPE adr6-smtp_addr.

  DATA:
    l_xf TYPE x,
    l_fpath(128) TYPE c,
    i1 TYPE i,
    i_fsize TYPE i,
    wa LIKE solix,
    filebindat    TYPE solix_tab,
    s_filebindat  LIKE LINE OF filebindat,
    l_errtype     TYPE bcs_cxerr.

  DATA:
        d_binsize TYPE i,
        d_binfile TYPE xstring,
        d_subject TYPE so_obj_des.
  DATA: t_otf      TYPE TABLE OF itcoo,
        t_lines    TYPE TABLE OF tline,
        t_bintab   TYPE solix_tab,
        t_bodymail TYPE bcsy_text.

  DATA:
        l_attach    TYPE bcsy_text, " Attachment
        wa_text     TYPE soli,       "Work area for attach
        l_extension TYPE soodk-objtp VALUE 'OTF', " TXT format
        l_size      TYPE sood-objlen, " Size of Attachment
        l_lines     TYPE i.

  DATA: error_message TYPE string.

  DATA: BEGIN OF itab OCCURS 0,
          field(256),
        END   OF itab.


*--------------------------------------------------------------------*
* Begin New
*--------------------------------------------------------------------*
  DATA rq       TYPE tsp01.
  DATA bin_size TYPE i.
  DATA dummy    TYPE TABLE OF rspoattr.

* for mail
  DATA send_request  TYPE REF TO cl_bcs.
  DATA document      TYPE REF TO cl_document_bcs.
  DATA recipient     TYPE REF TO if_recipient_bcs.
  DATA pdf_size      TYPE so_obj_len.
  DATA pdf_content   TYPE solix_tab.
  DATA pdf_xstring   TYPE xstring.

*  LOOP AT gt_spoolids INTO gs_spool.
*&---------------------------------------------------------------------*
*& Create PDF
*&---------------------------------------------------------------------*
* Create PDF Content
* 1) get attributes of spool request
* 2) convert spool request to PDF dependent on document type
*----------------------------------------------------------------------*
*   ------------ get attributes of spool request ---------------------
  CALL FUNCTION 'RSPO_GET_ATTRIBUTES_SPOOLJOB'
    EXPORTING
      rqident     = gs_spool
    IMPORTING
      rq          = rq
    TABLES
      attributes  = dummy
    EXCEPTIONS
      no_such_job = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
*   error handling
*    CONTINUE.
  ENDIF.

*   --- convert spool request into PDF, dependent on document type ---
  IF rq-rqdoctype = 'OTF' OR rq-rqdoctype = 'SMART'.
    CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
      EXPORTING
        src_spoolid              = gs_spool
        no_dialog                = 'X'
        pdf_destination          = 'X'
        no_background            = 'X'
      IMPORTING
        pdf_bytecount            = bin_size
        bin_file                 = pdf_xstring
      EXCEPTIONS
        err_no_otf_spooljob      = 1
        err_no_spooljob          = 2
        err_no_permission        = 3
        err_conv_not_possible    = 4
        err_bad_dstdevice        = 5
        user_cancelled           = 6
        err_spoolerror           = 7
        err_temseerror           = 8
        err_btcjob_open_failed   = 9
        err_btcjob_submit_failed = 10
        err_btcjob_close_failed  = 11
        OTHERS                   = 12.
    IF sy-subrc <> 0.
*   error handling
*      CONTINUE.
    ENDIF.
  ELSEIF rq-rqdoctype = 'LIST'.
    CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
      EXPORTING
        src_spoolid              = gs_spool
        no_dialog                = 'X'
        pdf_destination          = 'X'
        no_background            = 'X'
      IMPORTING
        pdf_bytecount            = bin_size
        bin_file                 = pdf_xstring
      EXCEPTIONS
        err_no_abap_spooljob     = 1
        err_no_spooljob          = 2
        err_no_permission        = 3
        err_conv_not_possible    = 4
        err_bad_destdevice       = 5
        user_cancelled           = 6
        err_spoolerror           = 7
        err_temseerror           = 8
        err_btcjob_open_failed   = 9
        err_btcjob_submit_failed = 10
        err_btcjob_close_failed  = 11
        OTHERS                   = 12.
    IF sy-subrc <> 0.
*   error handling
*      CONTINUE.
    ENDIF.
  ELSE.
*   error handling
*    CONTINUE.
  ENDIF.
  pdf_size = bin_size.

*&---------------------------------------------------------------------*
*& Send mail
*&---------------------------------------------------------------------*

  TRY.

*     -------- create persistent send request ------------------------
      send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document -------------------------------
      pdf_content = cl_document_bcs=>xstring_to_solix( pdf_xstring ).

      document = cl_document_bcs=>create_document(
            i_type    = 'PDF'
            i_text    = w_body
            i_hex     = pdf_content
            i_length  = pdf_size
            i_subject = 'AR Statement' ).                   "#EC NOTEXT

*     add document object to send request
      send_request->set_document( document ).

* SENDER ADDESS
      g_sender = cl_sapuser_bcs=>create( sy-uname ).
      send_request->set_sender( g_sender ).

* RECIPIENT ADDRESS
      LOOP AT gt_customer_email INTO gs_customer_email WHERE kunnr = gs_customer_info-kunnr.

        mail_address = gs_customer_email-smtp_addr.
        g_recipient = cl_cam_address_bcs=>create_internet_address( mail_address ).

* ADD RECIPIENT ADDRESS TO SEND REQUEST
        CALL METHOD send_request->add_recipient
          EXPORTING
            i_recipient  = g_recipient
            i_express    = 'X'
            i_copy       = ' '
            i_blind_copy = ' '
            i_no_forward = ' '.
      ENDLOOP.

      IF sy-subrc <> 0.

        <alv>-mail = space.
        <alv>-status = text-002. "Printing/to be Printed

      ELSE.
        send_request->send( ).
        COMMIT WORK.
        MESSAGE 'send mail successfully' TYPE 'S'.

        <alv>-mail = 'Y'.
        <alv>-status = text-001. "Document sent to queue for E-Mail/Fax
      ENDIF.

*   ------------ exception handling ----------------------------------
*   replace this rudimentary exception handling with your own one !!!
    CATCH cx_bcs INTO bcs_exception.

  ENDTRY.

*  ENDLOOP.
*--------------------------------------------------------------------*
* END New
*--------------------------------------------------------------------*


*  "Check mail address
*  CLEAR error_message.
*  LOOP AT gt_customer_email INTO gs_customer_email WHERE kunnr = gs_customer_info-kunnr.
*
*    CLEAR error_message.
*    IF gs_customer_email-smtp_addr IS INITIAL.
*      CONCATENATE 'Customer :' gs_customer_info-kunnr 'have no mail address'INTO error_message.
*      MESSAGE error_message TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
*  ENDLOOP.
*
*  IF error_message IS INITIAL.
*    CLEAR: g_send_request.
*    CLEAR: w_subject, w_body, w_body[].
*    CLEAR: w_to, w_to[].
*
*    w_subject = 'AR Statement'.
*
*    TRY .
*
*        g_send_request = cl_bcs=>create_persistent( ).
*        l_document = cl_document_bcs=>create_document(
*                     i_type    = 'HTM'   "RAW
*                     i_text    = w_body
*                     i_subject = w_subject ).
*
** ADD ATTACHMENT
*
**convert otf to pdf file
*        t_otf = pv_otfdata.
*        CALL FUNCTION 'CONVERT_OTF'
*          EXPORTING
*            format       = 'PDF'
*          IMPORTING
*            bin_filesize = d_binsize
*            bin_file     = d_binfile
*          TABLES
*            otf          = t_otf
*            lines        = t_lines.
*
**convert xstring to binary
*        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*          EXPORTING
*            buffer     = d_binfile
*          TABLES
*            binary_tab = t_bintab.
*
*        l_attsubj = g_filename.
*
*        CALL METHOD l_document->add_attachment
*          EXPORTING
*            i_attachment_type    = 'PDF'
*            i_attachment_subject = l_attsubj
*            i_att_content_hex    = t_bintab.
*
*        g_send_request->set_document( l_document ).
*
*
** SENDER ADDESS
*        g_sender = cl_sapuser_bcs=>create( sy-uname ).
*        g_send_request->set_sender( g_sender ).
*
** RECIPIENT ADDRESS
*        LOOP AT gt_customer_email INTO gs_customer_email WHERE kunnr = gs_customer_info-kunnr.
*
*          mail_address = gs_customer_email-smtp_addr.
*          g_recipient = cl_cam_address_bcs=>create_internet_address( mail_address ).
*
** ADD RECIPIENT ADDRESS TO SEND REQUEST
*          CALL METHOD g_send_request->add_recipient
*            EXPORTING
*              i_recipient  = g_recipient
*              i_express    = 'X'
*              i_copy       = ' '
*              i_blind_copy = ' '
*              i_no_forward = ' '.
*        ENDLOOP.
*
*        g_send_request->send( ).
*
*      CATCH cx_bcs INTO bcs_exception.
*        l_errtype = bcs_exception->error_type.
*    ENDTRY.
*    COMMIT WORK.
*
*  ENDIF.



*<< TRY 1 : Display OTF
*        l_attach[] = pv_otfdata.
*        l_lines = LINES( l_attach ).
*        l_size = l_lines * 255.
*        l_attsubj = g_filename.
*
*        CALL METHOD l_document->add_attachment
*          EXPORTING
*            i_attachment_type    = l_extension "'PDF'
*            i_attachment_subject = l_attsubj
*            i_attachment_size    = l_size
*            i_att_content_text   = l_attach.
*
*        g_send_request->set_document( l_document ).

*<< TRY 2 : Get BINARY from AP SERVER
*        g_server_mode = 'X'.
*
*        CALL FUNCTION 'Z_FSD_DOWNLOAD_PDF'
*          EXPORTING
*            i_server_mode          = g_server_mode
*            i_filename             = g_filename
*          TABLES
*            t_otf                  = pv_otfdata
*          EXCEPTIONS
*            err_otf_data_not_exist = 1
*            err_convert_otf        = 2
*            err_file_path          = 3
*            OTHERS                 = 4.
*
*        CONCATENATE '.\' g_filename INTO l_fpath.
*        OPEN DATASET l_fpath FOR INPUT IN BINARY MODE.
*        IF sy-subrc <> 0.
*          "Error handle
*          EXIT.
*        ENDIF.
*
*        READ DATASET l_fpath INTO big_string.
*        IF sy-subrc <> 0.
*          CLEAR big_string.
*        ENDIF.
*
*        CLOSE DATASET l_fpath.
*
*        IF big_string IS NOT INITIAL.
*          CALL METHOD cl_bcs_convert=>xstring_to_solix
*            EXPORTING
*              iv_xstring = big_string
*            RECEIVING
*              et_solix   = it_solix.
*        ENDIF.
*
*        filebindat[] = it_solix[].
*        l_attach_size = XSTRLEN( big_string ).
*
**        DO.
**          IF i1 = 255.
**            APPEND wa TO filebindat.
**            CLEAR: wa, i1.
**          ENDIF.
**          READ DATASET l_fpath INTO l_xf.
**          IF NOT sy-subrc IS INITIAL.
**            EXIT.
**          ENDIF.
**
**          wa-line+i1(1) = l_xf.
**          i_fsize = i_fsize + 1.
**          i1 = i1 + 1.
**        ENDDO.
**        IF NOT wa IS INITIAL.
**          APPEND wa TO filebindat.
**        ENDIF.
**        CLOSE DATASET l_fpath.
**        l_attach_size = i_fsize.
*
*        l_attsubj = g_filename.
*
*        CALL METHOD l_document->add_attachment
*          EXPORTING
*            i_attachment_type    = 'BIN'
*            i_attachment_subject = l_attsubj
*            i_att_content_hex    = filebindat
*            i_attachment_size    = l_attach_size.
*
*        g_send_request->set_document( l_document ).

*<< TRY 3 : Get PDF from local file
*        DATA : filename TYPE  string.
*        filename = g_filename_local.
*        CALL FUNCTION 'GUI_UPLOAD'
*          EXPORTING
*            filename = filename
*            filetype = 'BIN'
*          TABLES
*            data_tab = itab.
*
*        LOOP AT itab.
*          s_filebindat-line = itab-field.
*          APPEND s_filebindat TO filebindat[].
*        ENDLOOP.
*
*        l_attsubj = g_filename.
*        l_lines = LINES( filebindat ).
*        l_size = l_lines * 255.
*
*        CALL METHOD l_document->add_attachment
*          EXPORTING
*            i_attachment_type    = 'BIN'
*            i_attachment_subject = l_attsubj
*            i_att_content_hex    = filebindat
*            i_attachment_size    = l_size.
*
*        g_send_request->set_document( l_document ).

ENDFORM.                    " SEND_MAIL_WITH_PDF
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_JOB_INFO_OTFDATA  text
*----------------------------------------------------------------------*
FORM download_local  USING  pv_otfdata  TYPE ssfcrescl-otfdata.

  DATA: l_bin_filesize TYPE i.
  DATA: t_otf TYPE TABLE OF itcoo WITH HEADER LINE,
        lt_lines LIKE tline OCCURS 0 WITH HEADER LINE.
  DATA: gui_path1 TYPE string.
  DATA: pdf LIKE tline OCCURS 100 WITH HEADER LINE.
  DATA: l_director  TYPE string,
        l_filename  LIKE rlgrap-filename.

  DATA: l_timestamp	TYPE char15.

  "Get Timestamp
  CALL FUNCTION 'Z_FBC0007'
    IMPORTING
      ret_timestamp = l_timestamp.

  CONCATENATE gs_customer_info-kunnr '_' l_timestamp '_AR Statement.pdf' INTO g_filename. "0110000592_20220413183328_AR Statement.pdf
  CONCATENATE 'C:\Users\'sy-uname'\APData\' INTO l_director.
  CONCATENATE l_director g_filename INTO l_filename.

  "Set initial
  g_server_mode = ''.

  IF g_server_mode = ''.

*    CALL FUNCTION 'Z_FSD_DOWNLOAD_PDF'
*      EXPORTING
*        i_server_mode          = g_server_mode
*        i_filename             = l_filename
*      TABLES
*        t_otf                  = pv_otfdata
*      EXCEPTIONS
*        err_otf_data_not_exist = 1
*        err_convert_otf        = 2
*        err_file_path          = 3
*        OTHERS                 = 4.

* Convert otf data
    REFRESH lt_lines[].
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = l_bin_filesize
      TABLES
        otf                   = pv_otfdata
        lines                 = lt_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        OTHERS                = 4.
    IF sy-subrc NE 0.
      MESSAGE 'Download PDF to local folder fail' TYPE 'E'.
    ENDIF.

    gui_path1 = l_filename.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = l_bin_filesize
        filename                = gui_path1
        filetype                = 'BIN'
        no_auth_check           = 'X'
      TABLES
        data_tab                = lt_lines
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
  ENDIF.
*--------------------------------------------------------------------*
* For fucture request
*--------------------------------------------------------------------*
  IF g_server_mode = 'X'.

    l_director = '\\nuvoton.com\apfile\assy\'.
    CONCATENATE l_director g_filename INTO l_filename.

    CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
      EXPORTING
        src_spoolid              = gs_spool
        no_dialog                = 'X'
        pdf_destination          = ' '
        no_background            = ' '
      TABLES
        pdf                      = pdf
      EXCEPTIONS
        err_no_otf_spooljob      = 1
        err_no_spooljob          = 2
        err_no_permission        = 3
        err_conv_not_possible    = 4
        err_bad_dstdevice        = 5
        user_cancelled           = 6
        err_spoolerror           = 7
        err_temseerror           = 8
        err_btcjob_open_failed   = 9
        err_btcjob_submit_failed = 10
        err_btcjob_close_failed  = 11
        OTHERS                   = 12.
    IF sy-subrc <> 0.
      MESSAGE 'Upload PDF to Server folder fail' TYPE 'E'.
    ELSE.

      IF pdf[] IS NOT INITIAL.
        OPEN DATASET l_filename FOR OUTPUT IN BINARY MODE.
        IF sy-subrc = 0.
          LOOP AT pdf.
            TRANSFER pdf-tdformat TO l_filename.
            TRANSFER pdf-tdline TO l_filename.
          ENDLOOP.
        ENDIF.
        CLOSE DATASET l_filename.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " DOWNLOAD_LOCAL
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_alv .

  CHECK p_send = 'X'.

  IF gt_alv IS INITIAL.
    WRITE:/ 'No Result data.'.
    EXIT.
  ENDIF.

  PERFORM create_dynamic_table.
  PERFORM assign_data.
  PERFORM display_alv.

ENDFORM.                    " SHOW_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_dynamic_table .

  DATA: l_fieldname1(30) TYPE c,
        l_fieldname2(30) TYPE c.
  DATA: l_check(1)       TYPE c.

  ls_fieldcat-fieldname = 'KUNNR'.
  ls_fieldcat-ref_tabname = 'KNA1'.
  ls_fieldcat-seltext_l  = 'Cust.Code'.
  APPEND ls_fieldcat TO lt_fieldcat.  "ALV
  CLEAR : ls_fieldcat.

  ls_fieldcat-fieldname = 'NAME1'.
  ls_fieldcat-ref_tabname = 'ADRC'.
  ls_fieldcat-seltext_l  = 'Customer Name'.
  APPEND ls_fieldcat TO lt_fieldcat.  "ALV
  CLEAR : ls_fieldcat.

  ls_fieldcat-fieldname = 'RSPOID'.
  ls_fieldcat-ref_tabname = 'TSP01'.
  ls_fieldcat-seltext_l  = 'Spool Request#'.
  APPEND ls_fieldcat TO lt_fieldcat.  "ALV
  CLEAR : ls_fieldcat.

  ls_fieldcat-fieldname = 'MAIL'.
  ls_fieldcat-ref_tabname = ' '.
  ls_fieldcat-seltext_l  = 'Mail/Fax'.
  APPEND ls_fieldcat TO lt_fieldcat.  "ALV
  CLEAR : ls_fieldcat.

  ls_fieldcat-fieldname = 'STATUS'.
  ls_fieldcat-ref_tabname = ' '.
  ls_fieldcat-seltext_l  = 'Status'.
  APPEND ls_fieldcat TO lt_fieldcat.  "ALV
  CLEAR : ls_fieldcat.


  is_lvc_cat-fieldname = 'KUNNR'.
  is_lvc_cat-ref_field = 'KUNNR'.
  is_lvc_cat-ref_table = 'KNA1'.
  is_lvc_cat-scrtext_s  = is_lvc_cat-scrtext_m .
  is_lvc_cat-scrtext_l  = 'Cust.Code'.
  APPEND is_lvc_cat TO it_lvc_cat.

  is_lvc_cat-fieldname = 'NAME1'.
  is_lvc_cat-ref_field = 'NAME1'.
  is_lvc_cat-ref_table = 'ADRC'.
  is_lvc_cat-scrtext_s  = is_lvc_cat-scrtext_m .
  is_lvc_cat-scrtext_l  = 'Customer Number'.
  APPEND is_lvc_cat TO it_lvc_cat.

  is_lvc_cat-fieldname = 'RSPOID'.
  is_lvc_cat-ref_field = ''.
  is_lvc_cat-ref_table = ''.
  is_lvc_cat-scrtext_s = ''.
  is_lvc_cat-scrtext_l = 'Spool Request#'.
  APPEND is_lvc_cat TO it_lvc_cat.

  is_lvc_cat-fieldname = 'MAIL'.
  is_lvc_cat-ref_field = ' '.
  is_lvc_cat-ref_table = ' '.
  is_lvc_cat-scrtext_s  = ' '.
  is_lvc_cat-scrtext_l  = 'MAIL'.
  APPEND is_lvc_cat TO it_lvc_cat.

  is_lvc_cat-fieldname = 'STATUS'.
  is_lvc_cat-ref_field = 'MESSAGE'.
  is_lvc_cat-ref_table = 'ZTSAPUSRROLEINIT'.
  is_lvc_cat-scrtext_s  = is_lvc_cat-scrtext_m .
  is_lvc_cat-scrtext_l  = 'STATUS'.
  APPEND is_lvc_cat TO it_lvc_cat.

  LOOP AT gt_waers INTO gs_waers.

    CONCATENATE 'DOC_CURR_' gs_waers-waers INTO l_fieldname1.
    CONCATENATE 'Doc.Curr(' gs_waers-waers ')' INTO l_fieldname2.

    is_lvc_cat-fieldname   = l_fieldname1.
    is_lvc_cat-ref_field   = 'WRSHB'.
    is_lvc_cat-ref_table   = 'RFPOSXEXT'.
    is_lvc_cat-scrtext_s   = l_fieldname2.
    is_lvc_cat-scrtext_m   = l_fieldname2.
    is_lvc_cat-scrtext_l   = l_fieldname2.
    ls_fieldcat-datatype   = 'CURR'.
    APPEND is_lvc_cat TO it_lvc_cat.

    ls_fieldcat-fieldname     = l_fieldname1.
    ls_fieldcat-ref_fieldname = 'WRSHB'.
    ls_fieldcat-ref_tabname   = 'RFPOSXEXT'.
    ls_fieldcat-seltext_s     = l_fieldname2.
    ls_fieldcat-seltext_m     = l_fieldname2.
    ls_fieldcat-seltext_l     = l_fieldname2.
    ls_fieldcat-datatype      = 'DEC'.
    ls_fieldcat-just          = 'R'.
    ls_fieldcat-ddictxt       = 'L'.
    ls_fieldcat-do_sum        = 'X'.

    PERFORM no_decimals USING gs_waers-waers
                        CHANGING l_check.
    IF l_check = 'X'.
      ls_fieldcat-decimals_out = '0'.
    ELSE.
      ls_fieldcat-decimals_out = ' '.
    ENDIF.

    APPEND ls_fieldcat TO lt_fieldcat. "ALV
    CLEAR : ls_fieldcat.

  ENDLOOP.

  LOOP AT gt_waers_local INTO gs_waers_local.

    CONCATENATE 'LOCAL_CURR_' gs_waers_local-waers INTO l_fieldname1.
    CONCATENATE 'Local Currency(' gs_waers_local-waers ')' INTO l_fieldname2.

    is_lvc_cat-fieldname   = l_fieldname1.
    is_lvc_cat-ref_field   = 'DMSHB'.
    is_lvc_cat-ref_table   = 'RFPOSXEXT'.
    is_lvc_cat-scrtext_s   = l_fieldname2.
    is_lvc_cat-scrtext_m   = l_fieldname2.
    is_lvc_cat-scrtext_l   = l_fieldname2.
    ls_fieldcat-datatype   = 'CURR'.
    APPEND is_lvc_cat TO it_lvc_cat.

    ls_fieldcat-fieldname     = l_fieldname1.
    ls_fieldcat-ref_fieldname = 'DMSHB'.
    ls_fieldcat-ref_tabname   = 'RFPOSXEXT'.
    ls_fieldcat-seltext_s     = l_fieldname2.
    ls_fieldcat-seltext_m     = l_fieldname2.
    ls_fieldcat-seltext_l     = l_fieldname2.
    ls_fieldcat-datatype      = 'DEC'.
    ls_fieldcat-just          = 'R'.
    ls_fieldcat-ddictxt       = 'L'.
    ls_fieldcat-do_sum        = 'X'.

    PERFORM no_decimals USING gs_waers_local-waers
                        CHANGING l_check.
    IF l_check = 'X'.
      ls_fieldcat-decimals_out = '0'.
    ELSE.
      ls_fieldcat-decimals_out = ' '.
    ENDIF.

    APPEND ls_fieldcat TO lt_fieldcat. "ALV
    CLEAR : ls_fieldcat.

  ENDLOOP.


  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_lvc_cat
    IMPORTING
      ep_table        = new_table.


ENDFORM.                    " CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_data .

  DATA: l_fieldname1(30) TYPE c.

* Create a new Line with the same structure of the table.
  ASSIGN new_table->* TO <l_table>.
  CREATE DATA new_line LIKE LINE OF <l_table>.
  ASSIGN new_line->* TO <l_line>.

  LOOP AT gt_alv INTO gs_alv.
    ASSIGN COMPONENT 'KUNNR' OF STRUCTURE <l_line> TO <l_field>.
    IF sy-subrc = 0.
      <l_field> = gs_alv-kunnr.
    ENDIF.
    ASSIGN COMPONENT 'NAME1' OF STRUCTURE <l_line> TO <l_field>.
    IF sy-subrc = 0.
      <l_field> = gs_alv-name1.
    ENDIF.
    ASSIGN COMPONENT 'RSPOID' OF STRUCTURE <l_line> TO <l_field>.
    IF sy-subrc = 0.
      <l_field> = gs_alv-rspoid.
    ENDIF.
    ASSIGN COMPONENT 'MAIL' OF STRUCTURE <l_line> TO <l_field>.
    IF sy-subrc = 0.
      <l_field> = gs_alv-mail.
    ENDIF.
    ASSIGN COMPONENT 'STATUS' OF STRUCTURE <l_line> TO <l_field>.
    IF sy-subrc = 0.
      <l_field> = gs_alv-status.
    ENDIF.

    LOOP AT gt_doc_curr INTO gs_doc_curr WHERE kunnr = gs_alv-kunnr.
      CONCATENATE 'DOC_CURR_' gs_doc_curr-waers INTO l_fieldname1.
      ASSIGN COMPONENT l_fieldname1 OF STRUCTURE <l_line> TO <l_field>.
      IF sy-subrc = 0.
        <l_field>  = gs_doc_curr-wrshb_cf.
*        PERFORM no_decimals USING gs_doc_curr-waers
*                                  gs_doc_curr-wrshb_cf
*                            CHANGING <l_field>.
*        WRITE gs_doc_curr-wrshb_cf TO <l_field> LEFT-JUSTIFIED.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_local_curr INTO gs_local_curr WHERE kunnr = gs_alv-kunnr.
      CONCATENATE 'LOCAL_CURR_' gs_local_curr-hwaer INTO l_fieldname1.
      ASSIGN COMPONENT l_fieldname1 OF STRUCTURE <l_line> TO <l_field>.
      IF sy-subrc = 0.
        <l_field> = gs_local_curr-dmbtr_l.
*        PERFORM no_decimals USING gs_local_curr-hwaer
*                                  gs_local_curr-dmbtr_l
*                            CHANGING <l_field>.
*        WRITE gs_local_curr-dmbtr_l  TO <l_field> LEFT-JUSTIFIED.
      ENDIF.
    ENDLOOP.

    INSERT <l_line> INTO TABLE <l_table>.
    CLEAR : <l_line> ,<l_field>.
  ENDLOOP.

ENDFORM.                    " ASSIGN_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
  PERFORM events_init USING gt_events[].
  PERFORM layout_init USING l_layout.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_events          = gt_events
      is_layout          = is_layout
      it_fieldcat        = lt_fieldcat
      i_bypassing_buffer = 'X'
      i_callback_program = sy-repid
      i_save             = 'A'
    TABLES
      t_outtab           = <l_table>. "internal table
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DISPLAY_ALV
*---------------------------------------------------------------------*
*       FORM events_init                                              *
*---------------------------------------------------------------------*
FORM events_init USING rt_events TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = rt_events.

  READ TABLE rt_events WITH KEY name = slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'TOP_OF_PAGE' TO ls_event-form.
    MODIFY rt_events FROM ls_event INDEX sy-tabix.
  ENDIF.

  READ TABLE rt_events WITH KEY name = slis_ev_user_command
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'USER_COMMAND' TO ls_event-form.
    MODIFY rt_events FROM ls_event INDEX sy-tabix.
  ENDIF.

ENDFORM.                  " EVENTS_INIT
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_INIT
*&---------------------------------------------------------------------*
FORM layout_init USING p_l_layout TYPE slis_layout_alv.
  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.
  is_layout-window_titlebar = sy-title.
ENDFORM.                    " LAYOUT_INIT
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.
  DATA: t_header TYPE slis_t_listheader,
        wa_header TYPE slis_listheader,
        t_line LIKE wa_header-info,
        ld_lines TYPE i,
        ld_linesc(10) TYPE c.
* Title
  wa_header-typ  = 'H'.
  wa_header-info  = gs_company_info-name1.
  APPEND wa_header TO t_header.
  CLEAR wa_header.

  wa_header-typ  = 'H'.
  CONCATENATE 'Status of sending A.R.Statement as at'
              p_budat
              INTO wa_header-info SEPARATED BY space.
  APPEND wa_header TO t_header.
  CLEAR wa_header.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.
*            i_logo             = 'Z_LOGO'.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  NO_DECIMALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_DOC_CURR_WAERS  text
*      <--P_<L_FIELD>  text
*----------------------------------------------------------------------*
FORM no_decimals  USING    p_waers
                  CHANGING p_check.

*<< set default
  p_check = ''.

*<< get Decimal Places in Currencies
  IF gt_tcurx IS INITIAL.
    SELECT * INTO TABLE gt_tcurx FROM tcurx.            "#EC CI_NOWHERE
  ENDIF.

  READ TABLE gt_tcurx
  TRANSPORTING NO FIELDS WITH KEY currkey = p_waers
                                  currdec = 0.
  IF sy-subrc = 0.
    p_check = 'X'.
  ENDIF.

ENDFORM.                    " NO_DECIMALS
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_DUEDATE_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM determine_duedate_sort .

  "initial
  CLEAR: g_date1_f,
         g_date1_t,
         g_date2_f,
         g_date2_t,
         g_date3_f,
         g_date3_t,
         g_date4_f,
         g_date4_t,
         g_date5_f.

  g_date1_f = rastbis1 + 1.
  g_date1_t = rastbis2.

  g_date2_f = rastbis2 + 1.
  g_date2_t = rastbis3.

  g_date3_f = rastbis3 + 1.
  g_date3_t = rastbis4.

  g_date4_f = rastbis4 + 1.
  g_date4_t = rastbis5.

  g_date5_f = rastbis5.

  IF rastbis2 < rastbis1.
    CLEAR : g_date1_f,
            g_date1_t,
            g_date2_f,
            g_date2_t,
            g_date3_f,
            g_date3_t,
            g_date4_f,
            g_date4_t,
            g_date5_f.
    EXIT.
  ENDIF.

  IF rastbis3 < rastbis2.
    CLEAR : g_date2_f,
            g_date2_t,
            g_date3_f,
            g_date3_t,
            g_date4_f,
            g_date4_t,
            g_date5_f.
    EXIT.
  ENDIF.

  IF rastbis4 < rastbis3.
    CLEAR : g_date3_f,
            g_date3_t,
            g_date4_f,
            g_date4_t,
            g_date5_f.
    EXIT.
  ENDIF.

  IF rastbis5 < rastbis4.
    CLEAR : g_date4_f,
            g_date4_t,
            g_date5_f.
    EXIT.
  ENDIF.

ENDFORM.                    " DETERMINE_DUEDATE_SORT

*----------------------------------------------------------------------*
* INCLUDE ZFUNCTION
* 上傳檔案(ASC檔)
* PERFORM UPLOAD_ASC         USING U_FILE
* 下載檔案(ASC檔)
* PERFORM DOWNLOAD_ASC      USING U_FILE
* 上傳檔案(DAT檔)
* PERFORM UPLOAD_DAT        TABLES U_DATA STRUCTURE Z_ALCMF1
*                           USING U_FILE
* 批次檔BDCDATA FOR FIELDS
* PERFORM BDC_DYNPRO        USING PROGRAM DYNPRO
* 批次檔BDCDATA FOR SCREEN
* PERFORM BDC_FIELD         USING  FNAM FVAL
* 執行交易(含訊息)
* PERFORM CALL_TRANS        USING U_TX   U_MODE U_UPDATE U_TEXTU_FLAG
* 日期格式轉換 MMDDYYYY -> YYYYMMDD
* PERFORM CHANGE_DATE       USING U_DATE T_DATE
* 幣別轉換之係數&匯率
* PERFORM read_exchange_rate     USING u_date  U_WAERS u_localu_type
*                                 U_kursf U_FACTOR1 U_FACTOR2
* 截取上個月最後一天
* PERFORM CHANGE_LAST_DATE  USING U_DATE U_DAT1
* 迴轉傳票之批次檔準備
* PERFORM REVERSE_DOC USING U_BELNR U_BUKRS U_GJAHR U_STGRD U_DATUM
* 確認供應商資料
* PERFORM CHECK_LFA1 USING U_LIFNR U_FLAG U_TEXT
* 確認客戶資料
* PERFORM CHECK_KNA1 USING U_KUNNR U_FLAG U_TEXT
* 報表表頭
* PERFORM PAGE_HEADER USING U_TITLE
* 報表表頭1(公司變數)
* PERFORM PAGE_HEADER1 USING U_BUKRS U_TITLE
* 展CO群組
* PERFORM GET_HIERARCHY_DATA  TABLES   f_nodes  f_values
*                             USING    f_class  f_setid   f_kokrs.
* 身分證統一編號檢查程式
* PERFORM CHECK_INPUT_ID USING P_ID       " P_ID(10) TYPE C
*                              P_MESSAGE. " P_MESSAGE(39)
* 營利事業編號檢查
* PERFORM CHECK_UNICODE USING P_UNICOD   " P_UNICOD(8) TYPE C
*                             P_MESSAGE. " P_MESSAGE(39)
*檢查日期格式YYYYMMDD
* PERFORM CHECK_DATE_CONVERT USING F_DATE1   "輸入日期(10)
*                                  F_DATE2   "輸出日期(10)
*                                  F_FALG    "錯誤FLAG  E
*                                  F_TEXT .  "錯誤訊息
*插入前置0
*PERFORM INPUT_ZERO USING  U_ALPHA .   "輸入字串
*刪除前置0
*PERFORM OUTPUT_ZERO USING  U_ALPHA .   "輸入字串
*將text1中有text2(如:")符號者取代為空白 ex '123"4"5' 取代為 '123 45'
*PERFORM Overlay_space USING  text1 text2 .
*06/09/2006 added download_file_gui for sap upgrade
*----------------------------------------------------------------------*
************************************************************************
* DATA DECLARE
************************************************************************
*  UPLOAD iday
DATA : BEGIN OF IDATA OCCURS 0 ,
         TEXT(600) ,
       END OF IDATA .
*  Batch Input data of single transaction
DATA : BEGIN OF BDCDATA OCCURS 0 .
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDCDATA.
* Structure for message of batch input process
DATA : BEGIN OF MESSTAB OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESSTAB.
DATA : G_FTEXT(200) .
*Hierarchy internal tables for co profit center
DATA : F_INFO LIKE GRPHINFO OCCURS 0 WITH HEADER LINE.
* DATA
DATA : F_LIFNR LIKE LFA1-LIFNR ,
       F_ZERO(10) VALUE '0000000000' ,
       F_KUNNR LIKE KNA1-KUNNR ,
       F_OVER LIKE SY-DATAR ,
       F_SETID LIKE SETHIER-SETID,
       P_MESSAGE(255),                 " PUSH ERROR MESSAGE
       F_FLAG(1) .
DATA: BEGIN OF IHEAD OCCURS 0,
            LINE(1000)  TYPE c    ,
      END OF IHEAD.

************************************************************************
* COMMON FORM
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_ASC FOR TEXT FILE 'ASC'
*&---------------------------------------------------------------------*
FORM UPLOAD_ASC USING U_FILE.
  DATA: L_OUTPUT_NAME type string.
  L_OUTPUT_NAME = U_FILE.
*  CALL FUNCTION 'WS_UPLOAD'
*      EXPORTING
**           CODEPAGE                = 'TIBM'
*           FILENAME                = U_FILE
*           FILETYPE                = 'ASC'
*       TABLES
*            DATA_TAB                = IDATA
*    EXCEPTIONS
*         CONVERSION_ERROR        = 1
*         INVALID_TABLE_WIDTH     = 2
*         INVALID_TYPE            = 3
*         NO_BATCH                = 4
*         UNKNOWN_ERROR           = 5
*         GUI_REFUSE_FILETRANSFER = 6
*         OTHERS                  = 7.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    FILENAME                      = L_OUTPUT_NAME
    FILETYPE                      = 'ASC'
*   HAS_FIELD_SEPARATOR           = ' '
*   HEADER_LENGTH                 = 0
*   READ_BY_LINE                  = 'X'
*   DAT_MODE                      = ' '
*   CODEPAGE                      = ' '
*   IGNORE_CERR                   = ABAP_TRUE
*   REPLACEMENT                   = '#'
*   CHECK_BOM                     = ' '
*   VIRUS_SCAN_PROFILE            =
*   NO_AUTH_CHECK                 = ' '
* IMPORTING
*   FILELENGTH                    =
*   HEADER                        =
  TABLES
    DATA_TAB                      = IDATA
 EXCEPTIONS
   FILE_OPEN_ERROR               = 1
   FILE_READ_ERROR               = 2
   NO_BATCH                      = 3
   GUI_REFUSE_FILETRANSFER       = 4
   INVALID_TYPE                  = 5
   NO_AUTHORITY                  = 6
   UNKNOWN_ERROR                 = 7
   BAD_DATA_FORMAT               = 8
   HEADER_NOT_ALLOWED            = 9
   SEPARATOR_NOT_ALLOWED         = 10
   HEADER_TOO_LONG               = 11
   UNKNOWN_DP_ERROR              = 12
   ACCESS_DENIED                 = 13
   DP_OUT_OF_MEMORY              = 14
   DISK_FULL                     = 15
   DP_TIMEOUT                    = 16
   OTHERS                        = 17
          .
IF SY-SUBRC <> 0.
   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDFORM.                               " UPLOAD_ASC
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_ASC FOR TEXT FILE 'ASC'
*&---------------------------------------------------------------------*
FORM DOWNLOAD_ASC USING U_FILE  .
  DATA: L_OUTPUT_NAME type string.
  L_OUTPUT_NAME = U_FILE.
*  CALL FUNCTION 'WS_DOWNLOAD'
*      EXPORTING
**             CODEPAGE                = ' '
*           FILENAME                = U_FILE
*           FILETYPE                = 'ASC'
*       TABLES
*            DATA_TAB                = IDATA
*      EXCEPTIONS
*           FILE_OPEN_ERROR         = 1
*           FILE_WRITE_ERROR        = 2
*           INVALID_FILESIZE        = 3
*           INVALID_TYPE            = 4
*           NO_BATCH                = 5
*           UNKNOWN_ERROR           = 6
*           INVALID_TABLE_WIDTH     = 7
*           GUI_REFUSE_FILETRANSFER = 8
*           CUSTOMER_ERROR          = 9
*           OTHERS                  = 10
*            .
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE                    =
      FILENAME                        = L_OUTPUT_NAME
      FILETYPE                        = 'ASC'
*     APPEND                          = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
*     HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*   IMPORTING
*     FILELENGTH                      =
    TABLES
      DATA_TAB                        = IDATA
*     FIELDNAMES                      =
   EXCEPTIONS
     FILE_WRITE_ERROR                = 1
     NO_BATCH                        = 2
     GUI_REFUSE_FILETRANSFER         = 3
     INVALID_TYPE                    = 4
     NO_AUTHORITY                    = 5
     UNKNOWN_ERROR                   = 6
     HEADER_NOT_ALLOWED              = 7
     SEPARATOR_NOT_ALLOWED           = 8
     FILESIZE_NOT_ALLOWED            = 9
     HEADER_TOO_LONG                 = 10
     DP_ERROR_CREATE                 = 11
     DP_ERROR_SEND                   = 12
     DP_ERROR_WRITE                  = 13
     UNKNOWN_DP_ERROR                = 14
     ACCESS_DENIED                   = 15
     DP_OUT_OF_MEMORY                = 16
     DISK_FULL                       = 17
     DP_TIMEOUT                      = 18
     FILE_NOT_FOUND                  = 19
     DATAPROVIDER_EXCEPTION          = 20
     CONTROL_FLUSH_ERROR             = 21
     OTHERS                          = 22
            .
  IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                               " DOWNLOAD_ASC
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DAT FOR EXCEL FILE 'DAT'
*&---------------------------------------------------------------------*
FORM UPLOAD_DAT TABLES U_DATA          "STRUCTURE z_alcm
                USING U_FILE       .
* mark by ymwu 2010/01/27 for unicode
*  CALL FUNCTION 'WS_UPLOAD'
*        EXPORTING
***           CODEPAGE                = 'TIBM'
*             FILENAME                = U_FILE
*             FILETYPE                = 'DAT'
*         TABLES
*              DATA_TAB                = U_DATA
*      EXCEPTIONS
*           CONVERSION_ERROR        = 1
*           INVALID_TABLE_WIDTH     = 2
*           INVALID_TYPE            = 3
*           NO_BATCH                = 4
*           UNKNOWN_ERROR           = 5
*           GUI_REFUSE_FILETRANSFER = 6
*           OTHERS                  = 7.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

 DATA: L_OUTPUT_NAME type string.
  L_OUTPUT_NAME = U_FILE.
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    FILENAME                      = L_OUTPUT_NAME
    FILETYPE                      = 'ASC'
    HAS_FIELD_SEPARATOR           = 'X'
*   HEADER_LENGTH                 = 0
*   READ_BY_LINE                  = 'X'
*   DAT_MODE                      = ' '
*   CODEPAGE                      = ' '
*   IGNORE_CERR                   = ABAP_TRUE
*   REPLACEMENT                   = '#'
*   CHECK_BOM                     = ' '
*   VIRUS_SCAN_PROFILE            =
*   NO_AUTH_CHECK                 = ' '
* IMPORTING
*   FILELENGTH                    =
*   HEADER                        =
  TABLES
    DATA_TAB                      = U_DATA
 EXCEPTIONS
   FILE_OPEN_ERROR               = 1
   FILE_READ_ERROR               = 2
   NO_BATCH                      = 3
   GUI_REFUSE_FILETRANSFER       = 4
   INVALID_TYPE                  = 5
   NO_AUTHORITY                  = 6
   UNKNOWN_ERROR                 = 7
   BAD_DATA_FORMAT               = 8
   HEADER_NOT_ALLOWED            = 9
   SEPARATOR_NOT_ALLOWED         = 10
   HEADER_TOO_LONG               = 11
   UNKNOWN_DP_ERROR              = 12
   ACCESS_DENIED                 = 13
   DP_OUT_OF_MEMORY              = 14
   DISK_FULL                     = 15
   DP_TIMEOUT                    = 16
   OTHERS                        = 17
          .
IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDFORM.                               " UPLOAD_DAT
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING    PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                               " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING  FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                                                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANS
*&---------------------------------------------------------------------*
FORM CALL_TRANS USING U_TX   U_MODE U_UPDATE U_TEXT U_FLAG .
  CLEAR : MESSTAB, MESSTAB[].

  CALL TRANSACTION U_TX USING    BDCDATA
                        MODE     U_MODE
                        UPDATE   U_UPDATE
                        MESSAGES INTO MESSTAB.
  IF SY-SUBRC = 0.
    U_FLAG = ' '.
    U_TEXT = SY-MSGV1.
  ELSE.
    U_FLAG = 'E' .
    CLEAR G_FTEXT .
    LOOP AT MESSTAB .
      IF MESSTAB-MSGTYP = 'E' OR MESSTAB-MSGTYP = 'A' .
        PERFORM GET_ERROR_MESSAGE.
      ENDIF .
    ENDLOOP .
    IF G_FTEXT = SPACE .
      PERFORM GET_ERROR_MESSAGE.
    ENDIF .
    U_TEXT = G_FTEXT .
  ENDIF.
  CLEAR : BDCDATA,BDCDATA[].
ENDFORM.                               " CALL_TRANS
*&---------------------------------------------------------------------*
*&      Form  GET_ERROR_MESSAGE
*&---------------------------------------------------------------------*
FORM GET_ERROR_MESSAGE.
  TABLES : T100 .
  DATA : G_T1(100) , G_T2(100) , G_T3(100) , G_T4(100) , G_T5(100).
  SELECT SINGLE TEXT FROM T100 INTO T100-TEXT
         WHERE SPRSL = MESSTAB-MSGSPRA
           AND ARBGB = MESSTAB-MSGID
           AND MSGNR = MESSTAB-MSGNR.
  IF SY-SUBRC = 0.
    CLEAR: G_FTEXT, G_T1, G_T2, G_T3, G_T4, G_T5.
    SPLIT T100-TEXT AT '&' INTO G_T1 G_T2 G_T3 G_T4 G_T5.
    CONCATENATE G_T1 MESSTAB-MSGV1 G_T2 MESSTAB-MSGV2 G_T3
                MESSTAB-MSGV3 G_T4 MESSTAB-MSGV4 G_T5 INTO G_FTEXT
                SEPARATED BY ' '.
  ENDIF.
ENDFORM.                               " GET_ERROR_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATE  MMDDYYYY -> YYYYMMDD轉換BATCH輸入之日期形態
*&---------------------------------------------------------------------*
FORM CHANGE_DATE USING U_DATE T_DATE .
  DATA : G_DATE LIKE SY-DATUM .
  CLEAR : G_DATE .
  MOVE U_DATE(2)   TO G_DATE+4(2).
  MOVE U_DATE+2(2) TO G_DATE+6(2).
  MOVE U_DATE+4(4) TO G_DATE(4).
*BATCH輸入之日期形態
  WRITE G_DATE TO T_DATE.
ENDFORM.                               " CHANGE_DATE
*&---------------------------------------------------------------------*
*&      Form read_exchange_rate 截取幣別換算時之單位係數
*&---------------------------------------------------------------------*
FORM READ_EXCHANGE_RATE USING U_DATE U_WAERS U_LOCAL U_TYPE
                         U_KURSF U_FACTOR1 U_FACTOR2 .
  CLEAR : U_FACTOR1 , U_FACTOR2 .
  CALL FUNCTION 'READ_EXCHANGE_RATE'
       EXPORTING
            CLIENT            = SY-MANDT
            DATE              = U_DATE
            FOREIGN_CURRENCY  = U_WAERS
            LOCAL_CURRENCY    = U_LOCAL
            TYPE_OF_RATE      = U_TYPE
       IMPORTING
            EXCHANGE_RATE     = U_KURSF
            FOREIGN_FACTOR    = U_FACTOR1
            LOCAL_FACTOR      = U_FACTOR2
*              VALID_FROM_DATE   =
*              DERIVED_RATE_TYPE =
*              FIXED_RATE        =
       EXCEPTIONS
            NO_RATE_FOUND     = 1
            NO_FACTORS_FOUND  = 2
            NO_SPREAD_FOUND   = 3
            DERIVED_2_TIMES   = 4
            OVERFLOW          = 5
            OTHERS            = 6
            .
  IF SY-SUBRC <> 0.
    U_FACTOR1 = 0 .
    U_KURSF = 0 .
  ELSE .
    IF U_KURSF < 0 .
      U_KURSF = 1 / ( U_KURSF * -1 ) .
    ENDIF .
    U_KURSF = U_KURSF * U_FACTOR2 / U_FACTOR1 .
  ENDIF.

*  CALL FUNCTION 'READ_EXCHANGE_RATE_N'
*       EXPORTING
*            CLIENT            = SY-MANDT
*            DATE              = U_DATE
*            FOREIGN_CURRENCY  = U_WAERS
*            LOCAL_CURRENCY    = U_LOCAL
*            TYPE_OF_RATE      = U_TYPE
*       IMPORTING
*            EXCHANGE_RATE     = U_KURSF
*            FOREIGN_FACTOR    = U_FACTOR1
*            LOCAL_FACTOR      = U_FACTOR2
**              VALID_FROM_DATE   =
**              DERIVED_RATE_TYPE =
**              FIXED_RATE        =
*       EXCEPTIONS
*            NO_RATE_FOUND     = 1
*            NO_FACTORS_FOUND  = 2
*            NO_SPREAD_FOUND   = 3
*            DERIVED_2_TIMES   = 4
*            OVERFLOW          = 5
*            OTHERS            = 6
*            .
*  IF SY-SUBRC <> 0.
*    U_FACTOR1 = 0 .
*    U_KURSF = 0 .
*  ELSE .
*    IF U_KURSF < 0 .
*      U_KURSF = 1 / ( U_KURSF * -1 ) .
*    ENDIF .
*    U_KURSF = U_KURSF * U_FACTOR2 / U_FACTOR1 .
*  ENDIF.
ENDFORM.                               " read_exchange_rate
*&---------------------------------------------------------------------*
*&      Form  CHANGE_LAST_DATE 截取上個月之最後一天為傳票日期
*&---------------------------------------------------------------------*
FORM CHANGE_LAST_DATE USING U_DATE U_DAT1.
  U_DAT1 = U_DATE .
  U_DAT1+6(2) = '01' .
  U_DAT1 = U_DAT1 - 1 .
ENDFORM.                               " CHANGE_LAST_DATE
*&---------------------------------------------------------------------*
*&      Form  REVERSE_DOC
*&---------------------------------------------------------------------*
FORM REVERSE_DOC USING  U_BELNR U_BUKRS U_GJAHR U_STGRD U_DATUM.
  DATA : F_DATUM(10) .
*SCREEN 0105 FOR REVERSE
  PERFORM BDC_DYNPRO  USING 'SAPMF05A'  '0105' .
  PERFORM BDC_FIELD   USING 'RF05A-BELNS' U_BELNR .     "欲迴轉傳票
  PERFORM BDC_FIELD   USING 'BKPF-BUKRS'  U_BUKRS .     "公司代碼
  PERFORM BDC_FIELD   USING 'RF05A-GJAHS' U_GJAHR .     "會計年度
  PERFORM BDC_FIELD   USING 'UF05A-STGRD' U_STGRD .     "迴轉原因
  IF U_STGRD <> '01' .  "次月迴轉
    WRITE U_DATUM TO F_DATUM .
    PERFORM BDC_FIELD   USING 'BSIS-BUDAT'  F_DATUM .     "過帳日期
    CLEAR F_DATUM .
  ENDIF .
  PERFORM BDC_FIELD   USING 'BDC_OKCODE' '=BU' .            "OKCODE
ENDFORM.                               " REVERSE_DOC
*&---------------------------------------------------------------------*
*&      Form  CHECK_LFA1
*&---------------------------------------------------------------------*
FORM CHECK_LFA1 USING U_LIFNR U_FLAG U_TEXT U_CODE .
  TRANSLATE U_LIFNR TO UPPER CASE .
* FORNT ZERO
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = U_LIFNR
       IMPORTING
            OUTPUT = U_LIFNR.
  SELECT SINGLE LIFNR FROM LFA1 INTO F_LIFNR
                      WHERE LIFNR = U_LIFNR .
  IF SY-SUBRC <> 0 AND U_CODE <> 'A' . "新增時不判斷
    U_FLAG = 'E' .
    U_TEXT = 'VANDER NOT FOUND!' .
  ENDIF .
ENDFORM.                               " CHECK_LFA1
*&---------------------------------------------------------------------*
*&      Form  CHECK_KNA1
*&---------------------------------------------------------------------*
FORM CHECK_KNA1 USING U_KUNNR U_FLAG U_TEXT U_CODE .
  TRANSLATE U_KUNNR TO UPPER CASE .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = U_KUNNR
       IMPORTING
            OUTPUT = U_KUNNR.
  SELECT SINGLE KUNNR FROM KNA1 INTO F_KUNNR
                      WHERE KUNNR = U_KUNNR .
  IF SY-SUBRC <> 0 AND U_CODE <> 'A' . "新增時不判斷
    U_FLAG = 'E' .
    U_TEXT = 'CUSTOMER NOT FOUND!' .
  ENDIF .
ENDFORM.                               " CHECK_LFA1
*&---------------------------------------------------------------------*
*&      Form  PAGE_HEADER
*&---------------------------------------------------------------------*
FORM PAGE_HEADER USING U_TITLE .
  DATA: F_POSITION LIKE SY-LINSZ ,
        F_TITLE(32)              .
  IF SY-LANGU = 'M' .
    F_TITLE = '新唐科技股份有限公司' .
  ELSE .
    F_TITLE = 'Nuvoton Technology Corporation' .
  ENDIF .
  WRITE AT /1(SY-LINSZ) F_TITLE CENTERED .
  SKIP .
  WRITE : 1 'Program:', SY-REPID(15) . "CPROG .      主程式代碼
  F_POSITION = SY-LINSZ - 20 .
  WRITE: AT F_POSITION 'Date:', SY-DATUM .
  WRITE : /1 'User   :', SY-UNAME .
  WRITE: AT F_POSITION 'Time:',SY-UZEIT .
* add by ymwu 20100602 for unicode
  DATA: F_POS LIKE SY-LINSZ ,          "報表位置
        F_LINE LIKE SY-LINSZ ,         "報表長度
        F_TITLE1(50)  .                "報表名稱
  F_TITLE1 = U_TITLE .
  CONDENSE F_TITLE1 .
  CALL METHOD cl_scp_linebreak_util=>get_visual_stringlength
    EXPORTING
      im_string               = F_TITLE1
    IMPORTING
      ex_pos_vis              = F_LINE
    EXCEPTIONS
      invalid_text_enviroment = 1
      OTHERS                  = 2.
  F_POS = ( SY-LINSZ - F_LINE ) / 2 .
  WRITE AT /F_POS(F_LINE) U_TITLE." CENTERED.
* end add by ymwu 20100602
*  WRITE: AT /1(SY-LINSZ) U_TITLE CENTERED . "marked by ymwu 2010/06/02
  WRITE AT F_POSITION 'Page:' .
  WRITE SY-PAGNO .
ENDFORM.                               " PAGE_HEADER
*&---------------------------------------------------------------------*
*&      Form  PAGE_HEADER1
*&---------------------------------------------------------------------*
FORM PAGE_HEADER1 USING U_BUKRS U_TITLE .
  DATA: F_POSITION LIKE SY-LINSZ ,     "後方位置
        F_LINE LIKE SY-LINSZ ,         "報表長度
        F_POS LIKE SY-LINSZ ,          "報表位置
        F_BUTXT(32)    ,               "公司名稱
        F_TITLE1(100)  ,               "報表名稱
        F_ADRNR LIKE T001-ADRNR,       "ADD BY YMWU 2012/01/03
        F_LANGU LIKE SY-LANGU.
  CLEAR : F_BUTXT .
  IF SY-LANGU = 'M' .
     F_LANGU = SY-LANGU .
  ELSE.
     F_LANGU = 'I' .
  ENDIF.
  SELECT SINGLE ADRNR BUTXT  FROM T001 INTO (F_ADRNR,F_BUTXT)
         WHERE BUKRS = U_BUKRS .
  SELECT SINGLE NAME1 FROM ADRC INTO F_BUTXT WHERE ADDRNUMBER =
  F_ADRNR AND NATION = F_LANGU.
*  IF SY-LANGU = 'M' ."marked by ymwu 2012/01/04
*    SELECT SINGLE BUTXT FROM T001 INTO F_ADRNR WHERE BUKRS =
*    U_BUKRS .
*  ELSE .
*    F_BUTXT = 'Nuvoton Technology Corporation' .
*  ENDIF .
  F_TITLE1 = U_TITLE .
  CONDENSE F_TITLE1 .
* add by ymwu 20100601 for unicode
  CALL METHOD cl_scp_linebreak_util=>get_visual_stringlength
    EXPORTING
      im_string               = F_TITLE1
    IMPORTING
      ex_pos_vis              = F_LINE
    EXCEPTIONS
      invalid_text_enviroment = 1
      OTHERS                  = 2.
* end add by ymwu 20100601
*  F_LINE = STRLEN( F_TITLE1 ) . mark by ymwu 2010/06/01
  F_POS = ( SY-LINSZ - F_LINE ) / 2 .
  F_POSITION = SY-LINSZ - 20 .
  WRITE AT /1(SY-LINSZ) F_BUTXT CENTERED .     "公司
  WRITE : /1 'Program:', SY-REPID(15) ."主程式代碼,日期
  WRITE: AT F_POSITION 'Date:', SY-DATUM .
  WRITE : /1 'User   :', SY-UNAME .    "製表人,報表表頭,時間
  WRITE : AT F_POS(F_LINE)  U_TITLE CENTERED .
  WRITE : AT F_POSITION 'Time:',SY-UZEIT .
  WRITE : / ' ' ,                      "頁數
          AT F_POSITION 'Page:' , SY-PAGNO .
ENDFORM.                               " PAGE_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_HIERARCHY_DATA
*&---------------------------------------------------------------------*
FORM GET_HIERARCHY_DATA TABLES   F_NODES  F_VALUES
                        USING    F_CLASS  F_SETID   F_KOKRS.
  CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
       EXPORTING
            E_CLASS                     = F_CLASS
            E_SETID                     = F_SETID
            E_KOKRS                     = F_KOKRS
*          E_MANDT                     =
*          E_MASTER_DATA               =
*          E_STRUCTURE                 =
*          E_REPLACE_CLASS             =
*          E_REPLACE_UNIT              =
*          E_REPLACE_TABLE             = ' '
*          E_REPLACE_FIELD             = ' '
*          E_SUFFIX                    =
*          E_OLD_LINE_LEVEL            = 0
*     IMPORTING
*          I_DOUBLE_CHECK              =
*          I_MASTER_DATA               =
       TABLES
            T_NODES                     = F_NODES
            T_VALUES                    = F_VALUES
*          T_MASTER_DATA               =
*          T_FORMULA                   =
*          T_FIELD_INFO                =
*          T_NODE_LIST_OVERWRITE       =
       CHANGING
            C_INFO                      = F_INFO
            C_OVERWRITE                 = F_OVER
       EXCEPTIONS
            NO_CONTROLLING_AREA         = 1
            NO_CHART_OF_ACCOUNT         = 2
            DIFFERENT_CONTROLLING_AREAS = 3
            DIFFERENT_CHART_OF_ACCOUNTS = 4
            SET_NOT_FOUND               = 5
            ILLEGAL_FIELD_REPLACEMENT   = 6
            ILLEGAL_TABLE_REPLACEMENT   = 7
            FM_RAISE                    = 8
            CONVERT_ERROR               = 9
            NO_OVERWRITE_STANDARD_HIER  = 10
            NO_BUKRS_FOR_KOKRS          = 11
            OTHERS                      = 12
            .
  IF SY-SUBRC <> 0.

  ENDIF.
ENDFORM.                               " GET_HIERARCHY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_HIERARCHY
*&---------------------------------------------------------------------*
FORM GET_HIERARCHY TABLES F_NODES F_VALUES
                   USING  F_CLASS F_NAME F_KOKRS.
  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
       EXPORTING
            SETCLASS             = F_CLASS
            SHORTNAME            = F_NAME
            KOKRS                = F_KOKRS
*         KTOPL                =
*         LIB                  =
*         RNAME                =
*         ECCS_DIMEN           =
*         ECCS_ITCLG           =
*         ECCS_SITYP           =
       IMPORTING
            SETID                = F_SETID
       EXCEPTIONS
            NO_CO_AREA_SPECIFIED = 1
            ILLEGAL_SETCLASS     = 2
            OTHERS               = 3
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  PERFORM GET_HIERARCHY_DATA TABLES F_NODES F_VALUES
                             USING F_CLASS F_SETID F_KOKRS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_ID
*&---------------------------------------------------------------------*
* 身分證統一編號檢查程式
*&---------------------------------------------------------------------*
* 注意事項 : A ~ Z 的數字檢查碼
*     A : 10, B : 11, C : 12, D : 13, E : 14, F : 15, G : 16,
*     H : 17, J : 18, K : 19, L : 20, M : 21, N : 22, P : 23,
*     Q : 24, R : 25, S : 26, T : 27, U : 28, V : 29,
*
*     X : 30, Y : 31, W : 32, Z : 33, I : 34, O : 35 <--- 特別碼
*&---------------------------------------------------------------------*
* 計算公式 :
* EX : CODE = A121114455
* 第一碼 = A , 則 A = 10
* TOTAL = A+1(1) + A+2(1)*9 + CODE+1(1)*8 + CODE+2(1)*7 +CODE+3(1)*6 +
*                             CODE+4(1)*5 + CODE+5(1)*4 +CODE+6(1)*3 +
*                             CODE+7(1)*2 + CODE+8(1)
* MOD = 10 - ( MOD = TOTAL / 10 )
* IF CODE+9(1) = MOD ----> 正確
*----------------------------------------------------------------------*
* --> CHECK_ID(10) TYPE C
*----------------------------------------------------------------------*
FORM CHECK_INPUT_ID USING    CHECK_ID
                             P_MESSAGE.

  DATA: BEGIN OF CHECK_NUMBER OCCURS 0," 英文字母代碼存放區
          CODE(1),                     " 英文字母存放區
          NUMBER(2) TYPE N,            " 代碼存放區
        END   OF CHECK_NUMBER.

  DATA: ABCDE(52),                     " 英文字母大小寫存放區
        NUMBER(10) VALUE '0123456789', " 數字存放區
        TABIX LIKE SY-TABIX,
        TOT_FG TYPE I,
        MOD_FG TYPE I.

*$*$   ### 計算英文代碼 ###
  CLEAR SY-TABIX.
  DO 26 TIMES.
    TABIX = TABIX + 1.
    SY-INDEX = SY-INDEX - 1.
    CLEAR CHECK_NUMBER.

    CHECK_NUMBER-CODE = SY-ABCDE+SY-INDEX(1).

    IF CHECK_NUMBER-CODE = 'I'.
      CHECK_NUMBER-NUMBER = 34.
      TABIX = TABIX - 1.
    ELSEIF CHECK_NUMBER-CODE = 'O'.
      CHECK_NUMBER-NUMBER = 35.
      TABIX = TABIX - 1.
    ELSE.
      CHECK_NUMBER-NUMBER = 9 + TABIX.
    ENDIF.

    IF CHECK_NUMBER-NUMBER = 30.
      CHECK_NUMBER-CODE = 'X'.
    ELSEIF CHECK_NUMBER-NUMBER = 31.
      CHECK_NUMBER-CODE = 'Y'.
    ELSEIF CHECK_NUMBER-NUMBER = 32.
      CHECK_NUMBER-CODE = 'W'.
    ELSEIF CHECK_NUMBER-NUMBER = 33.
      CHECK_NUMBER-CODE = 'Z'.
    ENDIF.
    APPEND CHECK_NUMBER.
  ENDDO.

*$*&  ### 結合英文大小寫 ###
  ABCDE = SY-ABCDE.

*translate abcde TO UPPER CASE.
  TRANSLATE ABCDE TO LOWER CASE.

  CONCATENATE SY-ABCDE ABCDE INTO ABCDE.

  IF  CHECK_ID(1)   CO ABCDE  AND
      CHECK_ID+1(9) CO NUMBER AND
    ( CHECK_ID+1(1) = 1 OR CHECK_ID+1(1) = 2 ).
    READ TABLE CHECK_NUMBER WITH KEY CODE = CHECK_ID(1).
    TOT_FG = CHECK_NUMBER-NUMBER(1) + CHECK_NUMBER-NUMBER+1(1) * 9
           + CHECK_ID+1(1) * 8 + CHECK_ID+2(1) * 7
           + CHECK_ID+3(1) * 6 + CHECK_ID+4(1) * 5
           + CHECK_ID+5(1) * 4 + CHECK_ID+6(1) * 3
           + CHECK_ID+7(1) * 2 + CHECK_ID+8(1).
* ADD 整除時,取0非10
*    MOD_FG = TOT_FG MOD 10.
*    MOD_FG = 10 - MOD_FG.
*    CHECK MOD_FG <> CHECK_ID+9(1).
    TOT_FG = TOT_FG + CHECK_ID+9(1) .
    MOD_FG = TOT_FG MOD 10.
    CHECK MOD_FG <> 0 .
    MOVE 'ID ERROR' TO P_MESSAGE.
    EXIT.
  ELSE.
    MOVE 'INPUT FORMAT ERROR' TO P_MESSAGE.
    EXIT.
  ENDIF.
ENDFORM.                               " CHECK_INPUT_ID

*&---------------------------------------------------------------------*
*&      Form  CHECK_UNICODE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* P_UNICODE = A1 A2 A3 A4 A5 A6 A7 A8
* STRING    = A1 A3 A5 A8 B1 B2 B3 B4 B5 B6 B7 B8
*                         ----- ----- ----- -----
*                         2*A2  2*A4  2*A6  4*A7
* SUM( STRING(1...12) )
*----------------------------------------------------------------------*
FORM CHECK_UNICODE USING    P_UNICODE
                            P_MESSAGE.

  DATA  STRING(12) TYPE N.
  DATA: BEGIN OF IDAY OCCURS 0,
          I TYPE I,
        END OF IDAY.
  DATA: LEF TYPE I.
  DATA: UNICODE(8).
  UNICODE = P_UNICODE.
  IF UNICODE CO '0123456789'.
  ELSE.
    MOVE 'INPUT FORMAT ERROR'  TO P_MESSAGE.
    EXIT.
  ENDIF.
  LEF = STRLEN( UNICODE ).
  IF LEF NE 8.
    MOVE 'UNICODE ERROR' TO P_MESSAGE.
    EXIT.
  ENDIF.
  STRING(1) = UNICODE(1).
  STRING+1(1) = UNICODE+2(1).
  STRING+2(1) = UNICODE+4(1).
  STRING+3(1) = UNICODE+7(1).
  STRING+4(2) = 2 * UNICODE+1(1).
  STRING+6(2) = 2 * UNICODE+3(1).
  STRING+8(2) = 2 * UNICODE+5(1).
  STRING+10(2) = 4 * UNICODE+6(1).
  STRING = STRING(1) + STRING+1(1) + STRING+2(1) + STRING+3(1)
         + STRING+4(1) + STRING+5(1) + STRING+6(1) + STRING+7(1)
         + STRING+8(1) + STRING+9(1) + STRING+10(1) + STRING+11(1).
  IF NOT ( STRING+11(1) = 0
     OR ( STRING+11(1) = 9 AND UNICODE+6(1) = 7 ) ).
    MOVE 'UNICODE ERROR' TO P_MESSAGE.
    EXIT.
  ENDIF.
ENDFORM.                               " CHECK_UNICODE
*----------------------------------------------------------------------*
* CHECK_DATE_CONVERT : 檢查日期格式YYYYMMDD
*----------------------------------------------------------------------*
FORM CHECK_DATE_CONVERT USING F_DATE1 F_FALG F_TEXT .
  DATA : L_DATE LIKE SY-DATUM .
  CLEAR : L_DATE .
  L_DATE = F_DATE1 .
  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
       EXPORTING
            DATE                      = L_DATE
       EXCEPTIONS
            PLAUSIBILITY_CHECK_FAILED = 1
            OTHERS                    = 2.
  IF SY-SUBRC <> 0.
    F_FLAG = 'E' .
    F_TEXT = '日期轉換錯誤,非日期格式YYYYMMDD!' .
  ENDIF.
ENDFORM .
*&---------------------------------------------------------------------*
*&      Form  INPUT_ZERO 前置0
*&---------------------------------------------------------------------*
FORM INPUT_ZERO USING  U_ALPHA .
  TRANSLATE U_ALPHA TO UPPER CASE .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'               " 前置0
       EXPORTING
            INPUT   = U_ALPHA
       IMPORTING
            OUTPUT  = U_ALPHA .
ENDFORM.                               " INPUT_ZERO
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ZERO 去除前置零
*&---------------------------------------------------------------------*
FORM OUTPUT_ZERO USING  U_ALPHA .
  TRANSLATE U_ALPHA TO UPPER CASE .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'    "去除前置零
       EXPORTING
            INPUT   = U_ALPHA
       IMPORTING
            OUTPUT  = U_ALPHA .
ENDFORM.                               " OUTPUT_ZERO
*&---------------------------------------------------------------------*
*&      Form  OVERLAY_SIGN : 將u_text1中有u_text2(如:")符號者取代為空白
*----------------------------------------------------------------------*
FORM OVERLAY_SPACE USING  U_TEXT1 U_TEXT2.
  DATA : L_SPACE(255) ,
         L_LEN TYPE I .
  CLEAR : L_SPACE , L_LEN .
* add by ymwu 20100601 for unicode
  CALL METHOD cl_scp_linebreak_util=>get_visual_stringlength
    EXPORTING
      im_string               = U_TEXT1
    IMPORTING
      ex_pos_vis              = L_LEN
    EXCEPTIONS
      invalid_text_enviroment = 1
      OTHERS                  = 2.
* end add by ymwu 2010601
*  L_LEN = STRLEN( U_TEXT1 ) . marked by ymwu 2010/06/01
  IF U_TEXT2 NE SPACE AND L_LEN > 0 .
    OVERLAY U_TEXT1 WITH L_SPACE(L_LEN) ONLY U_TEXT2 .
    CONDENSE U_TEXT1 .
  ENDIF .
ENDFORM.                    " OVERLAY_SIGN

*&---------------------------------------------------------------------*
*&      Form  download_file_gui :
*----------------------------------------------------------------------*
form download_file_gui tables u_data u_data1         "STRUCTURE z_alcm
                using u_file u_type      .

 data: l_output_name type string.
  l_output_name = u_file.

  call function 'GUI_DOWNLOAD'
    exporting
*     BIN_FILESIZE                    =
      filename                        = l_output_name
      filetype                        = u_type
*      filetype                        = 'ASC'
*     APPEND                          = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
*     HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
      TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*   IMPORTING
*     FILELENGTH                      =
    tables
      data_tab                        = u_data
      FIELDNAMES                      = u_data1
*     FIELDNAMES                      =
   exceptions
     file_write_error                = 1
     no_batch                        = 2
     gui_refuse_filetransfer         = 3
     invalid_type                    = 4
     no_authority                    = 5
     unknown_error                   = 6
     header_not_allowed              = 7
     separator_not_allowed           = 8
     filesize_not_allowed            = 9
     header_too_long                 = 10
     dp_error_create                 = 11
     dp_error_send                   = 12
     dp_error_write                  = 13
     unknown_dp_error                = 14
     access_denied                   = 15
     dp_out_of_memory                = 16
     disk_full                       = 17
     dp_timeout                      = 18
     file_not_found                  = 19
     dataprovider_exception          = 20
     control_flush_error             = 21
     others                          = 22
            .
  if sy-subrc <> 0.
     message id sy-msgid type sy-msgty number sy-msgno
         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.
*add by ymwu 2010/07/09 for unicode
*&---------------------------------------------------------------------*
*&      Form  download_file_gui_cp :
*----------------------------------------------------------------------*
form download_file_gui_cp tables u_data u_data1         "STRUCTURE z_alcm
                using u_file u_type u_code_page  .

 data: l_output_name type string.
  l_output_name = u_file.

  call function 'GUI_DOWNLOAD'
    exporting
*     BIN_FILESIZE                    =
      filename                        = l_output_name
      filetype                        = u_type
*      filetype                        = 'ASC'
*     APPEND                          = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
*     HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
      CODEPAGE                        = u_code_page
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
      TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*   IMPORTING
*     FILELENGTH                      =
    tables
      data_tab                        = u_data
      FIELDNAMES                      = u_data1
*     FIELDNAMES                      =
   exceptions
     file_write_error                = 1
     no_batch                        = 2
     gui_refuse_filetransfer         = 3
     invalid_type                    = 4
     no_authority                    = 5
     unknown_error                   = 6
     header_not_allowed              = 7
     separator_not_allowed           = 8
     filesize_not_allowed            = 9
     header_too_long                 = 10
     dp_error_create                 = 11
     dp_error_send                   = 12
     dp_error_write                  = 13
     unknown_dp_error                = 14
     access_denied                   = 15
     dp_out_of_memory                = 16
     disk_full                       = 17
     dp_timeout                      = 18
     file_not_found                  = 19
     dataprovider_exception          = 20
     control_flush_error             = 21
     others                          = 22
            .
  if sy-subrc <> 0.
     message id sy-msgid type sy-msgty number sy-msgno
         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.

*end by ymwu 2010/07/09
*YM
*&---------------------------------------------------------------------*
*&      Form  download_file : add by ymwu 2010/01/29
*----------------------------------------------------------------------*
form download_file tables u_data
                using u_file u_type      .
 data: l_output_name type string.
  l_output_name = u_file.

  call function 'GUI_DOWNLOAD'
    exporting
*     BIN_FILESIZE                    =
      filename                        = l_output_name
      filetype                        = u_type
*      filetype                        = 'ASC'
*     APPEND                          = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
*     HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
      TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*   IMPORTING
*     FILELENGTH                      =
    tables
      data_tab                        = u_data
*     FIELDNAMES                      = u_data1
*     FIELDNAMES                      =
   exceptions
     file_write_error                = 1
     no_batch                        = 2
     gui_refuse_filetransfer         = 3
     invalid_type                    = 4
     no_authority                    = 5
     unknown_error                   = 6
     header_not_allowed              = 7
     separator_not_allowed           = 8
     filesize_not_allowed            = 9
     header_too_long                 = 10
     dp_error_create                 = 11
     dp_error_send                   = 12
     dp_error_write                  = 13
     unknown_dp_error                = 14
     access_denied                   = 15
     dp_out_of_memory                = 16
     disk_full                       = 17
     dp_timeout                      = 18
     file_not_found                  = 19
     dataprovider_exception          = 20
     control_flush_error             = 21
     others                          = 22
            .
  if sy-subrc <> 0.
     message id sy-msgid type sy-msgty number sy-msgno
         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      UPLOAD_DATA_GUI_TAB
*&---------------------------------------------------------------------*
FORM UPLOAD_DATA_GUI_TAB TABLES P_DATA USING P_FILENAME .

  DATA: S_FILE TYPE STRING.
  S_FILE = P_FILENAME .
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = S_FILE
*      FILETYPE                = 'DAT'
      HAS_FIELD_SEPARATOR     = 'X'
    TABLES
      DATA_TAB                = P_DATA
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 10
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      OTHERS                  = 17.

  IF SY-SUBRC <> 0.
  ENDIF.
ENDFORM.                    " UPLOAD_DATA_GUI_TAB
*&---------------------------------------------------------------------*
*&      Form  amount_display_i_sap
*&---------------------------------------------------------------------*
form amount_display_i_sap using p_waers p_amount.

call function 'CURRENCY_AMOUNT_DISPLAY_TO_SAP'
  exporting
    currency              = p_waers
    amount_display        = p_amount
 IMPORTING
   AMOUNT_INTERNAL       = p_amount
 EXCEPTIONS
   INTERNAL_ERROR        = 1
   OTHERS                = 2.
if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
endif.
endform.                    " amount_display_i_sap
*&---------------------------------------------------------------------*
*&      Form  amount_display_o_sap
*&---------------------------------------------------------------------*
form amount_display_o_sap using p_waers p_amount .
     call function 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
          exporting
            currency           = p_waers
            amount_internal    = p_amount
          importing
            amount_display     = p_amount
          exceptions
            internal_error     = 1
            others             = 2.
if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
endif.
endform.                    " amount_display_o_sap
*&---------------------------------------------------------------------*
*&      Form  CALC_OUTPUT_LENGTH
*&---------------------------------------------------------------------*
FORM CALC_OUTPUT_LENGTH  USING    L_LENGTH L_TXT
                         CHANGING L_LEN.
  DATA : L_STR(1),
         L_LEN1 TYPE I,
         L_LEN2 TYPE I.
  L_LEN1 = 0.
  L_LEN2 = 0.
  L_LEN = 0. "字串要取幾位
  IF  cl_abap_list_utilities=>dynamic_output_length( L_TXT )
              <= L_LENGTH .
    L_LEN = STRLEN( L_TXT ).
    IF L_LEN = 0 .
       L_LEN = L_LENGTH.
    ENDIF.
  ELSEif cl_abap_list_utilities=>dynamic_output_length( L_TXT )
          = STRLEN( L_TXT ).
    L_LEN = STRLEN( L_TXT ).
  ELSE.
    DO L_LENGTH TIMES.
      IF L_LEN1 >= L_LENGTH.
        IF L_LEN1 > L_LENGTH. "如果最後一個字是中文, 就將長度減一
          L_LEN = L_LEN - 1.
        ENDIF.
        EXIT.
      ENDIF.
      L_STR = L_TXT+L_LEN(1).
      L_LEN2 =  cl_abap_list_utilities=>dynamic_output_length( L_STR )
                - STRLEN( L_STR ).
      IF L_LEN2 = 0 .
        L_LEN1 = L_LEN1 + 1. "英文+1
      ELSE.
        L_LEN1 = L_LEN1 + 2. "中文+2
      ENDIF.
      L_LEN = L_LEN + 1.
    ENDDO.
  ENDIF.

ENDFORM.                    " CALC_OUTPUT_LENGTH

*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
FORM GET_FILE USING p_file1.
  call function 'WS_FILENAME_GET'
       exporting
            def_path         = 'E:\TEMP\'
            mask             = ',*.*,*.*.'
            mode             = 'O'
       importing
            filename         = P_FILE1
       exceptions
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            others           = 5.
ENDFORM.                    " GET_FILE

*&---------------------------------------------------------------------*
*&      Form  GET_DEFAULT_PATH
*&---------------------------------------------------------------------*
FORM GET_DEFAULT_PATH USING P_CREATE P_PATH P_FILE_NAME.
     DATA : T_RET_CODE TYPE I .
  CALL FUNCTION 'ZF_GET_APDATA_PATH_SEPAR'
      EXPORTING
        I_CREATEPATH       = P_CREATE
      IMPORTING
        E_PATH             = P_PATH
        E_RET_CODE         = T_RET_CODE .
  P_FILE_NAME = P_PATH.
ENDFORM.                    "GET_DEFAULT_PATH
*&---------------------------------------------------------------------*
*&      Form  conversion_wbs_element_output
*&---------------------------------------------------------------------*
FORM conversion_wbs_output  CHANGING p_l_pspnr.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
       EXPORTING
            input  = p_l_pspnr
       IMPORTING
            output = p_l_pspnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_BUKRS_PRIVILEGE
*&---------------------------------------------------------------------*
FORM CHECK_BUKRS_PRIVILEGE  USING T_PBUKRS CHANGING R_MSGID.
     authority-check object 'F_BKPF_BUK'
                      id 'BUKRS' field t_pbukrs
                      id 'ACTVT' field '02'.
    if sy-subrc <> 0.
       r_msgid = 'F'.
    else.
       r_msgid = 'S'.
    endif.
ENDFORM.                    " CHECK_BUKRS_PRIVILEGE
