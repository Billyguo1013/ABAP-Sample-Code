************************************************************************
* Program Name  : ZRSDSA0185
* Descriptions  : Maintain Packing Standard
* Create By     : V_BILLY
* Create Date   : 2022/10/31
* Tcode         : ZRSDSA0185
************************************************************************
* Modification Log
************************************************************************
*   Date     Ver.   Programmer   Descriptions
* ---------- ----   ------------ --------------------------------------
* 20221117  DNRK945462  V_Billy  Add new function for ZTSD0019
* 20230209  DNRK948777  V_Billy  Add new function for ZTSD0187 ( MaxQty & Korea Label)
************************************************************************

REPORT zrsdpa0185 NO STANDARD PAGE HEADING
                  LINE-SIZE 255
                  LINE-COUNT 65.

INCLUDE zrsdpa0185_top. " Data declaration
INCLUDE zrsdpa0185_s01. " Selection screen
INCLUDE zrsdpa0185_f01. " function

************************************************************************
*                      START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

  CASE 'X'.
    WHEN p_r1. "Packing Standard (ZTSD0018)
      g_mode = 'ZTSD0018'.
      PERFORM do_ztsd0018.
    WHEN p_r2. "Pack Material (ZTSD0019)
      g_mode = 'ZTSD0019'.
      PERFORM do_ztsd0019.

*<< Begin add 2023/02/09 ***************************
    WHEN p_r3. "MaxQty & Korea Label
      g_mode = 'ZTSD0187'.
      PERFORM do_ZTSD0187.
*<< End   add 2023/02/09 ***************************

  ENDCASE.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&  Include           ZRSDPA0185_TOP
*&---------------------------------------------------------------------*

TABLES: sscrfields,
        kna1,
        ztsd0018,
        ztsd0019,
        ztsd0187,  "add on 2023/02/09
        vbak.

TYPE-POOLS: slis,icon,truxs,vrm.
DATA: functxt TYPE smp_dyntxt.


**********************************************************************
* Upload File
**********************************************************************
TYPES:
  BEGIN OF ty_excel,
    f001 TYPE string,
    f002 TYPE string,
    f003 TYPE string,
    f004 TYPE string,
    f005 TYPE string,
    f006 TYPE string,
    f007 TYPE string,
    f008 TYPE string,
    f009 TYPE string,
    f010 TYPE string,
    f011 TYPE string,
    f012 TYPE string,
    f013 TYPE string,
    f014 TYPE string,
    f015 TYPE string,
    f016 TYPE string,    "add on 2023/02/21
    f017 TYPE string,    "add on 2023/02/21
  END OF ty_excel,
  tt_excel TYPE TABLE OF ty_excel.

**********************************************************************
* Upload Data
**********************************************************************
TYPES: BEGIN OF ty_data,
          zzvkorg	       TYPE vkorg,
          zzpartno       TYPE zzpartno_25,
          zzshape	       TYPE zmmkind,
          zzproductno	   TYPE zzproductno_25,
          zzpackmatno	   TYPE zzpackmatno,
          zzgccode       TYPE zgccode,
          cust_no	       TYPE zcust_no,
          zzpackmatnocat TYPE zzpackmatnocat,
          zzmeasure	     TYPE zzmeasure,
          by_pallet	     TYPE zby_pallet,
          zzboxqty       TYPE zzboxqty,
*          zzcapacityrate TYPE zzcapacityrate,
          zzcapacityrate(13) TYPE c,
          zztotalpackqty TYPE zztotalpackqty,
          ntgew	         TYPE ntgew,
          brgew          TYPE brgew,
          zzmaxpackqty   TYPE zzmaxqty,   "add 2023/02/21
          zzalumbag      TYPE zzalumbag,  "add 2023/02/21
       END OF ty_data.
DATA: gt_data   TYPE TABLE OF ty_data,
      gs_data   TYPE ty_data.

TYPES: BEGIN OF ty_data2,
          zzpackmatno	   TYPE zzpackmatno,
          zzpackmatnocat TYPE zzpackmatnocat,
          zzmeasure	     TYPE zzmeasure,
          ntgew	         TYPE ntgew,
          brgew          TYPE brgew,
       END OF ty_data2.
DATA: gt_data2   TYPE TABLE OF ty_data2,
      gs_data2   TYPE ty_data2.

*<< Begin add 2023/02/09 ********************************
TYPES: BEGIN OF ty_data3,
          zzgccode         TYPE zgccode,
          zzpartno         TYPE zzpartno,
          zzproductno	     TYPE zzproductno,
          zzshape	         TYPE zzshape,
          zzgrade          TYPE zzgrade,
          matnr            TYPE matnr,
          zzmaxpackcapa    TYPE zzmaxpackcapa,
          zzrecyclelabelno TYPE zzrecyclelabelno,
          ernam            TYPE ernam,
          erdat            TYPE erdat,
          erzet            TYPE erzet,
       END OF ty_data3.
DATA: gt_data3   TYPE TABLE OF ty_data3,
      gs_data3   TYPE ty_data3.
*<< End   add 2023/02/09 ********************************


**********************************************************************
* ALV
**********************************************************************
DATA:
  gt_fieldcat TYPE slis_t_fieldcat_alv,
  gs_fieldcat TYPE slis_fieldcat_alv,
  gs_layout TYPE slis_layout_alv,
  selfld TYPE slis_selfield,
  g_grid TYPE REF TO cl_gui_alv_grid,
  g_variant   LIKE disvariant.

TYPES: BEGIN OF ty_alv,
        checkbox(1) TYPE c,
        status TYPE icon_d,
        seq TYPE i,
        msg(100) TYPE c.
*        INCLUDE STRUCTURE ztsd0018.
        INCLUDE TYPE ty_data.
TYPES: END OF ty_alv.
DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.

TYPES: BEGIN OF ty_alv2,
        checkbox(1) TYPE c.
        INCLUDE STRUCTURE ztsd0018.
TYPES: END OF ty_alv2.
DATA: gt_alv2 TYPE TABLE OF ty_alv2,
      gs_alv2 TYPE ty_alv2.

TYPES: BEGIN OF ty_alv3,
        checkbox(1) TYPE c,
        status TYPE icon_d,
        seq TYPE i,
        msg(100) TYPE c.
        INCLUDE TYPE ty_data2.
TYPES: END OF ty_alv3.
DATA: gt_alv3 TYPE TABLE OF ty_alv3,
      gs_alv3 TYPE ty_alv3.

TYPES: BEGIN OF ty_alv4,
        checkbox(1) TYPE c.
        INCLUDE STRUCTURE ztsd0019.
TYPES: END OF ty_alv4.
DATA: gt_alv4 TYPE TABLE OF ty_alv4,
      gs_alv4 TYPE ty_alv4.

*<< Begin add on 2023/02/09 ****************************
TYPES: BEGIN OF ty_alv5,
        checkbox(1) TYPE c,
        status TYPE icon_d,
        seq TYPE i,
        msg(100) TYPE c.
        INCLUDE TYPE ty_data3.
TYPES: END OF ty_alv5.
DATA: gt_alv5 TYPE TABLE OF ty_alv5,
      gs_alv5 TYPE ty_alv5.

TYPES: BEGIN OF ty_alv6,
        checkbox(1) TYPE c.
        INCLUDE STRUCTURE ztsd0187.
TYPES: END OF ty_alv6.
DATA: gt_alv6 TYPE TABLE OF ty_alv6,
      gs_alv6 TYPE ty_alv6.
*<< End   add on 2023/02/09 ****************************

FIELD-SYMBOLS: <alv>  LIKE LINE OF gt_alv,
               <alv2> LIKE LINE OF gt_alv2,
               <alv3> LIKE LINE OF gt_alv3,
               <alv4> LIKE LINE OF gt_alv4,
               <alv5> LIKE LINE OF gt_alv5,  "add on 2023/02/09
               <alv6> LIKE LINE OF gt_alv6.  "add on 2023/02/09

DATA : new_table TYPE REF TO data,
       new_line  TYPE REF TO data.

FIELD-SYMBOLS :
                <g_table> TYPE STANDARD TABLE,
                <g_line>  TYPE ANY,
                <g_field> TYPE ANY.


DATA g_mode TYPE tname.
DATA g_brand TYPE c.

**********************************************************************
* Global Data
**********************************************************************
DATA: gt_ztsd0019     TYPE TABLE OF ztsd0019,
      gs_ztsd0019     TYPE ztsd0019,
      gt_ztsd0018     TYPE TABLE OF ztsd0018,
      gs_ztsd0018     TYPE ztsd0018,
      gt_ztsd0087     TYPE TABLE OF ztsd0087,
      gs_ztsd0087     TYPE ztsd0087,
      gt_ztsd0018_log TYPE TABLE OF ztsd0018_log,
      gs_ztsd0018_log TYPE ztsd0018_log,
      gt_ztsd0187     TYPE TABLE OF ztsd0187,     "add on 2023/02/09
      gs_ztsd0187     TYPE ztsd0187,              "add on 2023/02/09
      gt_ztsd0187_log TYPE TABLE OF ztsd0187_log, "add on 2023/02/09
      gs_ztsd0187_log TYPE ztsd0187_log.          "add on 2023/02/09

DATA: g_tabix TYPE sy-tabix.
DATA: g_error(1) TYPE c.

DATA: s_zzpackmatno TYPE RANGE OF zzpackmatno,
      s_zzgccode    TYPE RANGE OF zgccode,
      s_cust_no     TYPE RANGE OF zcust_no.

DATA: g_timestamp	  TYPE char15.

FIELD-SYMBOLS: <rs_zzpackmatno> LIKE LINE OF s_zzpackmatno,
               <rs_zzgccode>    LIKE LINE OF s_zzgccode,
               <rs_cust_no>     LIKE LINE OF s_cust_no,
               <ztsd0018_log>   LIKE LINE OF gt_ztsd0018_log.

**********************************************************************
* Popup message
**********************************************************************
TYPE-POOLS: esp1.
DATA: t_message_tab	TYPE esp1_message_tab_type.
FIELD-SYMBOLS: <message> LIKE LINE OF t_message_tab.

**********************************************************************
* ICON
**********************************************************************
DATA: gv_led_gray   TYPE icon-id,
      gv_led_green  TYPE icon-id,
      gv_led_red    TYPE icon-id,
      gv_led_yellow TYPE icon-id.

CONSTANTS: gc_white   TYPE iconname VALUE 'ICON_LIGHT_OUT',
           gc_green   TYPE iconname VALUE 'ICON_LED_GREEN',
           gc_red     TYPE iconname VALUE 'ICON_LED_RED',
           gc_yellow  TYPE iconname VALUE 'ICON_LED_YELLOW'.


*&---------------------------------------------------------------------*
*& macro
*&---------------------------------------------------------------------*
DEFINE remove_symbol.
  replace all occurrences of ',' in &1 with space.
  condense &1 no-gaps.
END-OF-DEFINITION.

DEFINE conversion_exit_alpha_input.
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = &1
    importing
      output = &1.
END-OF-DEFINITION.

DEFINE field_desc_i.
  clear gw_fieldtab.
  gw_fieldtab-fieldname    = &1.
  gw_fieldtab-scrtext_s    = &2.
  gw_fieldtab-scrtext_m    = &3.
  gw_fieldtab-scrtext_l    = &4.
  gw_fieldtab-outputlen    = &5.

  if gw_fieldtab-fieldname = 'SEL'.
    gw_fieldtab-tech = 'X'.
  endif.

  gw_fieldtab-colddictxt = 'L'.
  gw_fieldtab-selddictxt = 'L'.
  gw_fieldtab-tipddictxt = 'L'.

  append gw_fieldtab to gt_fcat.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&  Include           ZRSDPA0185_S01
*&---------------------------------------------------------------------*
**********************************************************************
* Selection Screen
**********************************************************************

SELECTION-SCREEN BEGIN OF BLOCK blk0 WITH FRAME TITLE text-s00.

SELECTION-SCREEN: FUNCTION KEY 1.
SELECTION-SCREEN: FUNCTION KEY 2.
SELECTION-SCREEN: FUNCTION KEY 3.

PARAMETERS: p_r1 RADIOBUTTON GROUP gp1 DEFAULT 'X' USER-COMMAND flag.
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-002.
PARAMETERS: r1      TYPE c RADIOBUTTON GROUP gr1  DEFAULT 'X' MODIF ID 01.
PARAMETERS: p_vkorg(7) AS LISTBOX VISIBLE LENGTH 10 DEFAULT ' ' LOWER CASE MODIF ID 01.
PARAMETERS: p_file  TYPE rlgrap-filename MODIF ID 01.

SELECTION-SCREEN SKIP 1.

PARAMETERS: r2 TYPE c RADIOBUTTON GROUP gr1 MODIF ID 01.
SELECT-OPTIONS:
    s_part   FOR ztsd0018-zzpartno MODIF ID 01,
    s_shape  FOR ztsd0018-zzshape MODIF ID 01,
    s_packno FOR ztsd0018-zzpackmatno MODIF ID 01,
    s_gccode FOR ztsd0018-zzgccode MODIF ID 01,
    s_custno FOR ztsd0018-cust_no MODIF ID 01,
    s_cat    FOR ztsd0018-zzpackmatnocat MODIF ID 01,
    s_measur FOR ztsd0018-zzmeasure MODIF ID 01,
    s_pallet FOR ztsd0018-by_pallet MODIF ID 01.
PARAMETERS:  p_path TYPE string MODIF ID 01.

SELECTION-SCREEN END OF BLOCK blk1.

PARAMETERS: p_r2 RADIOBUTTON GROUP gp1.
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-000.
PARAMETERS: r3 TYPE c RADIOBUTTON GROUP gr2 MODIF ID 02.
PARAMETERS: p_file2 TYPE rlgrap-filename MODIF ID 02.

SELECTION-SCREEN SKIP 1.

PARAMETERS: r4 TYPE c RADIOBUTTON GROUP gr2 MODIF ID 02.
SELECT-OPTIONS:
    s_part2 FOR ztsd0019-zzpackmatno MODIF ID 02,
    s_cat2  FOR ztsd0019-zzpackmatnocat MODIF ID 02.
PARAMETERS:  p_path2 TYPE string MODIF ID 02.

SELECTION-SCREEN END OF BLOCK blk2.

*<< Begin add 2023/02/09 **********************************************
PARAMETERS: p_r3 RADIOBUTTON GROUP gp1.
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-000.
PARAMETERS: r5 TYPE c RADIOBUTTON GROUP gr3 MODIF ID 03.
PARAMETERS: p_file3 TYPE rlgrap-filename MODIF ID 03.

SELECTION-SCREEN SKIP 1.

PARAMETERS: r6 TYPE c RADIOBUTTON GROUP gr3 MODIF ID 03.
SELECT-OPTIONS:
    s_gcode   FOR ztsd0187-zzgccode MODIF ID 03,
    s_rlabel  FOR ztsd0187-zzrecyclelabelno MODIF ID 03.
PARAMETERS:  p_path3 TYPE string MODIF ID 03.

SELECTION-SCREEN END OF BLOCK blk3.
*<< End   add 2023/02/09 **********************************************

SELECTION-SCREEN END OF BLOCK blk0.

************************************************************************
* INITIALIZATION
************************************************************************
INITIALIZATION.
  functxt-icon_id = icon_xxl.
  functxt-quickinfo = text-m01.
  functxt-icon_text = text-m01.
  sscrfields-functxt_01 = functxt.

  functxt-icon_id = icon_xxl.
  functxt-quickinfo = text-m02.
  functxt-icon_text = text-m02.
  sscrfields-functxt_02 = functxt.

  functxt-icon_id = icon_xxl.
  functxt-quickinfo = text-m03.
  functxt-icon_text = text-m03.
  sscrfields-functxt_03 = functxt.

* Path: user APData
  CALL FUNCTION 'ZF_GET_APDATA_PATH'
    EXPORTING
      i_createpath = ''
    IMPORTING
      e_path       = p_path.

  p_path2 = p_path.
  p_path3 = p_path.   "add 2023/02/09

  SELECT SINGLE id INTO gv_led_gray   FROM icon WHERE name = gc_white.
  SELECT SINGLE id INTO gv_led_green  FROM icon WHERE name = gc_green.
  SELECT SINGLE id INTO gv_led_red    FROM icon WHERE name = gc_red.
  SELECT SINGLE id INTO gv_led_yellow FROM icon WHERE name = gc_yellow.


**********************************************************************
* AT Selection Screen
**********************************************************************
AT SELECTION-SCREEN OUTPUT.
  CASE abap_true.
    WHEN p_r1.
      LOOP AT SCREEN.
        IF ( screen-group1 = '02' OR screen-group1 = '03' ). "add on 2023/02/09
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN p_r2.
      LOOP AT SCREEN.
        IF ( screen-group1 = '01' OR screen-group1 = '03' ). "add on 2023/02/09
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN p_r3.
      LOOP AT SCREEN.
        IF ( screen-group1 = '01' OR screen-group1 = '02' ). "add on 2023/02/09
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.

  DATA: name TYPE vrm_id,
        list TYPE vrm_values,
        value TYPE vrm_value.
  name = 'P_VKORG'.
  value-key = 'Waltech'.
  APPEND value TO list.
  value-key = 'UTAC'.
  APPEND value TO list.
  value-key = 'NTC'.
  APPEND value TO list.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id                    = name
      values                = list
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS                = 2
            .

*<< Get File Name
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM get_filename CHANGING p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file2.
  PERFORM get_filename CHANGING p_file2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file3.
  PERFORM get_filename CHANGING p_file3.

*<< Get Download Path
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM search_path CHANGING p_path.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.
  PERFORM search_path CHANGING p_path2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path3.
  PERFORM search_path CHANGING p_path3.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM download_template USING 'ZRSDPA0185'.
    WHEN 'FC02'.
      PERFORM download_template USING 'ZRSDPA0185_PM'.
    WHEN 'FC03'.
      PERFORM download_template USING `ZRSDPA0185_MAX & KOREALBL`. "add on 2023/02/09
  ENDCASE.

*&---------------------------------------------------------------------*
*&  Include           ZRSDPA0185_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FILE  text
*----------------------------------------------------------------------*
FORM get_filename  CHANGING c_file.

  DATA: l_filename    TYPE string,
        l_user_action TYPE i.

  CALL FUNCTION 'GUI_FILE_LOAD_DIALOG'
    EXPORTING
      window_title = 'Please select a file'
      file_filter  = 'EXCEL|*.XLSX;*.XLS'
    IMPORTING
      fullpath     = l_filename
      user_action  = l_user_action.
  IF l_user_action = 0.
    c_file = l_filename.
  ENDIF.

ENDFORM.                    " GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  SEARCH_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_PATH  text
*----------------------------------------------------------------------*
FORM search_path  CHANGING c_dir.

  DATA : l_title  TYPE string, " Window Title
         l_string TYPE string. "File path string

  l_title = 'Search for Download Path'.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = l_title
      initial_folder  = 'C:\'
    CHANGING
      selected_folder = l_string
    EXCEPTIONS
      cntl_error      = 1
      error_no_gui    = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF l_string IS INITIAL.
* Path: user APData
    CALL FUNCTION 'ZF_GET_APDATA_PATH'
      EXPORTING
        i_createpath = ''
      IMPORTING
        e_path       = l_string.
  ENDIF.

  c_dir = l_string.

ENDFORM.                    " SEARCH_PATH
*&---------------------------------------------------------------------*
*&  Include           ZRSDSA0178_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0049   text
*----------------------------------------------------------------------*
FORM download_template  USING p_objid TYPE wwwdata-objid.
  DATA: l_objdata     LIKE wwwdatatab,
        l_destination LIKE rlgrap-filename,
        l_rc          LIKE sy-subrc,
        l_errtxt      TYPE string.
  DATA: l_fullpath  TYPE string,
        l_extension TYPE string,
        l_fname     LIKE rlgrap-filename,
        l_formkey   LIKE  wwwdatatab.

  l_extension = p_objid.
  PERFORM get_file_name USING '.XLSX'
                              'EXCEL|*.XLSX;'
                     CHANGING l_fullpath.
  IF l_fullpath = space.
    MESSAGE 'Please specified download file name'(044) TYPE 'I'.
  ELSE.

    CONCATENATE l_fullpath '' INTO l_fname.
    SELECT SINGLE relid objid
      FROM wwwdata
      INTO CORRESPONDING FIELDS OF l_objdata
      WHERE relid = 'MI'
        AND objid = p_objid .
    IF sy-subrc NE 0 OR l_objdata-objid = space.
      MESSAGE 'Documend not found！'(045) TYPE 'W'.
    ELSE.
      CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
        EXPORTING
          key         = l_objdata
          destination = l_fname
        IMPORTING
          rc          = l_rc
        CHANGING
          temp        = l_fname.
      IF l_rc NE 0.
        MESSAGE 'Download Fail !' TYPE 'W'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "download_template
*&---------------------------------------------------------------------*
*& Form get_file_name
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      <-- L_FULLPATH
*&---------------------------------------------------------------------*
FORM get_file_name USING    p_extension
                            p_file_filter
                   CHANGING p_fullpath.

  DATA: l_filename TYPE string,
        l_path     TYPE string,
        l_fullpath TYPE string,
        l_titile   TYPE string,
        l_init_dir TYPE string.

  DATA: default_file_name TYPE string.

  CLEAR p_fullpath.

  CALL FUNCTION 'ZF_GET_APDATA_PATH'
    IMPORTING
      e_path = default_file_name.

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      CONCATENATE default_file_name '\Packing Stardand template' INTO default_file_name.
    WHEN 'FC02'.
      CONCATENATE default_file_name '\Pack Material template' INTO default_file_name.
    WHEN 'FC03'.
      CONCATENATE default_file_name '\MaxQty & KoreaLbl template' INTO default_file_name. "add on 2023/02/09
  ENDCASE.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension    = '.XLSX'
      prompt_on_overwrite  = 'X'
      file_filter          = p_file_filter
      default_file_name    = default_file_name
    CHANGING
      filename             = l_filename
      path                 = l_path
      fullpath             = l_fullpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_fullpath = l_fullpath.

ENDFORM.                    "get_file_name
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_excel .

  FIELD-SYMBOLS:
    <fs1>,<fs2>,<fs3>.

  FIELD-SYMBOLS: <f> TYPE ANY.
  DATA l_field_type(1) TYPE c.

  DATA:
    ls_excel       TYPE ty_excel,
    lt_excel       TYPE tt_excel.

  DATA:it_raw TYPE truxs_t_text_data.
  DATA: l_filename  LIKE  rlgrap-filename.

  DATA:l_zzpackmatno(30)  TYPE c,
       l_length           TYPE i.

  "initial
  REFRESH: gt_data,
           gt_data2,
           gt_data3.


  CASE g_mode.
    WHEN 'ZTSD0018'.
      l_filename = p_file.
    WHEN 'ZTSD0019'.
      l_filename = p_file2.
    WHEN 'ZTSD0187'.
      l_filename = p_file3.
  ENDCASE.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_tab_raw_data       = it_raw
      i_filename           = l_filename
    TABLES
      i_tab_converted_data = lt_excel
    EXCEPTIONS
      OTHERS               = 1.
  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH 'File upload with error'.
  ENDIF.

*<< Delete header line & ALV序列碼起始號
  CASE g_mode.
    WHEN 'ZTSD0018'.
      DELETE lt_excel FROM 1 TO 7.
      g_tabix = 7.
    WHEN 'ZTSD0019'.
      DELETE lt_excel FROM 1 TO 7.
      g_tabix = 7.
    WHEN 'ZTSD0187'.
      DELETE lt_excel FROM 1 TO 3.
      g_tabix = 3.
  ENDCASE.

*<< Move value
  CASE g_mode.
    WHEN 'ZTSD0018'.
      LOOP AT lt_excel INTO ls_excel.
        CLEAR gs_data.

        gs_data-zzvkorg = ls_excel-f001.
        gs_data-zzpartno = ls_excel-f002.
        gs_data-zzshape = ls_excel-f003.
        gs_data-zzproductno = ls_excel-f004.
        "當開頭是’MRB1’的話，要先剔除這四個字元
        l_zzpackmatno = ls_excel-f005.
        IF l_zzpackmatno+0(4) = 'MRB1'.
          l_length = STRLEN( l_zzpackmatno ).
          l_zzpackmatno = l_zzpackmatno+4(l_length).
        ENDIF.
        gs_data-zzpackmatno = l_zzpackmatno.
        gs_data-zzgccode = ls_excel-f006.
        gs_data-cust_no = ls_excel-f007.
        IF gs_data-cust_no+0(1) CO '0123456789'.
          conversion_exit_alpha_input gs_data-cust_no.
        ENDIF.
        gs_data-zzpackmatnocat = ls_excel-f008.
        gs_data-zzmeasure = ls_excel-f009.
        gs_data-by_pallet = ls_excel-f010.
        gs_data-zzboxqty = ls_excel-f011.
        gs_data-zzcapacityrate = ls_excel-f012.
        gs_data-zztotalpackqty = ls_excel-f013.
        gs_data-ntgew = ls_excel-f014.
        gs_data-brgew = ls_excel-f015.
        gs_data-zzmaxpackqty = ls_excel-f016.   "add on 2023/02/21
        gs_data-zzalumbag = ls_excel-f017.   "add on 2023/02/21

        APPEND gs_data TO gt_data.
      ENDLOOP.

      IF gt_data IS INITIAL.
        MESSAGE text-e01 TYPE 'E'.
      ENDIF.

    WHEN 'ZTSD0019'.
      LOOP AT lt_excel INTO ls_excel.
        CLEAR gs_data2.

        l_zzpackmatno = ls_excel-f001.
        IF l_zzpackmatno+0(4) = 'MRB1'.
          l_length = STRLEN( l_zzpackmatno ).
          l_zzpackmatno = l_zzpackmatno+4(l_length).
        ENDIF.
        gs_data2-zzpackmatno = l_zzpackmatno.
        gs_data2-zzpackmatnocat = ls_excel-f002.
        gs_data2-zzmeasure = ls_excel-f003.
        gs_data2-ntgew = ls_excel-f004.
        gs_data2-brgew = ls_excel-f005.

        APPEND gs_data2 TO gt_data2.
      ENDLOOP.

      IF gt_data2 IS INITIAL.
        MESSAGE text-e01 TYPE 'E'.
      ENDIF.
*<< Begin add 2023/02/09 ***************************
    WHEN 'ZTSD0187'.
      LOOP AT lt_excel INTO ls_excel.
        CLEAR gs_data3.

        gs_data3-zzgccode = ls_excel-f001.
        gs_data3-zzpartno = ls_excel-f002.
        gs_data3-zzproductno = ls_excel-f003.
        gs_data3-zzshape = ls_excel-f004.
        gs_data3-zzgrade = ls_excel-f005.
        gs_data3-matnr = ls_excel-f006.
        gs_data3-zzmaxpackcapa = ls_excel-f007.
        gs_data3-zzrecyclelabelno = ls_excel-f008.
        gs_data3-ernam = ls_excel-f009.
        gs_data3-erdat = ls_excel-f010.
        gs_data3-erzet = ls_excel-f011.

        APPEND gs_data3 TO gt_data3.
      ENDLOOP.

      IF gt_data3 IS INITIAL.
        MESSAGE text-e01 TYPE 'E'.
      ENDIF.
*<< End   add 2023/02/09 ***************************

  ENDCASE.

ENDFORM.                    " UPLOAD_EXCEL
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data .

  CASE g_mode.
    WHEN 'ZTSD0018'.
      PERFORM check_data_ztsd0018.
    WHEN 'ZTSD0019'.
      PERFORM check_data_ztsd0019.
    WHEN 'ZTSD0187'.
      PERFORM check_data_ztsd0187.
  ENDCASE.

ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  MANDATORY_FIELD_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0547   text
*----------------------------------------------------------------------*
FORM mandatory_field_check  USING  p_fieldname TYPE fieldname.

  FIELD-SYMBOLS: <value> TYPE ANY.
  FIELD-SYMBOLS: <fs_struc> TYPE ANY.
  DATA: lv_structure_name TYPE text30.

  "init
  UNASSIGN: <value>,
            <fs_struc>.

  TRANSLATE p_fieldname TO UPPER CASE.

  CASE g_mode.
    WHEN 'ZTSD0018'.
      lv_structure_name = 'GS_DATA'.
    WHEN 'ZTSD0019'.
      lv_structure_name = 'GS_DATA2'.
    WHEN 'ZTSD0187'.
      lv_structure_name = 'GS_DATA3'.
  ENDCASE.

  ASSIGN (lv_structure_name) TO <fs_struc>.
  ASSIGN COMPONENT p_fieldname OF STRUCTURE <fs_struc> TO <value>.

  CHECK <value> IS ASSIGNED.

  IF <value> IS INITIAL.
    g_error = 'X'.

    CASE g_mode.
      WHEN 'ZTSD0018'.
        APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
        MOVE-CORRESPONDING gs_data TO <alv>.
        <alv>-status = gv_led_red.
        <alv>-seq = g_tabix.
        <alv>-msg = text-e03.
        REPLACE ALL OCCURRENCES OF REGEX '&1' IN <alv>-msg WITH p_fieldname.
      WHEN 'ZTSD0019'.
        APPEND INITIAL LINE TO gt_alv3 ASSIGNING <alv3>.
        MOVE-CORRESPONDING gs_data2 TO <alv3>.
        <alv3>-status = gv_led_red.
        <alv3>-seq = g_tabix.
        <alv3>-msg = text-e03.
        REPLACE ALL OCCURRENCES OF REGEX '&1' IN <alv3>-msg WITH p_fieldname.

*<< Begin add 2023/02/09 ************************************************************
      WHEN 'ZTSD0187'.
        APPEND INITIAL LINE TO gt_alv5 ASSIGNING <alv5>.
        MOVE-CORRESPONDING gs_data3 TO <alv5>.
        <alv5>-status = gv_led_red.
        <alv5>-seq = g_tabix.
        <alv5>-msg = text-e03.
        REPLACE ALL OCCURRENCES OF REGEX '&1' IN <alv5>-msg WITH p_fieldname.
*<< End   add 2023/02/09 ************************************************************
    ENDCASE.

  ENDIF.

ENDFORM.                    " MANDATORY_FIELD_CHECK
*&---------------------------------------------------------------------*
*&      Form  SET_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_range .

  REFRESH: s_zzpackmatno[],
           s_zzgccode[],
           s_cust_no[].

  APPEND INITIAL LINE TO s_zzpackmatno ASSIGNING <rs_zzpackmatno>.
  <rs_zzpackmatno>-sign = 'I'.
  <rs_zzpackmatno>-option = 'EQ'.
  <rs_zzpackmatno>-low = gs_data-zzpackmatno.

  APPEND INITIAL LINE TO s_zzgccode ASSIGNING <rs_zzgccode>.
  <rs_zzgccode>-sign = 'I'.
  <rs_zzgccode>-option = 'EQ'.
  <rs_zzgccode>-low = gs_data-zzgccode.

  APPEND INITIAL LINE TO s_cust_no ASSIGNING <rs_cust_no>.
  <rs_cust_no>-sign = 'I'.
  <rs_cust_no>-option = 'EQ'.
  <rs_cust_no>-low = gs_data-cust_no.

ENDFORM.                    " SET_RANGE
*&---------------------------------------------------------------------*
*&      Form  CHECK_FIELD_I
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_field_i .

*	1.如果G欄有值，用F欄和G欄搜尋出H欄Category(TOB/TOC/TOD)，同時判斷若該F欄(ZTSD0018-ZZGCCODE)+G欄(ZTSD0018-CUST_NO)的組合也有存在ZTSD0018時，
*   也需要抓出ZTSD0018-ZZPACKMATNOCAT來一併考慮，將excel和ZTSD0018的資料均撈出後要檢查其最大的Category需有Measure，
*   如果沒有資料的話輸出: Please fill in the value of Measure

*	2.如果G欄無值，用F欄搜尋出H欄Category(TOB/TOC/TOD)，同時判斷若該F欄(ZTSD0018-ZZGCCODE) 也有存在ZTSD0018時，
*   也需要抓出ZTSD0018-ZZPACKMATNOCAT來一併考慮，將excel和ZTSD0018的資料均撈出後要檢查其最大的Category需有Measure，
*   如果沒有資料的話輸出: Please fill in the value of Measure

*	3.欄位內容的X必須都為大寫:  Measure must use uppercase ‘X’

  TYPES: BEGIN OF ty_check,
            zzgccode        TYPE zgccode,
            cust_no         TYPE zcust_no,
            zzpackmatnocat  TYPE zzpackmatnocat,
            zzmeasure       TYPE zzmeasure,
         END OF ty_check.
  DATA: lt_check    TYPE TABLE OF ty_check,
        ls_check    TYPE ty_check.
  DATA: ls_data     TYPE ty_data.

* init
  REFRESH: lt_check[].

*--------------------------------------------------------------------*
* G欄有值
*--------------------------------------------------------------------*
  IF gs_data-cust_no IS NOT INITIAL.
    LOOP AT gt_data INTO ls_data WHERE zzgccode = gs_data-zzgccode
                                   AND cust_no = gs_data-cust_no.

      CLEAR ls_check.
      MOVE-CORRESPONDING ls_data TO ls_check.
      APPEND ls_check TO lt_check.
    ENDLOOP.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_check
      FROM ztsd0018
      WHERE zzgccode = gs_data-zzgccode
        AND cust_no = gs_data-cust_no.

*--------------------------------------------------------------------*
* G欄無值
*--------------------------------------------------------------------*
  ELSE.
    LOOP AT gt_data INTO ls_data WHERE zzgccode = gs_data-zzgccode.
      CLEAR ls_check.
      MOVE-CORRESPONDING ls_data TO ls_check.
      APPEND ls_check TO lt_check.
    ENDLOOP.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_check
      FROM ztsd0018
      WHERE zzgccode = gs_data-zzgccode.

  ENDIF.

* Sorting...
  SORT lt_check BY zzpackmatnocat DESCENDING. "Sort by -> D/C/B

* Filter Duplicates
  DELETE ADJACENT DUPLICATES FROM lt_check COMPARING ALL FIELDS.

  READ TABLE lt_check INTO ls_check WITH KEY zzgccode = gs_data-zzgccode
                                             cust_no  = gs_data-cust_no
                                             zzpackmatnocat = gs_data-zzpackmatnocat.

  IF ( sy-subrc = 0 AND sy-tabix = 1 ). "當前檢查的資料有讀取到，且為最大的 Category
    IF ls_check-zzmeasure IS INITIAL.
      g_error = 'X'.
      APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
      MOVE-CORRESPONDING gs_data TO <alv>.
      <alv>-status = gv_led_red.
      <alv>-seq = g_tabix.
      <alv>-msg = text-e10.
    ENDIF.
  ENDIF.


* I欄位內容的X必須都為大寫:  Measure must use uppercase ‘X’
  FIND '*' IN gs_data-zzmeasure.
  IF sy-subrc = 0.
    g_error = 'X'.
    APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
    MOVE-CORRESPONDING gs_data TO <alv>.
    <alv>-status = gv_led_red.
    <alv>-seq = g_tabix.
    <alv>-msg = text-e11.
  ENDIF.

  FIND 'x' IN gs_data-zzmeasure.
  IF sy-subrc = 0.
    g_error = 'X'.
    APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
    MOVE-CORRESPONDING gs_data TO <alv>.
    <alv>-status = gv_led_red.
    <alv>-seq = g_tabix.
    <alv>-msg = text-e11.
  ENDIF.

ENDFORM.                    " CHECK_FIELD_I
*&---------------------------------------------------------------------*
*&      Form  CHECK_FIELD_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_field_l .

* 1. 不可以有小數點: Capacity Rate can’t have a decimal point
* 2. 用F欄搜尋出H欄Category(TOB/TOC/TOD)，如果有TOB的話要進一步判斷其TOB的Capacity Rate與TOC或TOD的有無相同，
*    如果TOB的上層是TOC的話就與TOC比對，
*    如果TOB的上層是TOD的話就與TOD比對，假使比對值相同就要報錯: Capacity Rate must different with TOB and TOC/TOD

  TYPES: BEGIN OF ty_check,
            zzgccode           TYPE zgccode,
            zzpackmatnocat     TYPE zzpackmatnocat,
            zzcapacityrate(13) TYPE p DECIMALS 1,
         END OF ty_check.
  DATA: lt_check TYPE TABLE OF ty_check,
        ls_check TYPE ty_check.
  DATA: ls_data  TYPE ty_data.

  DATA: l_str         TYPE string,
        l_integer(13) TYPE c,
        l_decimal(1)  TYPE c.

  "init
  REFRESH lt_check[].

  CHECK gs_data-zzcapacityrate IS NOT INITIAL.

  MOVE gs_data-zzcapacityrate TO l_str.

  SPLIT l_str AT '.' INTO: l_integer
                           l_decimal.

  IF l_decimal NE ' '.
    IF l_decimal NE '0'.
      g_error = 'X'.
      APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
      MOVE-CORRESPONDING gs_data TO <alv>.
      <alv>-status = gv_led_red.
      <alv>-seq = g_tabix.
      <alv>-msg = text-e12.
    ENDIF.
  ENDIF.

*<< Remark Below **********************************************
*  CHECK gs_data-zzpackmatnocat = 'B'.
*
*  LOOP AT gt_data INTO ls_data WHERE zzgccode = gs_data-zzgccode.
*    CLEAR ls_check.
*    MOVE-CORRESPONDING ls_data TO ls_check.
*    APPEND ls_check TO lt_check.
*  ENDLOOP.
*
** Sorting...
*  SORT lt_check BY zzpackmatnocat ASCENDING. "Sort by -> B/C/D
*
** Filter Duplicates
*  DELETE ADJACENT DUPLICATES FROM lt_check COMPARING zzpackmatnocat.
*
*  LOOP AT lt_check INTO ls_check.
*
*    IF ls_check-zzpackmatnocat = 'B'.
*      CONTINUE.
*    ENDIF.
*
*    IF ls_check-zzpackmatnocat = 'C'.
*
*      IF ls_check-zzcapacityrate = gs_data-zzcapacityrate.
*        g_error = 'X'.
*        APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
*        MOVE-CORRESPONDING gs_data TO <alv>.
*        <alv>-status = gv_led_red.
*        <alv>-seq = g_tabix.
*        <alv>-msg = text-e13.
*      ENDIF.
*
*      EXIT.
*    ENDIF.
*
*    IF ls_check-zzpackmatnocat = 'D'.
*
*      IF ls_check-zzcapacityrate = gs_data-zzcapacityrate.
*        g_error = 'X'.
*        APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
*        MOVE-CORRESPONDING gs_data TO <alv>.
*        <alv>-status = gv_led_red.
*        <alv>-seq = g_tabix.
*        <alv>-msg = text-e13.
*      ENDIF.
*
*      EXIT.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " CHECK_FIELD_L
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_alv USING tname.

  PERFORM init.
  PERFORM build_layout USING tname.
  PERFORM build_fieldcat USING tname.

  CASE tname.
    WHEN 'ZTSD0018'.
      CASE 'X'.
        WHEN r1.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_callback_program       = sy-repid
              i_callback_pf_status_set = 'MENU_SET'
              i_callback_user_command  = 'USER_COMMAND_EVENT'
              is_layout                = gs_layout
              it_fieldcat              = gt_fieldcat
            TABLES
              t_outtab                 = gt_alv
            EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.
        WHEN r2.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_callback_program       = sy-repid
              i_callback_pf_status_set = 'MENU_SET'
              i_callback_user_command  = 'USER_COMMAND_EVENT'
              is_layout                = gs_layout
              it_fieldcat              = gt_fieldcat
            TABLES
              t_outtab                 = gt_alv2
            EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.
      ENDCASE.

    WHEN 'ZTSD0019'.
      CASE 'X'.
        WHEN r3.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_callback_program       = sy-repid
              i_callback_pf_status_set = 'MENU_SET'
              i_callback_user_command  = 'USER_COMMAND_EVENT'
              is_layout                = gs_layout
              it_fieldcat              = gt_fieldcat
            TABLES
              t_outtab                 = gt_alv3
            EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.
        WHEN r4.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_callback_program       = sy-repid
              i_callback_pf_status_set = 'MENU_SET'
              i_callback_user_command  = 'USER_COMMAND_EVENT'
              is_layout                = gs_layout
              it_fieldcat              = gt_fieldcat
            TABLES
              t_outtab                 = gt_alv4
            EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.
      ENDCASE.
*<< Begin add 2023/02/09 ********************************************************
    WHEN 'ZTSD0187'.
      CASE 'X'.
        WHEN r5.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_callback_program       = sy-repid
              i_callback_pf_status_set = 'MENU_SET'
              i_callback_user_command  = 'USER_COMMAND_EVENT'
              is_layout                = gs_layout
              it_fieldcat              = gt_fieldcat
            TABLES
              t_outtab                 = gt_alv5
            EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.
        WHEN r6.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_callback_program       = sy-repid
              i_callback_pf_status_set = 'MENU_SET'
              i_callback_user_command  = 'USER_COMMAND_EVENT'
              is_layout                = gs_layout
              it_fieldcat              = gt_fieldcat
            TABLES
              t_outtab                 = gt_alv6
            EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.
      ENDCASE.
*<< End   add 2023/02/09 ********************************************************


  ENDCASE.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " SHOW_ALV
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .

  REFRESH gt_fieldcat[].
  CLEAR gs_layout.

  IF <g_table> IS ASSIGNED.
    FREE <g_table>.
  ENDIF.

ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout USING tname.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-detail_initial_lines = 'X'.
  gs_layout-zebra = 'X'.

  CASE tname.
    WHEN 'ZTSD0018'.
      CASE 'X'.
        WHEN r1.
          gs_layout-window_titlebar = 'Upload Packing Standard'.
        WHEN r2.
          gs_layout-window_titlebar = 'Query Packing Standard'.
      ENDCASE.
    WHEN 'ZTSD0019'.
      CASE 'X'.
        WHEN r3.
          gs_layout-window_titlebar = 'Upload Pack Material'.
        WHEN r4.
          gs_layout-window_titlebar = 'Query Pack Material'.
      ENDCASE.
    WHEN 'ZTSD0187'.
      CASE 'X'.
        WHEN r5.
          gs_layout-window_titlebar = 'Upload MaxQty & Korea Label'.
        WHEN r6.
          gs_layout-window_titlebar = 'Query MaxQty & Korea Label'.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat USING tname.

  FIELD-SYMBOLS: <fieldcat> LIKE LINE OF gt_fieldcat.
  DATA: structure_name  LIKE  dd02l-tabname.

  structure_name = tname.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = structure_name
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CASE tname.
    WHEN 'ZTSD0018'.
      CASE 'X'.
        WHEN r1.
          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 1.
          <fieldcat>-fieldname = 'CHECKBOX'.
          <fieldcat>-seltext_l = 'UPD'.
          <fieldcat>-checkbox = 'X'.
          <fieldcat>-edit = 'X'.

          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 2.
          <fieldcat>-fieldname = 'STATUS'.
          <fieldcat>-seltext_l = 'Status'.

          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 3.
          <fieldcat>-fieldname = 'SEQ'.
          <fieldcat>-seltext_l = 'Seq'.

          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 4.
          <fieldcat>-fieldname = 'MSG'.
          <fieldcat>-seltext_l = 'Message'.

        WHEN r2.
          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 1.
          <fieldcat>-fieldname = 'CHECKBOX'.
          <fieldcat>-seltext_l = 'DEL'.
          <fieldcat>-checkbox = 'X'.
          <fieldcat>-edit = 'X'.

      ENDCASE.

    WHEN 'ZTSD0019'.
      CASE 'X'.
        WHEN r3.
          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 1.
          <fieldcat>-fieldname = 'CHECKBOX'.
          <fieldcat>-seltext_l = 'UPD'.
          <fieldcat>-checkbox = 'X'.
          <fieldcat>-edit = 'X'.

          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 2.
          <fieldcat>-fieldname = 'STATUS'.
          <fieldcat>-seltext_l = 'Status'.

          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 3.
          <fieldcat>-fieldname = 'SEQ'.
          <fieldcat>-seltext_l = 'Seq'.

          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 4.
          <fieldcat>-fieldname = 'MSG'.
          <fieldcat>-seltext_l = 'Message'.

        WHEN r4.
          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 1.
          <fieldcat>-fieldname = 'CHECKBOX'.
          <fieldcat>-seltext_l = 'DEL'.
          <fieldcat>-checkbox = 'X'.
          <fieldcat>-edit = 'X'.
      ENDCASE.

*<< Begin add 2023/02/09 ********************************************************
    WHEN 'ZTSD0187'.
      CASE 'X'.
        WHEN r5.
          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 1.
          <fieldcat>-fieldname = 'CHECKBOX'.
          <fieldcat>-seltext_l = 'UPD'.
          <fieldcat>-checkbox = 'X'.
          <fieldcat>-edit = 'X'.

          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 2.
          <fieldcat>-fieldname = 'STATUS'.
          <fieldcat>-seltext_l = 'Status'.

          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 3.
          <fieldcat>-fieldname = 'SEQ'.
          <fieldcat>-seltext_l = 'Seq'.

          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 4.
          <fieldcat>-fieldname = 'MSG'.
          <fieldcat>-seltext_l = 'Message'.

        WHEN r6.
          INSERT INITIAL LINE INTO gt_fieldcat ASSIGNING <fieldcat> INDEX 1.
          <fieldcat>-fieldname = 'CHECKBOX'.
          <fieldcat>-seltext_l = 'DEL'.
          <fieldcat>-checkbox = 'X'.
          <fieldcat>-edit = 'X'.
      ENDCASE.
*<< End   add 2023/02/09 ********************************************************

  ENDCASE.

  LOOP AT gt_fieldcat ASSIGNING <fieldcat>.
    <fieldcat>-ddictxt = 'S'. "short description

*<< Begin add 2023/02/09 **************************************************************************************
    IF ( tname = 'ZTSD0018' OR tname = 'ZTSD0019' ).

      CASE <fieldcat>-fieldname.
        WHEN 'ZZVKORG' OR 'ZZPARTNO' OR 'ZZSHAPE' OR 'ZZPRODUCTNO' OR 'ZZPACKMATNO' OR 'ZZGCCODE' OR 'CUST_NO'.
          <fieldcat>-key = abap_true.
        WHEN 'MSG'.
          <fieldcat>-lowercase = abap_true.
      ENDCASE.

    ELSEIF tname = 'ZTSD0187'.

      CASE <fieldcat>-fieldname.
        WHEN 'ZZGCCODE' OR 'ZZPARTNO' OR 'ZZPRODUCTNO' OR 'ZZSHAPE' OR 'ZZGRADE' OR 'MATNR'.
          <fieldcat>-key = abap_true.
        WHEN 'MSG'.
          <fieldcat>-lowercase = abap_true.
      ENDCASE.
*<< End   add 2023/02/09 **************************************************************************************

    ENDIF.

  ENDLOOP.


ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  menu_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RE_EXTAB   text
*----------------------------------------------------------------------*
FORM menu_set USING re_extab TYPE slis_t_extab.

  DATA: lt_ucomm  TYPE TABLE OF sy-ucomm,
        ls_ucomm  LIKE LINE OF  lt_ucomm.

  REFRESH lt_ucomm.
  ls_ucomm = '&SUM'.
  APPEND ls_ucomm TO lt_ucomm.
  ls_ucomm = '&UMC'.
  APPEND ls_ucomm TO lt_ucomm.


  CASE 'X'.
    WHEN p_r1.

      CASE 'X'.
        WHEN r1. "Upload

          ls_ucomm = 'ZDEL'.
          APPEND ls_ucomm TO lt_ucomm.
          ls_ucomm = 'ZDOWNLOAD'.
          APPEND ls_ucomm TO lt_ucomm.

        WHEN r2. "Query

          ls_ucomm = 'ZUPD'.
          APPEND ls_ucomm TO lt_ucomm.

      ENDCASE.

    WHEN p_r2.

      CASE 'X'.
        WHEN r3. "Upload

          ls_ucomm = 'ZDEL'.
          APPEND ls_ucomm TO lt_ucomm.
          ls_ucomm = 'ZDOWNLOAD'.
          APPEND ls_ucomm TO lt_ucomm.

        WHEN r4. "Query

          ls_ucomm = 'ZUPD'.
          APPEND ls_ucomm TO lt_ucomm.

      ENDCASE.

*<< Begin add 2023/02/09 ********************************************************
    WHEN p_r3.
      CASE 'X'.
        WHEN r5. "Upload

          ls_ucomm = 'ZDEL'.
          APPEND ls_ucomm TO lt_ucomm.
          ls_ucomm = 'ZDOWNLOAD'.
          APPEND ls_ucomm TO lt_ucomm.

        WHEN r6. "Query

          ls_ucomm = 'ZUPD'.
          APPEND ls_ucomm TO lt_ucomm.
      ENDCASE.
*<< End   add 2023/02/09 ********************************************************

  ENDCASE.

  SET PF-STATUS 'STANDARD' EXCLUDING lt_ucomm. "Setting ALV Toolbar

ENDFORM.                    "menu_set
*&---------------------------------------------------------------------*
*&      Form  user_command_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command_event USING r_ucomm     LIKE sy-ucomm
                              rs_selfield TYPE slis_selfield.

  DATA: ref1      TYPE REF TO cl_gui_alv_grid,
        l_result  TYPE i.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.

  CALL METHOD ref1->check_changed_data.

  CASE r_ucomm.
    WHEN '&IC1'.

    WHEN '&AL'.

      CASE 'X'.
        WHEN p_r1.
          CASE 'X'.
            WHEN r1. "Upload

              LOOP AT gt_alv ASSIGNING <alv>.
                <alv>-checkbox = 'X'.
              ENDLOOP.

            WHEN r2. "Query

              LOOP AT gt_alv2 ASSIGNING <alv2>.
                <alv2>-checkbox = 'X'.
              ENDLOOP.

          ENDCASE.
        WHEN p_r2.

          CASE 'X'.
            WHEN r3. "Upload

              LOOP AT gt_alv3 ASSIGNING <alv3>.
                <alv3>-checkbox = 'X'.
              ENDLOOP.

            WHEN r4. "Query

              LOOP AT gt_alv4 ASSIGNING <alv4>.
                <alv4>-checkbox = 'X'.
              ENDLOOP.

          ENDCASE.
*<< Begin add 2023/02/09 ********************************************************
        WHEN p_r3.
          CASE 'X'.
            WHEN r5. "Upload

              LOOP AT gt_alv5 ASSIGNING <alv5>.
                <alv5>-checkbox = 'X'.
              ENDLOOP.

            WHEN r6. "Query

              LOOP AT gt_alv6 ASSIGNING <alv6>.
                <alv6>-checkbox = 'X'.
              ENDLOOP.

          ENDCASE.
*<< End   add 2023/02/09 ********************************************************
      ENDCASE.

    WHEN '&DAL'.

      CASE 'X'.
        WHEN p_r1.
          CASE 'X'.
            WHEN r1. "Upload

              LOOP AT gt_alv ASSIGNING <alv>.
                <alv>-checkbox = ''.
              ENDLOOP.

            WHEN r2. "Query

              LOOP AT gt_alv2 ASSIGNING <alv2>.
                <alv2>-checkbox = ''.
              ENDLOOP.

          ENDCASE.
        WHEN p_r2.
          CASE 'X'.
            WHEN r3. "Upload

              LOOP AT gt_alv3 ASSIGNING <alv3>.
                <alv3>-checkbox = ''.
              ENDLOOP.
            WHEN r4. "Query

              LOOP AT gt_alv4 ASSIGNING <alv4>.
                <alv4>-checkbox = ''.
              ENDLOOP.

          ENDCASE.
*<< Begin add 2023/02/09 ********************************************************
        WHEN p_r3.
          CASE 'X'.
            WHEN r5. "Upload

              LOOP AT gt_alv5 ASSIGNING <alv5>.
                <alv5>-checkbox = ''.
              ENDLOOP.

            WHEN r6. "Query

              LOOP AT gt_alv6 ASSIGNING <alv6>.
                <alv6>-checkbox = ''.
              ENDLOOP.

          ENDCASE.
*<< End   add 2023/02/09 ********************************************************
      ENDCASE.


    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'ZUPD'.
      PERFORM zupdate.

    WHEN 'ZDEL'.
      PERFORM zdelete USING g_mode.

    WHEN 'ZDOWNLOAD'.
      PERFORM zdownload.

  ENDCASE.

  rs_selfield-refresh = 'X'.

ENDFORM.                    "user_command_event
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_match .

*1  如果G欄有值，用F欄和G欄搜尋出H欄Category(ZTSD0018-ZZPACKMATNOCAT)，
*    同時用F欄=ZTSD0087-ZENDCUSTOMER和G欄=ZTSD0087-ZZGCCODE搜尋出ZTSD0087-TYPE
*2  如果G欄無值，用F欄搜尋出H欄Category(ZTSD0018-ZZPACKMATNOCAT)，同時用F欄=ZTSD0087-ZENDCUSTOMER搜尋出ZTSD0087-TYPE：
*  在去除ZTSD0087-TYPE=A的情況下，要比對ZTSD0018-ZZPACKMATNOCAT和ZTSD0087-TYPE有無一致

  TYPES: BEGIN OF ty_category,
          zzpackmatnocat TYPE zzpackmatnocat,
         END OF ty_category,

         BEGIN OF ty_type,
           type TYPE ztype5,
         END OF ty_type.

  DATA: lt_category TYPE TABLE OF ty_category,
        ls_category TYPE ty_category,
        lt_type     TYPE TABLE OF ty_type,
        ls_type     TYPE ty_type.

  DATA: ls_data TYPE ty_data.
  DATA: l_str1 TYPE string,
        l_str2 TYPE string.

  "init
  REFRESH: lt_category[],
           lt_type[].
  CLEAR: l_str1,
         l_str2.

*--------------------------------------------------------------------*
* G欄有值
*--------------------------------------------------------------------*
  IF gs_data-cust_no IS NOT INITIAL.

    LOOP AT gt_data INTO ls_data WHERE zzgccode = gs_data-zzgccode
                                   AND cust_no = gs_data-cust_no.

      CLEAR ls_category.
      ls_category-zzpackmatnocat = ls_data-zzpackmatnocat.
      APPEND ls_category TO lt_category.
    ENDLOOP.

    SELECT zzpackmatnocat
      APPENDING CORRESPONDING FIELDS OF TABLE lt_category
      FROM ztsd0018
      WHERE zzgccode = gs_data-zzgccode
      AND   cust_no = gs_data-cust_no.

    SELECT type
      INTO CORRESPONDING FIELDS OF TABLE lt_type
      FROM ztsd0087
      WHERE zzgccode = gs_data-zzgccode
      AND   zendcustomer = gs_data-cust_no.

  ELSE.

    LOOP AT gt_data INTO ls_data WHERE zzgccode = gs_data-zzgccode.

      CLEAR ls_category.
      ls_category-zzpackmatnocat = ls_data-zzpackmatnocat.
      APPEND ls_category TO lt_category.
    ENDLOOP.

    SELECT zzpackmatnocat
      APPENDING CORRESPONDING FIELDS OF TABLE lt_category
      FROM ztsd0018
      WHERE zzgccode = gs_data-zzgccode.

    SELECT type
      INTO CORRESPONDING FIELDS OF TABLE lt_type
      FROM ztsd0087
      WHERE zzgccode = gs_data-zzgccode.

  ENDIF.

  SORT lt_category BY zzpackmatnocat ASCENDING. "B/C/D
  DELETE ADJACENT DUPLICATES FROM lt_category COMPARING ALL FIELDS.
  LOOP AT lt_category INTO ls_category.
    CONCATENATE l_str1 ls_category-zzpackmatnocat INTO l_str1.
    SHIFT l_str1 LEFT DELETING LEADING space.
  ENDLOOP.


  SORT lt_type BY type ASCENDING. "A/B/C/D
  DELETE lt_type[] WHERE type = 'A'.
  DELETE ADJACENT DUPLICATES FROM lt_type COMPARING ALL FIELDS.
  LOOP AT lt_type INTO ls_type.
    CONCATENATE l_str2 ls_type-type INTO l_str2.
    SHIFT l_str2 LEFT DELETING LEADING space.
  ENDLOOP.

  IF l_str1 <> l_str2.
    g_error = 'X'.
    APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
    MOVE-CORRESPONDING gs_data TO <alv>.
    <alv>-status = gv_led_red.
    <alv>-seq = g_tabix.
    <alv>-msg = text-e14.
  ENDIF.

ENDFORM.                    " CHECK_MATCH
*&---------------------------------------------------------------------*
*&      Form  ZUPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zupdate .

  DATA: answer(1) TYPE c.
  DATA: l_msg(100) TYPE c.
  DATA: ls_ztsd0018_old TYPE ztsd0018,
        ls_ztsd0187_old TYPE ztsd0187.


  CASE g_mode.
    WHEN 'ZTSD0018'.
      "檢查出有一筆紅燈就跳出error不給執行
      READ TABLE gt_alv TRANSPORTING NO FIELDS WITH KEY status = gv_led_red.
      IF sy-subrc = 0.
        MESSAGE 'Have error Data(RED Light), can not do process' TYPE 'E'.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Update Packing Standard'
          text_question         = 'Are you sure want to execute?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CASE answer.
        WHEN '1'. "yes

          REFRESH: gt_ztsd0018[],
                   gt_ztsd0018_log[].

          LOOP AT gt_alv ASSIGNING <alv>.

            CASE <alv>-status.
*--------------------------------------------------------------------*
* Insert data
*--------------------------------------------------------------------*
              WHEN gv_led_green.

                "<< collect insert data
                CLEAR gs_ztsd0018.
                MOVE-CORRESPONDING <alv> TO gs_ztsd0018.
                WRITE <alv>-zzcapacityrate TO gs_ztsd0018-zzcapacityrate LEFT-JUSTIFIED NO-GROUPING.
                APPEND gs_ztsd0018 TO gt_ztsd0018.

                "<< collect log
                CLEAR gs_ztsd0018_log.
                MOVE-CORRESPONDING <alv> TO gs_ztsd0018_log.
                WRITE <alv>-zzcapacityrate TO gs_ztsd0018_log-zzcapacityrate LEFT-JUSTIFIED NO-GROUPING.

                CALL FUNCTION 'Z_FBC0007'
                  IMPORTING
                    ret_timestamp = g_timestamp.

                gs_ztsd0018_log-trtyp = 'H'. "add
                gs_ztsd0018_log-ernam = sy-uname.
                gs_ztsd0018_log-erdat = sy-datum.
                gs_ztsd0018_log-timestamp = g_timestamp.
                APPEND gs_ztsd0018_log TO gt_ztsd0018_log.

*--------------------------------------------------------------------*
* Update data
*--------------------------------------------------------------------*
              WHEN gv_led_yellow.

                "<< must select it, if no select, then no process.
                CHECK <alv>-checkbox = 'X'.

                "<< collect update data
                CLEAR gs_ztsd0018.
                MOVE-CORRESPONDING <alv> TO gs_ztsd0018.
                WRITE <alv>-zzcapacityrate TO gs_ztsd0018-zzcapacityrate LEFT-JUSTIFIED NO-GROUPING.
                APPEND gs_ztsd0018 TO gt_ztsd0018.

*                "<< get old data
                CLEAR ls_ztsd0018_old.
                SELECT SINGLE * INTO ls_ztsd0018_old FROM ztsd0018 WHERE zzvkorg = <alv>-zzvkorg
                                                                   AND   zzpartno = <alv>-zzpartno
                                                                   AND   zzshape = <alv>-zzshape
                                                                   AND   zzproductno = <alv>-zzproductno
                                                                   AND   zzpackmatno = <alv>-zzpackmatno
                                                                   AND   zzgccode = <alv>-zzgccode
                                                                   AND   cust_no = <alv>-cust_no.
                IF sy-subrc = 0.

                  "<< collect log
                  CLEAR gs_ztsd0018_log.
                  MOVE-CORRESPONDING ls_ztsd0018_old TO gs_ztsd0018_log.

                  CALL FUNCTION 'Z_FBC0007'
                    IMPORTING
                      ret_timestamp = g_timestamp.

                  gs_ztsd0018_log-trtyp = 'V'. "change
                  gs_ztsd0018_log-aenam = sy-uname.
                  gs_ztsd0018_log-aedat = sy-datum.
                  gs_ztsd0018_log-timestamp = g_timestamp.
                  APPEND gs_ztsd0018_log TO gt_ztsd0018_log.

                ENDIF.
              WHEN OTHERS.
            ENDCASE.

          ENDLOOP.

          "Insert or Update data
          IF gt_ztsd0018[] IS NOT INITIAL.
            MODIFY ztsd0018 FROM TABLE gt_ztsd0018.
            IF sy-subrc = 0.
              PERFORM popup_message TABLES gt_ztsd0018[]
                                    USING 'Update Success :'
                                          'S'.
*              MESSAGE 'Update Success!' TYPE 'S'.
            ENDIF.
            REFRESH gt_ztsd0018[].
          ENDIF.

          "Save log
          IF gt_ztsd0018_log[] IS NOT INITIAL.
            MODIFY ztsd0018_log FROM TABLE gt_ztsd0018_log.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
            ENDIF.
            REFRESH gt_ztsd0018_log[].
          ENDIF.

        WHEN '2'. "no
          "do nothing
      ENDCASE.

    WHEN 'ZTSD0019'.

      "檢查出有一筆紅燈就跳出error不給執行
      READ TABLE gt_alv3 TRANSPORTING NO FIELDS WITH KEY status = gv_led_red.
      IF sy-subrc = 0.
        MESSAGE 'Have error Data(RED Light), can not do process' TYPE 'E'.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Update Pack Material'
          text_question         = 'Are you sure want to execute?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


      CASE answer.
        WHEN '1'. "yes

          REFRESH: gt_ztsd0019[].
*                   gt_ztsd0018_log[].

          LOOP AT gt_alv3 ASSIGNING <alv3>.

            CASE <alv3>-status.
*--------------------------------------------------------------------*
* Insert data
*--------------------------------------------------------------------*
              WHEN gv_led_green.

                "<< collect insert data
                CLEAR gs_ztsd0019.
                MOVE-CORRESPONDING <alv3> TO gs_ztsd0019.
                APPEND gs_ztsd0019 TO gt_ztsd0019.

                "<< collect log
*                CLEAR gs_ztsd0019_log.
*                MOVE-CORRESPONDING <alv3> TO gs_ztsd0019_log.
*
*                CALL FUNCTION 'Z_FBC0007'
*                  IMPORTING
*                    ret_timestamp = g_timestamp.
*
*                gs_ztsd0019_log-trtyp = 'H'. "add
*                gs_ztsd0019_log-ernam = sy-uname.
*                gs_ztsd0019_log-erdat = sy-datum.
*                gs_ztsd0019_log-timestamp = g_timestamp.
*                APPEND gs_ztsd0019_log TO gt_ztsd0019_log.

*--------------------------------------------------------------------*
* Update data
*--------------------------------------------------------------------*
              WHEN gv_led_yellow.

                "<< must select it, if no select, then no process.
                CHECK <alv3>-checkbox = 'X'.

                "<< collect update data
                CLEAR gs_ztsd0019.
                MOVE-CORRESPONDING <alv3> TO gs_ztsd0019.
                APPEND gs_ztsd0019 TO gt_ztsd0019.

*                "<< get old data
*                CLEAR ls_ztsd0019_old.
*                SELECT SINGLE * INTO ls_ztsd0019_old FROM ztsd0019 WHERE ZZPACKMATNO = <alv3>-ZZPACKMATNO.
*                IF sy-subrc = 0.
*                  "<< collect log
*                  CLEAR gs_ztsd0019_log.
*                  MOVE-CORRESPONDING ls_ztsd0019_old TO gs_ztsd0019_log.
*
*                  CALL FUNCTION 'Z_FBC0007'
*                    IMPORTING
*                      ret_timestamp = g_timestamp.
*
*                  gs_ztsd0019_log-trtyp = 'V'. "change
*                  gs_ztsd0019_log-aenam = sy-uname.
*                  gs_ztsd0019_log-aedat = sy-datum.
*                  gs_ztsd0019_log-timestamp = g_timestamp.
*                  APPEND gs_ztsd0019_log TO gt_ztsd0019_log.
*
*                ENDIF.
            ENDCASE.

          ENDLOOP.

          "Insert or Update data
          IF gt_ztsd0019[] IS NOT INITIAL.
            MODIFY ztsd0019 FROM TABLE gt_ztsd0019.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
              PERFORM popup_message TABLES gt_ztsd0019[]
                                    USING 'Update Success :'
                                          'S'.
*              MESSAGE 'Update Success!' TYPE 'S'.
            ENDIF.
            REFRESH gt_ztsd0019[].
          ENDIF.

          "Save log
*          IF gt_ztsd0019_log[] IS NOT INITIAL.
*            MODIFY ztsd0019_log FROM TABLE gt_ztsd0019_log.
*            IF sy-subrc = 0.
*              COMMIT WORK AND WAIT.
*            ENDIF.
*            REFRESH gt_ztsd0019_log[].
*          ENDIF.

        WHEN '2'. "no
          "do nothing
      ENDCASE.

*<< Begin add 2023/02/09 **************************************************************
    WHEN 'ZTSD0187'.
      "檢查出有一筆紅燈就跳出error不給執行
      READ TABLE gt_alv5 TRANSPORTING NO FIELDS WITH KEY status = gv_led_red.
      IF sy-subrc = 0.
        MESSAGE 'Have error Data(RED Light), can not do process' TYPE 'E'.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Update MaxQty & Korea Label'
          text_question         = 'Are you sure want to execute?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CASE answer.
        WHEN '1'. "yes

          REFRESH: gt_ztsd0187[],
                   gt_ztsd0187_log[].

          LOOP AT gt_alv5 ASSIGNING <alv5>.

            CASE <alv5>-status.
*--------------------------------------------------------------------*
* Insert data
*--------------------------------------------------------------------*
              WHEN gv_led_green.

                "<< collect insert data
                CLEAR gs_ztsd0187.
                MOVE-CORRESPONDING <alv5> TO gs_ztsd0187.
                APPEND gs_ztsd0187 TO gt_ztsd0187.

                "<< collect log
                CLEAR gs_ztsd0187_log.
                MOVE-CORRESPONDING <alv5> TO gs_ztsd0187_log.

                CALL FUNCTION 'Z_FBC0007'
                  IMPORTING
                    ret_timestamp = g_timestamp.

                gs_ztsd0187_log-trtyp = 'H'. "add
                gs_ztsd0187_log-zernam = sy-uname.
                gs_ztsd0187_log-zerdat = sy-datum.
                gs_ztsd0187_log-timestamp = g_timestamp.

                APPEND gs_ztsd0187_log TO gt_ztsd0187_log.

*--------------------------------------------------------------------*
* Update data
*--------------------------------------------------------------------*
              WHEN gv_led_yellow.

                "<< must select it, if no select, then no process.
                CHECK <alv5>-checkbox = 'X'.

                "<< collect update data
                CLEAR gs_ztsd0187.
                MOVE-CORRESPONDING <alv5> TO gs_ztsd0187.
                APPEND gs_ztsd0187 TO gt_ztsd0187.

                "<< get old data
                CLEAR ls_ztsd0187_old.
                SELECT SINGLE * INTO ls_ztsd0187_old
                  FROM ztsd0187
                  WHERE zzgccode = <alv5>-zzgccode
                  AND   zzpartno = <alv5>-zzpartno
                  AND   zzproductno = <alv5>-zzproductno
                  AND   zzshape = <alv5>-zzshape
                  AND   zzgrade = <alv5>-zzgrade
                  AND   matnr = <alv5>-matnr.
                IF sy-subrc = 0.
                  "<< collect log
                  CLEAR gs_ztsd0187_log.
                  MOVE-CORRESPONDING ls_ztsd0187_old TO gs_ztsd0187_log.

                  SELECT SINGLE zernam
                                zerdat
                    INTO (gs_ztsd0187_log-zernam,
                          gs_ztsd0187_log-zerdat
                          )
                    FROM ztsd0187_log
                    WHERE zzgccode = <alv5>-zzgccode
                    AND   zzpartno = <alv5>-zzpartno
                    AND   zzproductno = <alv5>-zzproductno
                    AND   zzshape = <alv5>-zzshape
                    AND   zzgrade = <alv5>-zzgrade
                    AND   matnr = <alv5>-matnr.

                  CALL FUNCTION 'Z_FBC0007'
                    IMPORTING
                      ret_timestamp = g_timestamp.

                  gs_ztsd0187_log-trtyp = 'V'. "change
                  gs_ztsd0187_log-zaenam = sy-uname.
                  gs_ztsd0187_log-zaedat = sy-datum.
                  gs_ztsd0187_log-timestamp = g_timestamp.

                  APPEND gs_ztsd0187_log TO gt_ztsd0187_log.

                ENDIF.
              WHEN OTHERS.
            ENDCASE.

          ENDLOOP.

          "Insert or Update data
          IF gt_ztsd0187[] IS NOT INITIAL.
            MODIFY ztsd0187 FROM TABLE gt_ztsd0187.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
              PERFORM popup_message TABLES gt_ztsd0187[]
                                    USING 'Update Success :'
                                          'S'.
*              MESSAGE 'Update Success!' TYPE 'S'.
            ENDIF.
            REFRESH gt_ztsd0187[].
          ENDIF.

          "Save log
          IF gt_ztsd0187_log[] IS NOT INITIAL.
            MODIFY ztsd0187_log FROM TABLE gt_ztsd0187_log.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
            ENDIF.
            REFRESH gt_ztsd0187_log[].
          ENDIF.

        WHEN '2'. "no
          "do nothing
      ENDCASE.
*<< End   add 2023/02/09 **************************************************************

  ENDCASE.

ENDFORM.                    " ZUPDATE
*&---------------------------------------------------------------------*
*&      Form  RETRIEVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM retrieve_data USING tname.

  CASE tname.
    WHEN 'ZTSD0018'.
      PERFORM select_ztsd0018.
    WHEN 'ZTSD0019'.
      PERFORM select_ztsd0019.
*<< Begin add 2023/02/09 ***************************
    WHEN 'ZTSD0187'.
      PERFORM select_ztsd0187.
*<< End   add 2023/02/09 ***************************
  ENDCASE.

ENDFORM.                    " RETRIEVE_DATA
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_data .

  CASE 'X'.
    WHEN r1.

      CREATE DATA new_table TYPE TABLE OF ty_alv.
      ASSIGN new_table->* TO <g_table>.
      CREATE DATA new_line LIKE LINE OF <g_table>.
      ASSIGN new_line->* TO <g_line>.

      LOOP AT gt_alv INTO gs_alv.
        MOVE-CORRESPONDING gs_alv TO <g_line>.
        INSERT <g_line> INTO TABLE <g_table>.
        CLEAR : <g_line>.
      ENDLOOP.

    WHEN r2.

      CREATE DATA new_table TYPE TABLE OF ty_alv2.
      ASSIGN new_table->* TO <g_table>.
      CREATE DATA new_line LIKE LINE OF <g_table>.
      ASSIGN new_line->* TO <g_line>.

      LOOP AT gt_alv2 INTO gs_alv2.
        MOVE-CORRESPONDING gs_alv2 TO <g_line>.
        INSERT <g_line> INTO TABLE <g_table>.
        CLEAR : <g_line>.
      ENDLOOP.

  ENDCASE.

ENDFORM.                    " ASSIGN_DATA
*&---------------------------------------------------------------------*
*&      Form  ZDELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zdelete USING tname.

  DATA: lt_ztsd0018_del TYPE TABLE OF ztsd0018,
        ls_ztsd0018_del TYPE ztsd0018,
        lt_ztsd0019_del TYPE TABLE OF ztsd0019,
        ls_ztsd0019_del TYPE ztsd0019,
        lt_ztsd0187_del TYPE TABLE OF ztsd0187,  "add on 2023/02/09
        ls_ztsd0187_del TYPE ztsd0187,           "add on 2023/02/09
        l_tabix         TYPE sy-tabix.

  DATA: answer(1) TYPE c.

  CASE tname.
    WHEN 'ZTSD0018'.

      READ TABLE gt_alv2 TRANSPORTING NO FIELDS WITH KEY checkbox = 'X'.
      IF sy-subrc <> 0.
        MESSAGE 'Please select at least one data' TYPE 'E'.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Delete Packing Standard'
          text_question         = 'Are you sure want to delete?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CASE answer.
        WHEN '1'.

          REFRESH: lt_ztsd0018_del[],
                   gt_ztsd0018_log[].

          LOOP AT gt_alv2 ASSIGNING <alv2> WHERE checkbox = 'X'.

            l_tabix = sy-tabix.

            "collect delele data
            CLEAR ls_ztsd0018_del.
            MOVE-CORRESPONDING <alv2> TO ls_ztsd0018_del.
            APPEND ls_ztsd0018_del TO lt_ztsd0018_del.

            "collect log
            CLEAR gs_ztsd0018_log.
            MOVE-CORRESPONDING <alv2> TO gs_ztsd0018_log.

            CALL FUNCTION 'Z_FBC0007'
              IMPORTING
                ret_timestamp = g_timestamp.

            gs_ztsd0018_log-trtyp = 'L'. "delete
            gs_ztsd0018_log-aenam = sy-uname.
            gs_ztsd0018_log-aedat = sy-datum.
            gs_ztsd0018_log-timestamp = g_timestamp.

            APPEND gs_ztsd0018_log TO gt_ztsd0018_log.

            "delete alv screen data
            DELETE gt_alv2 INDEX l_tabix.
          ENDLOOP.

          IF lt_ztsd0018_del[] IS NOT INITIAL.

            "Delete db
            DELETE ztsd0018 FROM TABLE lt_ztsd0018_del[].

            "Save log
            MODIFY ztsd0018_log FROM TABLE gt_ztsd0018_log.

            COMMIT WORK AND WAIT.
*            MESSAGE 'Delete successfully' TYPE 'S'.
            PERFORM popup_message TABLES lt_ztsd0018_del[]
                                  USING 'Delete successfully :'
                                        'S'.

            REFRESH: lt_ztsd0018_del[],gt_ztsd0018_log[].
          ENDIF.

        WHEN '2'. "no
          "do nothing

      ENDCASE.

    WHEN 'ZTSD0019'.

      READ TABLE gt_alv4 TRANSPORTING NO FIELDS WITH KEY checkbox = 'X'.
      IF sy-subrc <> 0.
        MESSAGE 'Please select at least one data' TYPE 'E'.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Delete Pack Material'
          text_question         = 'Are you sure want to delete?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CASE answer.
        WHEN '1'.

          REFRESH: lt_ztsd0019_del[].
*                   gt_ztsd0019_log[].

          LOOP AT gt_alv4 ASSIGNING <alv4> WHERE checkbox = 'X'.

            l_tabix = sy-tabix.

            "collect delele data
            CLEAR ls_ztsd0019_del.
            MOVE-CORRESPONDING <alv4> TO ls_ztsd0019_del.
            APPEND ls_ztsd0019_del TO lt_ztsd0019_del.

            "collect log
*            CLEAR gs_ztsd0019_log.
*            MOVE-CORRESPONDING <alv4> TO gs_ztsd0019_log.
*
*            CALL FUNCTION 'Z_FBC0007'
*              IMPORTING
*                ret_timestamp = g_timestamp.
*
*            gs_ztsd0019_log-trtyp = 'L'. "delete
*            gs_ztsd0019_log-aenam = sy-uname.
*            gs_ztsd0019_log-aedat = sy-datum.
*            gs_ztsd0019_log-timestamp = g_timestamp.
*
*            APPEND gs_ztsd0019_log TO gt_ztsd0019_log.

            "delete alv screen data
            DELETE gt_alv4 INDEX l_tabix.
          ENDLOOP.

          IF lt_ztsd0019_del[] IS NOT INITIAL.

            "Delete db
            DELETE ztsd0019 FROM TABLE lt_ztsd0019_del[].

            "Save log
*            MODIFY ztsd0019_log FROM TABLE gt_ztsd0019_log.

            COMMIT WORK AND WAIT.
*            MESSAGE 'Delete successfully' TYPE 'S'.
            PERFORM popup_message TABLES lt_ztsd0019_del[]
                                  USING 'Delete successfully :'
                                         'S'.

            REFRESH: lt_ztsd0019_del[].
*                     gt_ztsd0019_log[].
          ENDIF.

        WHEN '2'. "no
          "do nothing

      ENDCASE.
*<< Begin add 2023/02/09 **************************************************************
    WHEN 'ZTSD0187'.
      READ TABLE gt_alv6 TRANSPORTING NO FIELDS WITH KEY checkbox = 'X'.
      IF sy-subrc <> 0.
        MESSAGE 'Please select at least one data' TYPE 'E'.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Delete MaxQty & Korea Label'
          text_question         = 'Are you sure want to delete?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CASE answer.
        WHEN '1'.

          REFRESH: lt_ztsd0187_del[],
                   gt_ztsd0187_log[].

          LOOP AT gt_alv6 ASSIGNING <alv6> WHERE checkbox = 'X'.

            l_tabix = sy-tabix.

            "collect delele data
            CLEAR ls_ztsd0187_del.
            MOVE-CORRESPONDING <alv6> TO ls_ztsd0187_del.
            APPEND ls_ztsd0187_del TO lt_ztsd0187_del.

            "collect log
            CLEAR gs_ztsd0187_log.
            MOVE-CORRESPONDING <alv6> TO gs_ztsd0187_log.

            SELECT SINGLE zernam
                          zerdat
              INTO (gs_ztsd0187_log-zernam,
                    gs_ztsd0187_log-zerdat
                    )
              FROM ztsd0187_log
              WHERE zzgccode = <alv6>-zzgccode
              AND   zzpartno = <alv6>-zzpartno
              AND   zzproductno = <alv6>-zzproductno
              AND   zzshape = <alv6>-zzshape
              AND   zzgrade = <alv6>-zzgrade
              AND   matnr = <alv6>-matnr.

            CALL FUNCTION 'Z_FBC0007'
              IMPORTING
                ret_timestamp = g_timestamp.

            gs_ztsd0187_log-trtyp = 'L'. "delete
            gs_ztsd0187_log-zaenam = sy-uname.
            gs_ztsd0187_log-zaedat = sy-datum.
            gs_ztsd0187_log-timestamp = g_timestamp.

            APPEND gs_ztsd0187_log TO gt_ztsd0187_log.

            "delete alv screen data
            DELETE gt_alv6 INDEX l_tabix.
          ENDLOOP.

          IF lt_ztsd0187_del[] IS NOT INITIAL.

            "Delete db
            DELETE ztsd0187 FROM TABLE lt_ztsd0187_del[].

            "Save log
            MODIFY ztsd0187_log FROM TABLE gt_ztsd0187_log.

            COMMIT WORK AND WAIT.
            PERFORM popup_message TABLES lt_ztsd0187_del[]
                                  USING 'Delete successfully :'
                                         'S'.

            REFRESH: lt_ztsd0187_del[],
                     gt_ztsd0187_log[].
          ENDIF.

        WHEN '2'. "no
          "do nothing

      ENDCASE.
*<< End   add 2023/02/09 **************************************************************

  ENDCASE.

ENDFORM.                    " ZDELETE
*&---------------------------------------------------------------------*
*&      Form  ZDOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zdownload .

  CASE g_mode.
    WHEN 'ZTSD0018'.
      PERFORM zdownload_ztsd0018.
    WHEN 'ZTSD0019'.
      PERFORM zdownload_ztsd0019.
*<< Begin add 2023/02/09 **************
    WHEN 'ZTSD0187'.
      PERFORM zdownload_ztsd0187.
*<< End   add 2023/02/09 **************
  ENDCASE.

ENDFORM.                    " ZDOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  CHECK_FIELD_E
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_field_e .

*  1.如果不存在ZTSD0019-ZZPACKMATNO的話: please maintain the Pack Material No. in ZTSD0019 first
*  2.以E欄為key值，E欄+H欄+I欄的組合與ZTSD0019不同時(ZTSD0019-ZZPACKMATNO+ZTSD0019-ZZPACKMATNOCAT+ZTSD0019-ZZMEASURE):
*     Pack Material No. can’t have different Pack Material Category in ZTSD0019
*  3.以E欄為key值，E欄+H欄+I欄的組合於excel中有不同時: Pack Material No. can’t have different Pack Material Category in excel

  TYPES: BEGIN OF ty_check,
            zzpackmatno     TYPE zzpackmatno,
            zzpackmatnocat  TYPE zzpackmatnocat,
**            zzmeasure       TYPE zzmeasure,  "Remark 2022/11/16 ，不檢查I欄
         END OF ty_check.
  DATA: lt_check TYPE TABLE OF ty_check,
        ls_check TYPE ty_check.
  DATA: ls_data  TYPE ty_data.
  DATA: l_count  TYPE i.

  CLEAR gs_ztsd0019.
  IF gs_data-zzpackmatno IS NOT INITIAL.
    SELECT SINGLE * FROM ztsd0019 INTO gs_ztsd0019 WHERE zzpackmatno = gs_data-zzpackmatno.
    IF sy-subrc <> 0.
      g_error = 'X'.
      APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
      MOVE-CORRESPONDING gs_data TO <alv>.
      <alv>-status = gv_led_red.
      <alv>-seq = g_tabix.
      <alv>-msg = text-e04.
    ELSE.

*<< Begin Remark 2022/11/16 ，第二點不檢查 *******************************
*      IF ( gs_ztsd0019-zzpackmatnocat NE gs_data-zzpackmatnocat
*       OR  gs_ztsd0019-zzmeasure NE gs_data-zzmeasure ).
*
*        g_error = 'X'.
*        APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
*        MOVE-CORRESPONDING gs_data TO <alv>.
*        <alv>-status = gv_led_red.
*        <alv>-seq = g_tabix.
*        <alv>-msg = text-e05.
*      ENDIF.
*<< End   Remark 2022/11/16 ，第二點不檢查 *******************************

    ENDIF.
  ENDIF.

  REFRESH lt_check.
  LOOP AT gt_data INTO ls_data WHERE zzpackmatno = gs_data-zzpackmatno.
    CLEAR ls_check.
    MOVE-CORRESPONDING ls_data TO ls_check.
    APPEND ls_check TO lt_check.
  ENDLOOP.

  SORT lt_check BY zzpackmatno.
  DELETE ADJACENT DUPLICATES FROM lt_check COMPARING ALL FIELDS.

  CLEAR l_count.
  l_count = LINES( lt_check ).
  IF l_count > 1.
    g_error = 'X'.
    APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
    MOVE-CORRESPONDING gs_data TO <alv>.
    <alv>-status = gv_led_red.
    <alv>-seq = g_tabix.
    <alv>-msg = text-e15.
  ENDIF.

ENDFORM.                    " CHECK_FIELD_E
*&---------------------------------------------------------------------*
*&      Form  DO_ZTSD0018
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_ztsd0018 .

  CASE 'X'.
    WHEN r1.
      IF p_file IS INITIAL.
        MESSAGE 'Please select file path !' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSE.
        PERFORM check_vkorg.
        PERFORM upload_excel.
        PERFORM check_data.
        PERFORM show_alv USING 'ZTSD0018'.
      ENDIF.

    WHEN r2.
      PERFORM retrieve_data USING 'ZTSD0018'. "Get data from DB
      PERFORM show_alv USING 'ZTSD0018'.

  ENDCASE.

ENDFORM.                    " DO_ZTSD0018
*&---------------------------------------------------------------------*
*&      Form  DO_ZTSD0019
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_ztsd0019 .

  CASE 'X'.
    WHEN r3.
      IF p_file2 IS INITIAL.
        MESSAGE 'Please select file path !' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSE.
        PERFORM upload_excel.
        PERFORM check_data.
        PERFORM show_alv USING 'ZTSD0019'.
      ENDIF.

    WHEN r4.
      PERFORM retrieve_data USING 'ZTSD0019'. "Get data from DB
      PERFORM show_alv USING 'ZTSD0019'.

  ENDCASE.

ENDFORM.                    " DO_ZTSD0019
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTSD0018
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_ztsd0018 .

*<< initial
  REFRESH gt_ztsd0018.

*<< get data from DB
  SELECT * INTO TABLE gt_ztsd0018
    FROM ztsd0018
    WHERE zzpartno IN s_part
    AND   zzshape IN s_shape
    AND   zzpackmatno IN s_packno
    AND   zzgccode IN s_gccode
    AND   cust_no IN s_custno
    AND   zzpackmatnocat IN s_cat
    AND   zzmeasure IN s_measur
    AND   zzmeasure IN s_pallet.
  IF sy-subrc <> 0.
    MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*<< Build alv data
  LOOP AT gt_ztsd0018 INTO gs_ztsd0018.
    APPEND INITIAL LINE TO gt_alv2 ASSIGNING <alv2>.
    MOVE-CORRESPONDING gs_ztsd0018 TO <alv2>.
  ENDLOOP.

ENDFORM.                    " SELECT_ZTSD0018
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTSD0019
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_ztsd0019 .

*<< initial
  REFRESH gt_ztsd0019.

*<< get data from DB
  SELECT * INTO TABLE gt_ztsd0019
    FROM ztsd0019
    WHERE zzpackmatno IN s_part2
    AND   zzpackmatnocat IN s_cat2.
  IF sy-subrc <> 0.
    MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*<< Build alv data
  LOOP AT gt_ztsd0019 INTO gs_ztsd0019.
    APPEND INITIAL LINE TO gt_alv4 ASSIGNING <alv4>.
    MOVE-CORRESPONDING gs_ztsd0019 TO <alv4>.
  ENDLOOP.

ENDFORM.                    " SELECT_ZTSD0019
*&---------------------------------------------------------------------*
*&      Form  ZDOWNLOAD_ZTSD0018
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zdownload_ztsd0018 .

  DATA: filename TYPE string.

  TYPES: BEGIN OF ty_header,
           line(50) TYPE c,
         END OF ty_header.
  DATA: lt_header TYPE TABLE OF ty_header,
        ls_header TYPE ty_header.

  TYPES: BEGIN OF ty_lownload,
          zzvkorg(4) TYPE c,
          zzpartno(25) TYPE c,
          zzshape(1) TYPE c,
          zzproductno(25) TYPE c,
          zzpackmatno(10) TYPE c,
          zzgccode(40) TYPE c,
          cust_no(10) TYPE c,
          zzpackmatnocat(1) TYPE c,
          zzmeasure(30) TYPE c,
          by_pallet(1) TYPE c,
          zzboxqty TYPE zzboxqty,
          zzcapacityrate TYPE zzcapacityrate,
          zztotalpackqty TYPE zztotalpackqty,
          ntgew TYPE ntgew,
          brgew TYPE brgew,
          zzmaxpackqty TYPE zzmaxqty,   "add on 2023/02/21
          zzalumbag TYPE zzalumbag,     "add on 2023/02/21
         END OF ty_lownload.

  DATA: lt_lownload TYPE TABLE OF ty_lownload,
        ls_lownload TYPE ty_lownload.

  filename = p_path.
  CONCATENATE  filename '\Packing Standard Download.xls' INTO filename.

  "header name
  IF lt_header[] IS INITIAL.
    ls_header-line = 'Sales Org'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Part Number'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Kind/Shape ID'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Product Number'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Pack.M.No'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'GC Code'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'End Cust No'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Pack.M.C'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Measure'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'By Pallet'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Box Qty'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Capacity R'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Total Pack Qty'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Net Weight'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Gross Weight'.
    APPEND ls_header TO lt_header.

*<< Begin add on 2023/02/21 **************************
    ls_header-line = 'MaxPackQty'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Alu. Bag'.
    APPEND ls_header TO lt_header.
*<< End   add on 2023/02/21 **************************

  ENDIF.

  REFRESH lt_lownload.
  LOOP AT gt_alv2 ASSIGNING <alv2>.
    MOVE-CORRESPONDING <alv2> TO ls_lownload.
    APPEND ls_lownload TO lt_lownload.
  ENDLOOP.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename              = filename
*      filetype              = 'ASC'
      filetype              = 'DBF'
      write_field_separator = 'X'
    TABLES
      data_tab              = lt_lownload
      fieldnames            = lt_header.

ENDFORM.                    " ZDOWNLOAD_ZTSD0018
*&---------------------------------------------------------------------*
*&      Form  ZDOWNLOAD_ZTSD0019
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zdownload_ztsd0019 .

  DATA: filename TYPE string.

  TYPES: BEGIN OF ty_header,
           line(50) TYPE c,
         END OF ty_header.
  DATA: lt_header TYPE TABLE OF ty_header,
        ls_header TYPE ty_header.

  TYPES: BEGIN OF ty_lownload,
          zzpackmatno(10) TYPE c,
          zzpackmatnocat(1) TYPE c,
          zzmeasure(30) TYPE c,
          ntgew TYPE ntgew,
          brgew TYPE brgew,
         END OF ty_lownload.

  DATA: lt_lownload TYPE TABLE OF ty_lownload,
        ls_lownload TYPE ty_lownload.

  filename = p_path2.
  CONCATENATE  filename '\Pack Material Download.xls' INTO filename.

  "header name
  IF lt_header[] IS INITIAL.
    ls_header-line = 'Pack Material No'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Pack Material Category'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Measure'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Net Weight'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Gross Weight'.
    APPEND ls_header TO lt_header.
  ENDIF.

  REFRESH lt_lownload.
  LOOP AT gt_alv4 ASSIGNING <alv4>.
    MOVE-CORRESPONDING <alv4> TO ls_lownload.
    APPEND ls_lownload TO lt_lownload.
  ENDLOOP.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename              = filename
*      filetype              = 'ASC'
      filetype              = 'DBF'
      write_field_separator = 'X'
    TABLES
      data_tab              = lt_lownload
      fieldnames            = lt_header.

ENDFORM.                    " ZDOWNLOAD_ZTSD0019
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_ZTSD0018
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data_ztsd0018 .

  DATA: value1   TYPE i,
        value2   TYPE i.

  DATA: l_error  TYPE flag,
        l_raube  LIKE mara-raube.

*<< Remove 2022/11/25
**<< 所有欄位的資訊都相同時，程式直接刪除重複的record
*  DELETE ADJACENT DUPLICATES FROM gt_data COMPARING ALL FIELDS.

  LOOP AT gt_data INTO gs_data.

*    g_tabix  = sy-tabix.
    g_tabix = g_tabix + 1.

    CLEAR g_error.

*<< Determine N-Brand or P-Brand:
    PERFORM determine_brand USING gs_data-zzgccode.

*<< Remove 2022/11/25
**<< 檢查A~D欄必須為空白: Sales Org, Part No, Shape, Product No only can put “Blank”
*    IF ( gs_data-zzvkorg     IS NOT INITIAL
*     OR  gs_data-zzpartno    IS NOT INITIAL
*     OR  gs_data-zzshape     IS NOT INITIAL
*     OR  gs_data-zzproductno IS NOT INITIAL ).
*
*      g_error = 'X'.
*      APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
*      MOVE-CORRESPONDING gs_data TO <alv>.
*      <alv>-status = gv_led_red.
*      <alv>-seq = g_tabix.
*      <alv>-msg = text-e02.
*    ENDIF.

*<< 檢查必填欄位，缺資料時輸出missing required data: &欄位名稱&
    IF g_brand = 'P'.
      PERFORM mandatory_field_check USING : 'ZZPACKMATNO',    "Pack Material No
                                            'ZZGCCODE',       "GC Code
                                            'ZZPACKMATNOCAT', "Pack Material Category
                                            'ZZBOXQTY',       "Box Qty
                                            'ZZCAPACITYRATE', "Capacity Rate
                                            'ZZTOTALPACKQTY'. "Total Pack Qty
    ENDIF.

*<< 檢查E欄
    PERFORM check_field_e.


*<< 檢查G欄 , 有值的話要檢查是否存在於KNA1-KUNNR，不存在時輸出the end customer doesn’t exist
    IF gs_data-cust_no IS NOT INITIAL.
      SELECT * UP TO 1 ROWS FROM kna1 WHERE kunnr = gs_data-cust_no.
      ENDSELECT.
      IF sy-subrc <> 0.
        g_error = 'X'.
        APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
        MOVE-CORRESPONDING gs_data TO <alv>.
        <alv>-status = gv_led_red.
        <alv>-seq = g_tabix.
        <alv>-msg = text-e06.
      ENDIF.
    ENDIF.

*<< 檢查I欄
    IF g_brand = 'P'.
      PERFORM check_field_i.
    ENDIF.


*<< 檢查J欄有壓值的話，必須只能為X: By Pallet only can put ‘X’
    IF ( gs_data-by_pallet IS NOT INITIAL AND gs_data-by_pallet NE 'X' ).
      g_error = 'X'.
      APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
      MOVE-CORRESPONDING gs_data TO <alv>.
      <alv>-status = gv_led_red.
      <alv>-seq = g_tabix.
      <alv>-msg = text-e07.
    ENDIF.

*<< 檢查L欄
    IF g_brand = 'P'.
      PERFORM check_field_l.
    ENDIF.

*<< 檢查K欄*L欄=M欄: please check the rule that Box Qty * Capacity Rate = Total Pack Qty
    IF ( gs_data-zzboxqty       IS NOT INITIAL AND
         gs_data-zzcapacityrate IS NOT INITIAL AND
         gs_data-zztotalpackqty IS NOT INITIAL ).

      value1 = gs_data-zzboxqty * gs_data-zzcapacityrate.
      value2 = gs_data-zztotalpackqty.

      IF value1 NE value2.
        g_error = 'X'.
        APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
        MOVE-CORRESPONDING gs_data TO <alv>.
        <alv>-status = gv_led_red.
        <alv>-seq = g_tabix.
        <alv>-msg = text-e08.
      ENDIF.
    ENDIF.

*<< 檢查ZTSD0018和ZTSD0087有無 match
*    IF g_brand = 'P'.
*      PERFORM check_match.
*    ENDIF.

*<< A~G欄資訊需要與ZTSD0018比對是否有重複key值資料: the same key data already exists
    PERFORM set_range.
    SELECT SINGLE * INTO gs_ztsd0018 FROM ztsd0018
      WHERE zzvkorg = gs_data-zzvkorg
      AND   zzpartno = gs_data-zzpartno
      AND   zzshape = gs_data-zzshape
      AND   zzproductno = gs_data-zzproductno
      AND   zzpackmatno IN s_zzpackmatno
      AND   zzgccode IN s_zzgccode
      AND   cust_no IN s_cust_no.
    IF sy-subrc = 0.
      g_error = 'X'.
      APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
      MOVE-CORRESPONDING gs_data TO <alv>.
      <alv>-status = gv_led_yellow.
      <alv>-seq = g_tabix.
      <alv>-msg = text-e09.
    ENDIF.

*<< 檢查A~G欄裡至少要有一欄有值：都無值時輸出 Not Allowed All Key Columns are blank
    IF gs_data-zzvkorg IS INITIAL.
      IF gs_data-zzpartno IS INITIAL.
        IF gs_data-zzshape IS INITIAL.
          IF gs_data-zzproductno IS INITIAL.
            IF gs_data-zzpackmatno IS INITIAL.
              IF gs_data-zzgccode IS INITIAL.
                IF gs_data-cust_no IS INITIAL.
                  g_error = 'X'.
                  APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
                  MOVE-CORRESPONDING gs_data TO <alv>.
                  <alv>-status = gv_led_red.
                  <alv>-seq = g_tabix.
                  <alv>-msg = text-e18.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*<< 檢查F欄有值的話，其值要存在物料主檔(MARA-ZZGCCODE)：the GC Code doesn’t exist
    IF g_brand = 'P'.
      IF gs_data-zzgccode IS NOT INITIAL.
        SELECT COUNT(*) FROM mara WHERE zzgccode = gs_data-zzgccode.
        IF sy-subrc <> 0.
          g_error = 'X'.
          APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
          MOVE-CORRESPONDING gs_data TO <alv>.
          <alv>-status = gv_led_red.
          <alv>-seq = g_tabix.
          <alv>-msg = text-e19.
        ELSE.
*<<檢查G欄(GC CODE)，是否有隸屬在正確的Shipping Site
*  用MARA-ZZGCCODE抓MARA-RAUBE，當此值為Z2時其對應的篩選條件Site必為Waltech，
*  當此值為Z3或Z4或Z5時其對應的篩選條件Site必為UTAC，
*  然後選NTC的話先不卡判斷邏輯
*  如果對應失敗的話顯示: this GC Code not allowed to be used by this Site.
          CLEAR l_error.
          IF p_vkorg NE 'NTC'.
            SELECT SINGLE raube INTO l_raube FROM mara WHERE zzgccode = gs_data-zzgccode.
            IF sy-subrc = 0.
              CASE l_raube.
                WHEN 'Z2'.
                  IF p_vkorg NE 'Waltech'.
                    l_error = 'X'.
                  ENDIF.
                WHEN 'Z3' OR 'Z4' OR 'Z5'.
                  IF p_vkorg NE 'UTAC'.
                    l_error = 'X'.
                  ENDIF.
              ENDCASE.
            ENDIF.

            IF l_error = 'X'.
              g_error = 'X'.
              APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
              MOVE-CORRESPONDING gs_data TO <alv>.
              <alv>-status = gv_led_red.
              <alv>-seq = g_tabix.
              <alv>-msg = text-e20.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    IF g_error = ' '. "check no error
      APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
      MOVE-CORRESPONDING gs_data TO <alv>.
      <alv>-status = gv_led_green.
      <alv>-seq = g_tabix.
      <alv>-msg = ' '.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHECK_DATA_ZTSD0018
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_ZTSD0019
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data_ztsd0019 .

*<< 所有欄位的資訊都相同時，程式直接刪除重複的record
  DELETE ADJACENT DUPLICATES FROM gt_data2 COMPARING ALL FIELDS.

  LOOP AT gt_data2 INTO gs_data2.

*    g_tabix  = sy-tabix.
    g_tabix = g_tabix + 1.

    CLEAR g_error.

*<< 檢查必填欄位，缺資料時輸出missing required data: &欄位名稱&
    PERFORM mandatory_field_check USING : 'ZZPACKMATNO',    "Pack Material No
                                          'ZZPACKMATNOCAT'. "Pack Material Category

    PERFORM check_zzpackmatno.

    FIND '*' IN gs_data2-zzmeasure.
    IF sy-subrc = 0.
      g_error = 'X'.
      APPEND INITIAL LINE TO gt_alv3 ASSIGNING <alv3>.
      MOVE-CORRESPONDING gs_data2 TO <alv3>.
      <alv3>-status = gv_led_red.
      <alv3>-seq = g_tabix.
      <alv3>-msg = text-e11.
    ENDIF.

    FIND 'x' IN gs_data2-zzmeasure.
    IF sy-subrc = 0.
      g_error = 'X'.
      APPEND INITIAL LINE TO gt_alv3 ASSIGNING <alv3>.
      MOVE-CORRESPONDING gs_data2 TO <alv3>.
      <alv3>-status = gv_led_red.
      <alv3>-seq = g_tabix.
      <alv3>-msg = text-e11.
    ENDIF.

*<< 檢查A欄(Pack Material)都必須要是大寫: Pack Material must use uppercase
    PERFORM check_eng_uppercase USING `ZZPACKMATNO`.

    IF g_error = ' '. "check no error
      APPEND INITIAL LINE TO gt_alv3 ASSIGNING <alv3>.
      MOVE-CORRESPONDING gs_data2 TO <alv3>.
      <alv3>-status = gv_led_green.
      <alv3>-seq = g_tabix.
      <alv3>-msg = ' '.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHECK_DATA_ZTSD0019
*&---------------------------------------------------------------------*
*&      Form  CHECK_ZZPACKMATNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_zzpackmatno .

  TYPES: BEGIN OF ty_check,
            zzpackmatno TYPE zzpackmatno,
            zzpackmatnocat TYPE zzpackmatnocat,
            zzmeasure TYPE zzmeasure,
*            ntgew TYPE ntgew,  "remove check
*            brgew TYPE brgew,  "remove check
         END OF ty_check.
  DATA: lt_check    TYPE TABLE OF ty_check,
        ls_check    TYPE ty_check.
  DATA: ls_data2    TYPE ty_data2.

  DATA: l_count TYPE i.

  REFRESH lt_check.
  LOOP AT gt_data2 INTO ls_data2 WHERE zzpackmatno = gs_data2-zzpackmatno.
    CLEAR ls_check.
    MOVE-CORRESPONDING ls_data2 TO ls_check.
    APPEND ls_check TO lt_check.
  ENDLOOP.

  SORT lt_check.
  DELETE ADJACENT DUPLICATES FROM lt_check COMPARING ALL FIELDS.

  l_count = LINES( lt_check ).
  IF l_count > 1.
    g_error = 'X'.
    APPEND INITIAL LINE TO gt_alv3 ASSIGNING <alv3>.
    MOVE-CORRESPONDING gs_data2 TO <alv3>.
    <alv3>-status = gv_led_red.
    <alv3>-seq = g_tabix.
    <alv3>-msg = text-e16.
  ENDIF.

  SELECT COUNT(*) FROM ztsd0019 WHERE zzpackmatno = gs_data2-zzpackmatno.
  IF sy-subrc = 0.
    g_error = 'X'.
    APPEND INITIAL LINE TO gt_alv3 ASSIGNING <alv3>.
    MOVE-CORRESPONDING gs_data2 TO <alv3>.
    <alv3>-status = gv_led_yellow.
    <alv3>-seq = g_tabix.
    <alv3>-msg = text-e17.
  ENDIF.


ENDFORM.                    " CHECK_ZZPACKMATNO
*&---------------------------------------------------------------------*
*&      Form  POPUP_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2611   text
*      -->P_2612   text
*      -->P_GT_ZTSD0018[]  text
*----------------------------------------------------------------------*
FORM popup_message  TABLES t_tab
                    USING  p_message
                           p_type.

  DATA: l_lines     TYPE i.
  DATA: number_c(6) TYPE c.

  CLEAR l_lines.
  l_lines = LINES( t_tab[] ).

  WRITE l_lines TO number_c LEFT-JUSTIFIED.

  REFRESH t_message_tab.

  APPEND INITIAL LINE TO t_message_tab ASSIGNING <message>.
  <message>-msgid = 'ZSD'.
  <message>-msgty = p_type.
  <message>-msgno = 000.
  CONCATENATE p_message number_c INTO  <message>-msgv1 SEPARATED BY space.

*<< Popup message
  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
    TABLES
      i_message_tab = t_message_tab.


ENDFORM.                    " POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_BRAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OUTPUT_ZZGCCODE  text
*----------------------------------------------------------------------*
FORM determine_brand  USING  p_zzgccode.

*<< GCcode有值時表示為P-Brand，無值則表示為N-Brand
  "CLEAR g_mode.

  IF p_zzgccode IS NOT INITIAL.
    g_brand = 'P'.
  ELSE.
    g_brand = 'N'.
  ENDIF.

ENDFORM.                    " DETERMINE_BRAND
*&---------------------------------------------------------------------*
*&      Form  CHECK_VKORG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vkorg .

  IF p_vkorg IS INITIAL.
    SET CURSOR FIELD 'P_VKORG'.
    MESSAGE 'Fill in all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " CHECK_VKORG
*&---------------------------------------------------------------------*
*&      Form  DO_ZTSD0187
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_ztsd0187 .

  CASE 'X'.
    WHEN r5.
      IF p_file3 IS INITIAL.
        MESSAGE 'Please select file path !' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSE.
        PERFORM upload_excel.
        PERFORM check_data.
        PERFORM show_alv USING 'ZTSD0187'.
      ENDIF.

    WHEN r6.
      PERFORM retrieve_data USING 'ZTSD0187'. "Get data from DB
      PERFORM show_alv USING 'ZTSD0187'.

  ENDCASE.

ENDFORM.                    " DO_ZTSD0187
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_ZTSD0187
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data_ztsd0187 .

*<< 所有欄位的資訊都相同時，程式直接刪除重複的record
  DELETE ADJACENT DUPLICATES FROM gt_data3 COMPARING ALL FIELDS.

  LOOP AT gt_data3 INTO gs_data3.

    g_tabix = g_tabix + 1.

    CLEAR g_error.

*<< 檢查必填欄位，缺資料時輸出missing required data: &欄位名稱&
    PERFORM mandatory_field_check USING : 'ZZGCCODE'.   "GC Code

*<< A欄(GC Code)有值的話，其值要存在物料主檔(MARA-ZZGCCODE)：the GC Code doesn’t exist→紅燈
    IF gs_data3-zzgccode IS NOT INITIAL.
      SELECT COUNT(*) FROM mara WHERE zzgccode = gs_data3-zzgccode.
      IF sy-subrc <> 0.
        g_error = 'X'.
        APPEND INITIAL LINE TO gt_alv5 ASSIGNING <alv5>.
        MOVE-CORRESPONDING gs_data3 TO <alv5>.
        <alv5>-status = gv_led_red.
        <alv5>-seq = g_tabix.
        <alv5>-msg = text-e19.
      ENDIF.
    ENDIF.

*<< key值已經存在ZTSD0187中的話：the same key data already exists→黃燈
*    PERFORM set_range.
    SELECT SINGLE * INTO gs_ztsd0187 FROM ztsd0187
      WHERE zzgccode = gs_data3-zzgccode
        AND zzpartno = gs_data3-zzpartno
        AND zzproductno = gs_data3-zzproductno
        AND zzshape = gs_data3-zzshape
        AND zzgrade = gs_data3-zzgrade
        AND matnr = gs_data3-matnr.
    IF sy-subrc = 0.
      g_error = 'X'.
      APPEND INITIAL LINE TO gt_alv5 ASSIGNING <alv5>.
      MOVE-CORRESPONDING gs_data3 TO <alv5>.
      <alv5>-status = gv_led_yellow.
      <alv5>-seq = g_tabix.
      <alv5>-msg = text-e09.
    ENDIF.

    IF g_error = ' '. "check no error
      APPEND INITIAL LINE TO gt_alv5 ASSIGNING <alv5>.
      MOVE-CORRESPONDING gs_data3 TO <alv5>.
      <alv5>-status = gv_led_green.
      <alv5>-seq = g_tabix.
      <alv5>-msg = ' '.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHECK_DATA_ZTSD0187
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTSD0187
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_ztsd0187 .

*<< initial
  REFRESH gt_ztsd0187.

*<< get data from DB
  SELECT * INTO TABLE gt_ztsd0187
    FROM ztsd0187
    WHERE zzgccode IN s_gcode
    AND   zzrecyclelabelno IN s_rlabel.
  IF sy-subrc <> 0.
    MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*<< Build alv data
  LOOP AT gt_ztsd0187 INTO gs_ztsd0187.
    APPEND INITIAL LINE TO gt_alv6 ASSIGNING <alv6>.
    MOVE-CORRESPONDING gs_ztsd0187 TO <alv6>.
  ENDLOOP.

ENDFORM.                    " SELECT_ZTSD0187
*&---------------------------------------------------------------------*
*&      Form  ZDOWNLOAD_ZTSD0187
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zdownload_ztsd0187 .

  DATA: filename TYPE string.

  TYPES: BEGIN OF ty_header,
           line(50) TYPE c,
         END OF ty_header.
  DATA: lt_header TYPE TABLE OF ty_header,
        ls_header TYPE ty_header.

  TYPES: BEGIN OF ty_lownload,
            zzgccode  TYPE zgccode,
            zzpartno  TYPE zzpartno,
            zzproductno TYPE zzproductno,
            zzshape TYPE zzshape,
            zzgrade TYPE zzgrade,
            matnr TYPE matnr,
            zzmaxpackcapa TYPE zzmaxpackcapa,
            zzrecyclelabelno TYPE zzrecyclelabelno,
            ernam TYPE ernam,
            erdat TYPE erdat,
            erzet TYPE erzet,
         END OF ty_lownload.

  DATA: lt_lownload TYPE TABLE OF ty_lownload,
        ls_lownload TYPE ty_lownload.

  filename = p_path3.
  CONCATENATE  filename '\MaxQty & Korea Label Download.xls' INTO filename.

  "header name
  IF lt_header[] IS INITIAL.
    ls_header-line = `GC Code`.
    APPEND ls_header TO lt_header.

    ls_header-line = `PARTNO`.
    APPEND ls_header TO lt_header.

    ls_header-line = `PRODUCTNO`.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Shape'.
    APPEND ls_header TO lt_header.

    ls_header-line = 'Grade'.
    APPEND ls_header TO lt_header.

    ls_header-line = `Material`.
    APPEND ls_header TO lt_header.

    ls_header-line = `M.Capacity`.
    APPEND ls_header TO lt_header.

    ls_header-line = `KOREA.L.N`.
    APPEND ls_header TO lt_header.

    ls_header-line = `CreatedBy`.
    APPEND ls_header TO lt_header.

    ls_header-line = `CreatedOn`.
    APPEND ls_header TO lt_header.

    ls_header-line = `Creat.Time`.
    APPEND ls_header TO lt_header.
  ENDIF.

  REFRESH lt_lownload.
  LOOP AT gt_alv6 ASSIGNING <alv6>.
    MOVE-CORRESPONDING <alv6> TO ls_lownload.
    APPEND ls_lownload TO lt_lownload.
  ENDLOOP.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename              = filename
*      filetype              = 'ASC'
      filetype              = 'DBF'
      write_field_separator = 'X'
    TABLES
      data_tab              = lt_lownload
      fieldnames            = lt_header.

ENDFORM.                    " ZDOWNLOAD_ZTSD0187
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENG_UPPERCASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_`ZZPACKMATNO`  text
*----------------------------------------------------------------------*
FORM check_eng_uppercase  USING  p_fieldname TYPE fieldname.

  CONSTANTS: c_eng TYPE string VALUE `abcdefghijklmnopqrstuvwxyz`.

  FIELD-SYMBOLS: <value> TYPE ANY.
  FIELD-SYMBOLS: <fs_struc> TYPE ANY.
  DATA: lv_structure_name TYPE text30.

  "init
  UNASSIGN: <value>,
            <fs_struc>.

  TRANSLATE p_fieldname TO UPPER CASE.

  CASE g_mode.
    WHEN 'ZTSD0018'.
      lv_structure_name = 'GS_DATA'.
    WHEN 'ZTSD0019'.
      lv_structure_name = 'GS_DATA2'.
    WHEN 'ZTSD0187'.
      lv_structure_name = 'GS_DATA3'.
  ENDCASE.

  ASSIGN (lv_structure_name) TO <fs_struc>.
  ASSIGN COMPONENT p_fieldname OF STRUCTURE <fs_struc> TO <value>.

  CHECK <value> IS ASSIGNED.
  IF <value> CA c_eng.

    g_error = 'X'.

    CASE g_mode.
      WHEN 'ZTSD0018'.
      WHEN 'ZTSD0019'.
        APPEND INITIAL LINE TO gt_alv3 ASSIGNING <alv3>.
        MOVE-CORRESPONDING gs_data2 TO <alv3>.
        <alv3>-status = gv_led_red.
        <alv3>-seq = g_tabix.
        IF p_fieldname = 'ZZPACKMATNO'.
          <alv3>-msg = text-e21.
        ENDIF.
      WHEN 'ZTSD0187'.
    ENDCASE.

  ENDIF.

ENDFORM.                    " CHECK_ENG_UPPERCASE
