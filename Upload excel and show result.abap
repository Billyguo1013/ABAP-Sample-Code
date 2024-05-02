************************************************************************
* Program Name  : ZRSDSA0178
* Descriptions  : Upload relevant SO Data for NTCJ
* Create By     : V_BILLY
* Create Date   : 2022/08/01
* Tcode         : ZRSDSA0178
************************************************************************
* Modification Log
************************************************************************
*   Date     Ver.   Programmer   Descriptions
* ---------- ----   ------------ --------------------------------------
*
************************************************************************

REPORT zrsdsa0178 NO STANDARD PAGE HEADING
                  LINE-SIZE 255
                  LINE-COUNT 65.

INCLUDE zrsdsa0178_top. " Data declaration
INCLUDE zrsdsa0178_s01. " Selection screen
INCLUDE zrsdsa0178_f01. " function


************************************************************************
*                      START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

  CASE 'X'.
    WHEN p_r1. "no use

    WHEN p_r2. "Mapping SO1,PO,SO2
      IF p_file IS INITIAL.
        MESSAGE 'Please select file path !' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSE.
        PERFORM upload_excel.
        PERFORM update_ztsd0084.
      ENDIF.
    WHEN p_r3. "Update SO2
      IF p_file IS INITIAL.
        MESSAGE 'Please select file path !' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSE.
        PERFORM upload_excel.
        PERFORM bapi_salesorder_change.
      ENDIF.
  ENDCASE.

END-OF-SELECTION.


/*----------------------------------------------------------------------------------------------------*/
/                                                                                                      /
/*----------------------------------------------------------------------------------------------------*/


*&---------------------------------------------------------------------*
*&  Include           ZRSDSA0178_TOP
*&---------------------------------------------------------------------*

TABLES: sscrfields.
TYPE-POOLS: slis,icon,truxs,abap.
DATA: functxt TYPE smp_dyntxt.

TABLES: vbak, vbap.

DATA: gt_ztsd0084 TYPE TABLE OF ztsd0084,
      gt_tvko     TYPE TABLE OF tvko.

FIELD-SYMBOLS: <ztsd0084> LIKE LINE OF gt_ztsd0084.

DATA: g_vbeln          TYPE vbeln,
      g_posnr          TYPE posnr_va,
      g_sold2          TYPE kunnr,
      g_sales_employee LIKE pa0001-pernr,
      g_inside_sales   LIKE pa0001-pernr.

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
    f009 TYPE string,   "add on 2023/01/12
  END OF ty_excel,
  tt_excel TYPE TABLE OF ty_excel.

TYPES: BEGIN OF ty_data,
         vbeln      LIKE ztsd0084-vbeln,
         ebeln      LIKE ztsd0084-ebeln,
         vkorg_so2  LIKE ztsd0084-vkorg_so2,
       END OF ty_data.
DATA: gt_data   TYPE TABLE OF ty_data,
      gw_data   TYPE ty_data.


TYPES: BEGIN OF ty_data2,
         vbeln          LIKE vbap-vbeln,
         posnr          LIKE vbap-posnr,
         sold_to        TYPE kunnr,
         end_cust       TYPE kunnr,
         zzappcode      TYPE zzappcode,
         kdmat          TYPE matnr_ku,  "Customer part no.
         sales_employee LIKE pa0001-pernr,
         inside_sales   LIKE pa0001-pernr,
         zzbstkd_e      LIKE vbap-zzbstkd_e, "Customer reference  2023/01/12
       END OF ty_data2.
DATA: gt_data2   TYPE TABLE OF ty_data2,
      gw_data2   TYPE ty_data2.

**********************************************************************
* ALV Declarations
**********************************************************************
DATA: gt_fcat     TYPE lvc_t_fcat,
      gw_fieldtab LIKE LINE OF gt_fcat,
      gt_events   TYPE slis_t_event,
      gw_events   LIKE LINE OF gt_events,
      gs_layout   TYPE lvc_s_layo,
      g_repid     LIKE sy-repid.

TYPES: BEGIN OF ty_alv,
         row(5)     TYPE c,
         message    TYPE bapi_msg, "Message Text
         vbeln      LIKE ztsd0084-vbeln,
         ebeln      LIKE ztsd0084-ebeln,
         vkorg_so2  LIKE ztsd0084-vkorg_so2,
      END OF ty_alv.
DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv LIKE LINE OF gt_alv.
FIELD-SYMBOLS: <alv> LIKE LINE OF gt_alv.

TYPES: BEGIN OF ty_alv2,
         row(5)     TYPE c,
         message    TYPE bapi_msg, "Message Text
         vbeln      LIKE vbap-vbeln,
         posnr      LIKE vbap-posnr,
         sold_to    TYPE kunnr,
         end_cust   TYPE kunnr,
         zzappcode  TYPE zzappcode,
         kdmat      TYPE matnr_ku,  "Customer part no.
         sales_employee LIKE pa0001-pernr,
         inside_sales   LIKE pa0001-pernr,
         zzbstkd_e      LIKE vbap-zzbstkd_e, "Customer reference  2023/01/12
      END OF ty_alv2.
DATA: gt_alv2 TYPE TABLE OF ty_alv2,
      gs_alv2 LIKE LINE OF gt_alv2.
FIELD-SYMBOLS: <alv2> LIKE LINE OF gt_alv2.

DATA : new_table TYPE REF TO data,
       new_line  TYPE REF TO data.

FIELD-SYMBOLS :
                <g_table> TYPE STANDARD TABLE,
                <g_line>  TYPE ANY,
                <g_field> TYPE ANY.

********************************************************
* BAPI Structure                                       *
********************************************************
DATA: testrun              TYPE bapiflag-bapiflag,
      salesdocumentin      TYPE bapivbeln-vbeln,
      order_header_in      TYPE bapisdhd1,
      order_header_inx     TYPE bapisdhd1x,
      logic_switch         TYPE bapisdls,
      salesdocument        TYPE bapivbeln-vbeln,
      return               TYPE TABLE OF bapiret2,
      order_items_in       TYPE TABLE OF bapisditm,
      order_items_inx      TYPE TABLE OF bapisditmx,
      partners             TYPE TABLE OF bapiparnr,
      partneraddresses     TYPE TABLE OF bapiaddr1,
      order_schedules_in   TYPE TABLE OF bapischdl,
      order_schedules_inx  TYPE TABLE OF bapischdlx,
      order_conditions_in  TYPE TABLE OF bapicond,
      order_conditions_inx TYPE TABLE OF bapicondx,
      order_text           TYPE TABLE OF bapisdtext,
      extensionin          TYPE TABLE OF bapiparex,
      test_return          LIKE return,
      partnerchanges       TYPE TABLE OF bapiparnrc.

DATA:
      order_header_in_chg     TYPE bapisdh1,
      order_header_inx_chg    TYPE bapisdh1x,
      simulation              TYPE bapiflag-bapiflag.


FIELD-SYMBOLS: <return>               LIKE LINE OF return,
               <order_items_in>       LIKE LINE OF order_items_in,
               <order_items_inx>      LIKE LINE OF order_items_inx,
               <partners>             LIKE LINE OF partners,
               <partneraddresses>     LIKE LINE OF partneraddresses,
               <order_schedules_in>   LIKE LINE OF order_schedules_in,
               <order_schedules_inx>  LIKE LINE OF order_schedules_inx,
               <order_conditions_in>  LIKE LINE OF order_conditions_in,
               <order_conditions_inx> LIKE LINE OF order_conditions_inx,
               <order_text>           LIKE LINE OF order_text,
               <extensionin>          LIKE LINE OF extensionin,
               <extensioninx>         LIKE LINE OF extensionin,
               <partnerchanges>       LIKE LINE OF partnerchanges.

CONSTANTS: true       TYPE abap_bool  VALUE abap_true,
           false      TYPE abap_bool  VALUE abap_false,
           c_posnr_h  TYPE vbap-posnr VALUE '000000'.

DATA: g_soitem(6).

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



/*----------------------------------------------------------------------------------------------------*/
/                                                                                                      /
/*----------------------------------------------------------------------------------------------------*/


*&---------------------------------------------------------------------*
*&  Include           ZRSDSA0178_S01
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Screen build
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-s00.

PARAMETERS: p_r1 RADIOBUTTON GROUP gp1 USER-COMMAND flag MODIF ID mod.  "hidden

PARAMETERS: p_r2 RADIOBUTTON GROUP gp1 .  "Mapping SO1,PO,SO2

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: p_r3 RADIOBUTTON GROUP gp1 .  "Update SO2
SELECTION-SCREEN: COMMENT (79) ztext_01.
SELECTION-SCREEN: END OF LINE.

PARAMETERS: p_file   LIKE rlgrap-filename. "Upload Filename

SELECTION-SCREEN END OF BLOCK blk1.
SELECTION-SCREEN: FUNCTION KEY 1,
                  FUNCTION KEY 2.

*--------------------------------------------------------------------*
* AT SELECTION-SCREEN
*--------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM download_template USING 'ZRSDSA0178_1'.
    WHEN 'FC02'.
      PERFORM download_template USING 'ZRSDSA0178_2'.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.


  LOOP AT SCREEN.
    IF screen-group1 = 'MOD'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM get_file CHANGING p_file.


INITIALIZATION.

  REFRESH: gt_alv,
           gt_alv2.

  ztext_01 = text-001.

  CLEAR functxt.
  functxt-icon_id = icon_xxl.
  functxt-quickinfo = text-m01.
  functxt-icon_text = text-m01.
  sscrfields-functxt_01 = functxt.

  CLEAR functxt.
  functxt-icon_id = icon_xxl.
  functxt-quickinfo = text-m02.
  functxt-icon_text = text-m02.
  sscrfields-functxt_02 = functxt.



  SELECT * INTO TABLE gt_tvko FROM tvko.                "#EC CI_NOWHERE


/*----------------------------------------------------------------------------------------------------*/
/                                                                                                      /
/*----------------------------------------------------------------------------------------------------*/


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
        MESSAGE 'Download Fail !'(046) TYPE 'W'.
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

  CASE sy-ucomm.
    WHEN 'FC01'.
      CONCATENATE default_file_name '\Mapping SO1,PO,SO2 Template' INTO default_file_name.
    WHEN 'FC02'.
      CONCATENATE default_file_name '\Update SO2 Template' INTO default_file_name.
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
*& Form get_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM get_file  CHANGING p_file.
  DATA:
    l_filename    TYPE string,
    l_user_action TYPE i.
  CALL FUNCTION 'GUI_FILE_LOAD_DIALOG'
    EXPORTING
      window_title = 'Please select a file'(043)
      file_filter  = 'EXCEL|*.XLSX;*.XLS'
    IMPORTING
      fullpath     = l_filename
      user_action  = l_user_action.
  IF l_user_action = 0.
    p_file = l_filename.
  ENDIF.

ENDFORM.                    "get_file
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
    lw_field       TYPE ty_excel,
    ls_excel       TYPE ty_excel,
    lt_excel       TYPE tt_excel,
    lf_cnt(3)      TYPE n,
    fieldname1(30),
    fieldname2(30),
    fieldname3(30).

  DATA:it_raw TYPE truxs_t_text_data.


  "initial
  REFRESH: gt_data,
           gt_data2.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_tab_raw_data       = it_raw
      i_filename           = p_file
    TABLES
      i_tab_converted_data = lt_excel
    EXCEPTIONS
      OTHERS               = 1.
  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH 'File upload with error'.
  ENDIF.

*<< Delete header
  DELETE lt_excel FROM 1 TO 2.

*<<
  CASE 'X'.
    WHEN p_r1. "no use

    WHEN p_r2. "Mapping SO1,PO,SO2

      LOOP AT lt_excel INTO ls_excel.
        CLEAR gw_data.

        gw_data-vbeln      = ls_excel-f001.
        gw_data-ebeln      = ls_excel-f002.
        gw_data-vkorg_so2  = ls_excel-f003.

        conversion_exit_alpha_input gw_data-vbeln.
        conversion_exit_alpha_input gw_data-ebeln.

        APPEND gw_data TO gt_data.
      ENDLOOP.

      IF gt_data IS INITIAL.
        MESSAGE text-e01 TYPE 'E'.
      ENDIF.

    WHEN p_r3. "Update SO2

      LOOP AT lt_excel INTO ls_excel.
        CLEAR gw_data2.

        gw_data2-vbeln          = ls_excel-f001.
        gw_data2-posnr          = ls_excel-f002.
        gw_data2-sold_to        = ls_excel-f003.
        gw_data2-end_cust       = ls_excel-f004.
        gw_data2-zzappcode      = ls_excel-f005.
        gw_data2-kdmat          = ls_excel-f006.
        gw_data2-sales_employee = ls_excel-f007.
        gw_data2-inside_sales   = ls_excel-f008.
        gw_data2-zzbstkd_e      = ls_excel-f009.  "add on 2023/01/12

        conversion_exit_alpha_input gw_data2-vbeln.

        IF gw_data2-sold_to+0(1) CO '0123456789'.
          conversion_exit_alpha_input gw_data2-sold_to.
        ENDIF.

        IF gw_data2-end_cust+0(1) CO '0123456789'.
          conversion_exit_alpha_input gw_data2-end_cust.
        ENDIF.

        APPEND gw_data2 TO gt_data2.
      ENDLOOP.

      IF gt_data2 IS INITIAL.
        MESSAGE text-e01 TYPE 'E'.
      ENDIF.

  ENDCASE.


ENDFORM.                    " UPLOAD_EXCEL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTSD0084
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztsd0084 .

  DATA: l_row(5)      TYPE c,
        l_row2(5)     TYPE c,
        l_error(1)    TYPE c,
        l_message     TYPE bapi_msg,
        l_duplicated  TYPE vbeln.

  DATA: lt_data       TYPE TABLE OF ty_data,
        ls_data       TYPE ty_data.

  DATA: ls_ztsd0084   TYPE ztsd0084.
  DATA: l_so2         LIKE ztsd0084-zso2.

  CHECK gt_data IS NOT INITIAL.

  "initial
  REFRESH: gt_ztsd0084,gt_alv.

  CLEAR: l_row,
         l_row2,
         l_error,
         l_message.

  " 檢查SO1, PO, Sales Org for So2當三欄資訊都相同時，程式直接刪除重複的record
  DELETE ADJACENT DUPLICATES FROM gt_data COMPARING ALL FIELDS.

*  SORT gt_data BY vbeln.

  LOOP AT gt_data INTO gw_data.

    l_row = l_row + 1.
    SHIFT l_row LEFT DELETING LEADING space.

    CLEAR: l_error,
           l_message,
           l_duplicated.
    PERFORM check_all_field USING gw_data
                           CHANGING l_error
                                    l_message     "error message
                                    l_duplicated. "duplicated SO1

    IF l_error = 'X'. " have error
      APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
      <alv>-row        = l_row.
      <alv>-message    = l_message.
      <alv>-vbeln      = gw_data-vbeln.
      <alv>-ebeln      = gw_data-ebeln.
      <alv>-vkorg_so2  = gw_data-vkorg_so2.

*把有重複的那列也一起帶出來
      IF l_duplicated IS NOT INITIAL.

        REFRESH lt_data.
        lt_data[] = gt_data[].

        LOOP AT lt_data INTO ls_data WHERE vbeln = l_duplicated.

          l_row2 = sy-tabix.
          SHIFT l_row2 LEFT DELETING LEADING space.

          CHECK l_row2 <> l_row.

          APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
          <alv>-row        = l_row2.
          <alv>-message    = l_message.
          <alv>-vbeln      = ls_data-vbeln.
          <alv>-ebeln      = ls_data-ebeln.
          <alv>-vkorg_so2  = ls_data-vkorg_so2.

        ENDLOOP.
      ENDIF.

    ELSE.
*<< Begin DNRK945659 2022/11/22 *****************************************************************
*<< 要先判斷SO2有沒有存在VBAK-VBELN (註:SO2是抓EXCEL中的SO1欄位，去零並加上前綴 I)
      PERFORM convert_s01s02 USING gw_data-vbeln CHANGING l_so2.

      SELECT COUNT(*) FROM vbak WHERE vbeln = l_so2.

      IF sy-subrc NE 0. "<< INSERT ZTSD0084

        APPEND INITIAL LINE TO gt_ztsd0084 ASSIGNING <ztsd0084>.
        <ztsd0084>-vbeln        = gw_data-vbeln.
        <ztsd0084>-ztype        = '2'.
        <ztsd0084>-zpo_err      = space.
        <ztsd0084>-ebeln        = gw_data-ebeln.
        <ztsd0084>-zso2_err     = 'X'.
        <ztsd0084>-zso2         = l_so2.
        <ztsd0084>-vkorg_so1    = 'NJP1'.
        <ztsd0084>-vkorg_so2    = gw_data-vkorg_so2.
        <ztsd0084>-so1_external = 'Y'.
        <ztsd0084>-so2_external = 'N'.
        <ztsd0084>-ernam        = sy-uname.
        <ztsd0084>-erdat        = sy-datum.
        <ztsd0084>-aedat        = space.

      ELSE. "<< UPDATE ZTSD0084

*當SO2有存在VBAK-VBELN時，用EXCEL中的SO1欄位=ZTSD0084-VBELN來取出ZTSD0084 TABLE中的資料，
*並做下述三欄的更新EBELN、VKORG_SO2、AEDAT
        CLEAR ls_ztsd0084.
        SELECT SINGLE * INTO ls_ztsd0084 FROM ztsd0084
          WHERE vbeln = gw_data-vbeln.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO gt_ztsd0084 ASSIGNING <ztsd0084>.
          <ztsd0084>-vbeln        = gw_data-vbeln.
          <ztsd0084>-ztype        = ls_ztsd0084-ztype.
          <ztsd0084>-zpo_err      = ls_ztsd0084-zpo_err.
          <ztsd0084>-ebeln        = gw_data-ebeln.
          <ztsd0084>-zso2_err     = ls_ztsd0084-zso2_err.
          <ztsd0084>-zso2         = ls_ztsd0084-zso2.
          <ztsd0084>-vkorg_so1    = ls_ztsd0084-vkorg_so1.

          SELECT SINGLE vkorg FROM vbak
            INTO <ztsd0084>-vkorg_so2
            WHERE vbeln = ls_ztsd0084-zso2.

          <ztsd0084>-so1_external = ls_ztsd0084-so1_external.
          <ztsd0084>-so2_external = ls_ztsd0084-so2_external.
          <ztsd0084>-ernam        = ls_ztsd0084-ernam.
          <ztsd0084>-erdat        = ls_ztsd0084-erdat.
          <ztsd0084>-aedat        = sy-datum.
*<< Begin fix, 2023/01/10 *************************************************
        ELSE.
          APPEND INITIAL LINE TO gt_ztsd0084 ASSIGNING <ztsd0084>.
          <ztsd0084>-vbeln        = gw_data-vbeln.
          <ztsd0084>-ztype        = '2'.
          <ztsd0084>-zpo_err      = space.
          <ztsd0084>-ebeln        = gw_data-ebeln.
          <ztsd0084>-zso2_err     = 'X'.
          <ztsd0084>-zso2         = l_so2.
          <ztsd0084>-vkorg_so1    = 'NJP1'.
          <ztsd0084>-vkorg_so2    = gw_data-vkorg_so2.
          <ztsd0084>-so1_external = 'Y'.
          <ztsd0084>-so2_external = 'N'.
          <ztsd0084>-ernam        = sy-uname.
          <ztsd0084>-erdat        = sy-datum.
          <ztsd0084>-aedat        = space.
*<< End   fix, 2023/01/10 *************************************************
        ENDIF.
      ENDIF.
*<< End   DNRK945659 2022/11/22 *****************************************************************
    ENDIF.

  ENDLOOP.

  " 只要檢查到一筆有問題的資訊時，就不更新TABLE，並且在ALV上呈現出錯誤的行數和訊息。
  IF gt_alv IS NOT INITIAL.
    SORT gt_alv BY row.
    DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING ALL FIELDS.

    PERFORM show_alv.
  ELSE.
    MODIFY ztsd0084 FROM TABLE gt_ztsd0084.
    COMMIT WORK AND WAIT.
    MESSAGE text-s01 TYPE 'S'.
  ENDIF.

ENDFORM.                    " UPDATE_ZTSD0084
*&---------------------------------------------------------------------*
*&      Form  CHECK_ALL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_DATA  text
*      <--P_L_ERROR  text
*----------------------------------------------------------------------*
FORM check_all_field  USING    pw_data       TYPE ty_data
                      CHANGING p_error       TYPE char1
                               p_message     TYPE bapi_msg
                               p_duplicated  TYPE vbeln.

  "Set default
  p_error = ''.

  "missing required data
  IF pw_data-vbeln IS INITIAL OR pw_data-ebeln IS INITIAL OR pw_data-vkorg_so2 IS INITIAL.
    p_error = 'X'.
    p_message = text-e02.
    EXIT.
  ENDIF.

  "the so1 is duplicated
  IF g_vbeln IS INITIAL.
    g_vbeln = pw_data-vbeln.
  ELSE.
    IF g_vbeln <> pw_data-vbeln.
      g_vbeln = pw_data-vbeln.
    ELSE.
      p_duplicated = pw_data-vbeln.
      p_error = 'X'.
      p_message = text-e03.
      EXIT.
    ENDIF.
  ENDIF.

  "存在VBAK-VBELN時輸出the so1 can’t exist in ECC system
  SELECT * UP TO 1 ROWS FROM vbak WHERE vbeln = pw_data-vbeln.
  ENDSELECT.
  IF sy-subrc = 0.
    p_error = 'X'.
    p_message = text-e09.
    EXIT.
  ENDIF.


  "the sales organization does not exist
  READ TABLE gt_tvko TRANSPORTING NO FIELDS WITH KEY vkorg = pw_data-vkorg_so2.
  IF sy-subrc <> 0.
    p_error = 'X'.
    p_message = text-e04.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_ALL_FIELD
*&---------------------------------------------------------------------*
*&      Form  CONVERT_S01S02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_DATA_EBELN  text
*      <--P_<ZTSD0084>_ZSO2  text
*----------------------------------------------------------------------*
FORM convert_s01s02  USING    p_ebeln
                     CHANGING p_zso2.

  CONCATENATE 'I'
              p_ebeln+1(9)
              INTO p_zso2.

ENDFORM.                    " CONVERT_S01S02
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_alv .

  PERFORM init.
  PERFORM build_layout.
  PERFORM build_fieldcat.
  PERFORM assign_data.

  DATA: l_repid LIKE sy-repid.
  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_bypassing_buffer       = 'X'
      i_callback_pf_status_set = 'MENU_SET'
      i_callback_user_command  = 'USER_COMMAND_EVENT'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fcat
      it_events                = gt_events
      i_default                = 'X'
      i_save                   = 'A'
    TABLES
      t_outtab                 = <g_table>  "internal table
    EXCEPTIONS
      OTHERS                   = 0.

ENDFORM.                    " SHOW_ALV
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .

  gs_layout-cwidth_opt = 'X'.
  gs_layout-zebra      = 'X'.

*  gs_layout-box_fieldname     = 'SEL'.
*  gs_layout-window_titlebar   = ''.

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat .

  CASE 'X'.
    WHEN p_r1. "no use

    WHEN p_r2. "Mapping SO1,PO,SO2

      field_desc_i 'ROW'            'ROW'         'ROW'               'ROW'                 '5'.
      field_desc_i 'MESSAGE'        'Msg.'        'Msg.'              'Message'             '220'.
      field_desc_i 'VBELN'          'SO1'         'SO1'               'SO1'                 '10'.
      field_desc_i 'EBELN'          'PO'          'PO'                'PO'                  '10'.
      field_desc_i 'VKORG_SO2'      'SOrg.S02'    'Sales Org for So2' 'Sales Org for So2'   '10'.

    WHEN p_r3. "Update SO2

      field_desc_i 'ROW'            'ROW'         'ROW'               'ROW'                 '5'.
      field_desc_i 'MESSAGE'        'Msg.'        'Msg.'              'Message'             '220'.
      field_desc_i 'VBELN'          'SO2'         'SO2'               'SO2'                 '10'.
      field_desc_i 'POSNR'          'Item'        'SO2 Item'          'SO2 Item'            '10'.
      field_desc_i 'SOLD_TO'        'Sold2'       'Sold-to(ZO)'       'Sold-to(ZO)'         '10'.
      field_desc_i 'END_CUST'       'End.c'       'End customer(Z3)'  'End customer(Z3)'    '10'.
      field_desc_i 'ZZAPPCODE'      'APP Code'    'Application Code'  'Application Code'    '10'.
      field_desc_i 'KDMAT'          'Cust.part'   'Customer part no.' 'Customer part no.'   '35'.
      field_desc_i 'SALES_EMPLOYEE' 'Sales.ee'    'Sales Employee'    'Sales Employee'      '8'.
      field_desc_i 'INSIDE_SALES'   'Inside.s'    'Inside Sales'      'Inside Sales'        '8'.
      field_desc_i 'ZZBSTKD_E'      'EndCust PO'  'End Customer PO'   'Customer Reference'  '35'.   "add on 2023/01/12

  ENDCASE.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fcat
    IMPORTING
      ep_table        = new_table.

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
  ls_ucomm = 'ZSAVE'.
  APPEND ls_ucomm TO lt_ucomm.

  SET PF-STATUS 'STANDARD' EXCLUDING lt_ucomm. "Setting ALV Toolbar

ENDFORM.                    "menu_set
*&---------------------------------------------------------------------*
*&      Form  BAPI_SALESORDER_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_salesorder_change .

  DATA: l_row(5)      TYPE c,
        l_row2(5)     TYPE c,
        l_error(1)    TYPE c,
        l_message     TYPE bapi_msg,
        l_duplicated  TYPE vbeln,
        l_sold2_x(1)  TYPE c,
        l_zp_x(1)     TYPE c,
        l_zq_x(1)     TYPE c.

  DATA: lt_data2      TYPE TABLE OF ty_data2,
        ls_data2      TYPE ty_data2.


  CHECK gt_data2 IS NOT INITIAL.

  "檢查當每一欄資訊都相同時，程式直接刪除重複的record
  DELETE ADJACENT DUPLICATES FROM gt_data2 COMPARING ALL FIELDS.

*  SORT gt_data2 BY vbeln.

  LOOP AT gt_data2 INTO gw_data2.

    l_row = l_row + 1.
    SHIFT l_row LEFT DELETING LEADING space.

    CLEAR: l_error,
           l_message,
           l_duplicated .
    PERFORM check_all_field2 USING gw_data2
                             CHANGING l_error
                                      l_message
                                      l_duplicated
                                      l_sold2_x
                                      l_zp_x
                                      l_zq_x.

    IF l_error = 'X'. " have error
      APPEND INITIAL LINE TO gt_alv2 ASSIGNING <alv2>.
      MOVE-CORRESPONDING gw_data2 TO <alv2>.
      <alv2>-row        = l_row.
      <alv2>-message    = l_message.

*把有重複SO/item 的那列也一起帶出來
      IF l_duplicated IS NOT INITIAL.
        REFRESH lt_data2.
        lt_data2[] = gt_data2[].

        LOOP AT lt_data2 INTO ls_data2 WHERE vbeln = l_duplicated
                                       AND   posnr = gw_data2-posnr.

          l_row2 = sy-tabix.
          SHIFT l_row2 LEFT DELETING LEADING space.

          CHECK l_row2 <> l_row.

          APPEND INITIAL LINE TO gt_alv2 ASSIGNING <alv2>.
          MOVE-CORRESPONDING ls_data2 TO <alv2>.
          <alv2>-row        = l_row2.
          <alv2>-message    = l_message.
        ENDLOOP.
      ENDIF.

*把相同SO, 不同的SOLD To 都帶出來
      IF l_sold2_x IS NOT INITIAL.
        REFRESH lt_data2.
        lt_data2[] = gt_data2[].

        LOOP AT lt_data2 INTO ls_data2 WHERE vbeln = gw_data2-vbeln.

          l_row2 = sy-tabix.
          SHIFT l_row2 LEFT DELETING LEADING space.

          CHECK l_row2 <> l_row.

          APPEND INITIAL LINE TO gt_alv2 ASSIGNING <alv2>.
          MOVE-CORRESPONDING ls_data2 TO <alv2>.
          <alv2>-row        = l_row2.
          <alv2>-message    = l_message.
        ENDLOOP.
      ENDIF.

*把相同SO, 不同的sales employee 都帶出來
      IF l_zp_x IS NOT INITIAL.
        REFRESH lt_data2.
        lt_data2[] = gt_data2[].

        LOOP AT lt_data2 INTO ls_data2 WHERE vbeln = gw_data2-vbeln.

          l_row2 = sy-tabix.
          SHIFT l_row2 LEFT DELETING LEADING space.

          CHECK l_row2 <> l_row.

          APPEND INITIAL LINE TO gt_alv2 ASSIGNING <alv2>.
          MOVE-CORRESPONDING ls_data2 TO <alv2>.
          <alv2>-row        = l_row2.
          <alv2>-message    = l_message.
        ENDLOOP.
      ENDIF.

*把相同SO, 不同的inside sales 都帶出來
      IF l_zq_x IS NOT INITIAL.
        REFRESH lt_data2.
        lt_data2[] = gt_data2[].

        LOOP AT lt_data2 INTO ls_data2 WHERE vbeln = gw_data2-vbeln.

          l_row2 = sy-tabix.
          SHIFT l_row2 LEFT DELETING LEADING space.

          CHECK l_row2 <> l_row.

          APPEND INITIAL LINE TO gt_alv2 ASSIGNING <alv2>.
          MOVE-CORRESPONDING ls_data2 TO <alv2>.
          <alv2>-row        = l_row2.
          <alv2>-message    = l_message.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDLOOP.

  " 只要檢查到一筆有問題的資訊時，就不更新TABLE，並且在ALV上呈現出錯誤的行數和訊息。
  IF gt_alv2 IS NOT INITIAL.
    SORT gt_alv2 BY row.
    DELETE ADJACENT DUPLICATES FROM gt_alv2 COMPARING ALL FIELDS.

    PERFORM show_alv.
    EXIT.
  ELSE.
*--------------------------------------------------------------------*
* Start SO Change => First time: simulation
*--------------------------------------------------------------------*
    simulation    = 'X'.
    CLEAR l_row.
    LOOP AT gt_data2 INTO gw_data2.

      l_row = l_row + 1.
      SHIFT l_row LEFT DELETING LEADING space.

*<< 將數字前面補滿0
      g_soitem = gw_data2-posnr.
      conversion_exit_alpha_input g_soitem.

      PERFORM bapi_so_chg_init.          "Initialize
      PERFORM bapi_so_chg_prepare_h.     "Change Header
      PERFORM bapi_so_chg_prepare_i.     "Change Item
      PERFORM bapi_so_chg_prepare_p.     "Change Partner
      PERFORM bapi_so_cre_prepare_i_ext. "Extensionin

      salesdocument = gw_data2-vbeln.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = salesdocument
          order_header_in  = order_header_in_chg
          order_header_inx = order_header_inx_chg
          simulation       = simulation
        TABLES
          return           = return
          order_item_in    = order_items_in
          order_item_inx   = order_items_inx
          partners         = partners
          schedule_lines   = order_schedules_in
          schedule_linesx  = order_schedules_inx
          conditions_in    = order_conditions_in
          conditions_inx   = order_conditions_inx
          partnerchanges   = partnerchanges
          extensionin      = extensionin.

      LOOP AT return ASSIGNING <return> WHERE ( type = 'E' OR type = 'A' ).
        APPEND INITIAL LINE TO gt_alv2 ASSIGNING <alv2>.
        MOVE-CORRESPONDING gw_data2 TO <alv2>.
        <alv2>-row        = l_row.
        <alv2>-message    = <return>-message.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  IF gt_alv2 IS NOT INITIAL.
    SORT gt_alv2 BY row.
    DELETE ADJACENT DUPLICATES FROM gt_alv2 COMPARING ALL FIELDS.

    PERFORM show_alv.
    EXIT.
  ELSE.
*--------------------------------------------------------------------*
* Start SO Change => Second time: True run
*--------------------------------------------------------------------*
    simulation    = ''.
    LOOP AT gt_data2 INTO gw_data2.

*<< 將數字前面補滿0
      g_soitem = gw_data2-posnr.
      conversion_exit_alpha_input g_soitem.

      PERFORM bapi_so_chg_init.          "Initialize
      PERFORM bapi_so_chg_prepare_h.     "Change Header
      PERFORM bapi_so_chg_prepare_i.     "Change Item
      PERFORM bapi_so_chg_prepare_p.     "Change Partner
      PERFORM bapi_so_cre_prepare_i_ext. "Extensionin

      salesdocument = gw_data2-vbeln.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = salesdocument
          order_header_in  = order_header_in_chg
          order_header_inx = order_header_inx_chg
          simulation       = simulation
        TABLES
          return           = return
          order_item_in    = order_items_in
          order_item_inx   = order_items_inx
          partners         = partners
          schedule_lines   = order_schedules_in
          schedule_linesx  = order_schedules_inx
          conditions_in    = order_conditions_in
          conditions_inx   = order_conditions_inx
          partnerchanges   = partnerchanges
          extensionin      = extensionin.

      LOOP AT return TRANSPORTING NO FIELDS WHERE ( type = 'E' OR type = 'A' ).
      ENDLOOP.
      IF sy-subrc <> 0. "SUCCESS
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        PERFORM modify_text.  "add on 2023/01/12

        WAIT UP TO '0.05' SECONDS.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          IMPORTING
            return = return.

        EXIT.
      ENDIF.

    ENDLOOP.

    MESSAGE text-s01 TYPE 'S'.
  ENDIF.

ENDFORM.                    " BAPI_SALESORDER_CHANGE
*&---------------------------------------------------------------------*
*&      Form  BAPI_SO_CHG_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_so_chg_init .
  CLEAR: salesdocumentin,
         order_header_in,
         order_header_inx,
         logic_switch,
         salesdocument,
         return,
         test_return,
         order_items_in,
         order_items_inx,
         partners,
         order_schedules_in,
         order_schedules_inx,
         order_conditions_in,
         order_conditions_inx,
         order_text,
         partneraddresses,
         extensionin,
         partnerchanges,
         order_header_in_chg,
         order_header_inx_chg.
ENDFORM.                    " BAPI_SO_CHG_INIT
*&---------------------------------------------------------------------*
*&      Form  BAPI_SO_CHG_PREPARE_H
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_so_chg_prepare_h.

  order_header_inx_chg-updateflag = 'U'.

ENDFORM.                    " BAPI_SO_CHG_PREPARE_H
*&---------------------------------------------------------------------*
*&      Form  BAPI_SO_CHG_PREPARE_I
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_so_chg_prepare_i .

  APPEND INITIAL LINE TO order_items_in ASSIGNING <order_items_in>.
  APPEND INITIAL LINE TO order_items_inx ASSIGNING <order_items_inx>.

  <order_items_inx>-updateflag = 'U'.

  "Sales Document Item
  <order_items_in>-itm_number = g_soitem.
  <order_items_inx>-itm_number = <order_items_in>-itm_number.

  "Customer part no.
  <order_items_in>-cust_mat35 = gw_data2-kdmat.
  <order_items_inx>-cust_mat35 = 'X'.

ENDFORM.                    " BAPI_SO_CHG_PREPARE_I
*&---------------------------------------------------------------------*
*&      Form  BAPI_SO_CHG_PREPARE_P
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_so_chg_prepare_p .

  DATA: lt_data2      TYPE TABLE OF ty_data2,
        ls_data2      TYPE ty_data2,
        l_diff_z3(1)  TYPE c,
        l_end_cust    TYPE kunnr.

  "initial
  CLEAR: lt_data2[], l_diff_z3, l_end_cust.

  "move data
  lt_data2[] = gt_data2[].
  DELETE lt_data2[] WHERE vbeln <> gw_data2-vbeln.

*<< Sold to
  APPEND INITIAL LINE TO partnerchanges ASSIGNING <partnerchanges>.
  <partnerchanges>-document = gw_data2-vbeln.
  <partnerchanges>-itm_number = c_posnr_h.
  <partnerchanges>-updateflag = 'U'.
  <partnerchanges>-partn_role = 'ZO'.
  <partnerchanges>-p_numb_new = gw_data2-sold_to.

*<< End customer
*--------------------------------------------------------------------*
*<< excel上的同一張訂單的所有項次中Z3都是同一個人時，
*<< 要改表頭的Z3，也就是item_number = 000000
*<< by項次而有所不同時，就維持現有的吃item進去
*--------------------------------------------------------------------*
  APPEND INITIAL LINE TO partnerchanges ASSIGNING <partnerchanges>.
  <partnerchanges>-document = gw_data2-vbeln.

  l_diff_z3 = ''. "set default

  LOOP AT lt_data2 INTO ls_data2.
    IF l_end_cust IS INITIAL.
      l_end_cust = ls_data2-end_cust.
    ELSEIF l_end_cust <> ls_data2-end_cust.
      l_diff_z3 = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF l_diff_z3 = ''. "=>Z3都是同一個人
    <partnerchanges>-itm_number = c_posnr_h.                "000000
  ELSE.
    <partnerchanges>-itm_number = g_soitem.
  ENDIF.

  <partnerchanges>-updateflag = 'U'.
  <partnerchanges>-partn_role = 'Z3'.
  <partnerchanges>-p_numb_new = gw_data2-end_cust.


*<< SALES EMPLOYEE
  APPEND INITIAL LINE TO partnerchanges ASSIGNING <partnerchanges>.
  <partnerchanges>-document = gw_data2-vbeln.
  <partnerchanges>-itm_number = c_posnr_h.
  <partnerchanges>-updateflag = 'U'.
  <partnerchanges>-partn_role = 'ZP'.
  <partnerchanges>-p_numb_new = gw_data2-sales_employee.

*<< INSIDE SALES
  APPEND INITIAL LINE TO partnerchanges ASSIGNING <partnerchanges>.
  <partnerchanges>-document = gw_data2-vbeln.
  <partnerchanges>-itm_number = c_posnr_h.
  <partnerchanges>-updateflag = 'U'.
  <partnerchanges>-partn_role = 'ZQ'.
  <partnerchanges>-p_numb_new = gw_data2-inside_sales.

ENDFORM.                    " BAPI_SO_CHG_PREPARE_P
*&---------------------------------------------------------------------*
*&      Form  BAPI_SO_CHG_START
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_so_chg_start .

  salesdocument = gw_data2-vbeln.
  simulation = ''.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = salesdocument
      order_header_in  = order_header_in_chg
      order_header_inx = order_header_inx_chg
      simulation       = simulation
    TABLES
      return           = return
      order_item_in    = order_items_in
      order_item_inx   = order_items_inx
      partners         = partners
      schedule_lines   = order_schedules_in
      schedule_linesx  = order_schedules_inx
      conditions_in    = order_conditions_in
      conditions_inx   = order_conditions_inx
      partnerchanges   = partnerchanges
      extensionin      = extensionin.


ENDFORM.                    " BAPI_SO_CHG_START
*&---------------------------------------------------------------------*
*&      Form  BAPI_SO_CRE_PREPARE_I_EXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_so_cre_prepare_i_ext .

  DATA: ls_bape_vbap  TYPE bape_vbap,
        ls_bape_vbapx TYPE bape_vbapx.

  CLEAR: ls_bape_vbap ,
         ls_bape_vbapx.

*--------------------------------------------------------------------*
*<< Application Code
*--------------------------------------------------------------------*
  IF gw_data2-zzappcode IS NOT INITIAL.  "<< add on 2023/02/24
    ls_bape_vbap-vbeln = gw_data2-vbeln.
    ls_bape_vbap-posnr = g_soitem.
    ls_bape_vbap-zzappcode = gw_data2-zzappcode.

    ls_bape_vbapx-vbeln = gw_data2-vbeln.
    ls_bape_vbapx-posnr = g_soitem.
    ls_bape_vbapx-zzappcode = 'X'.
  ENDIF.

*<< Begin Remark on 2023/02/24 ********************************************
*  APPEND INITIAL LINE TO extensionin ASSIGNING <extensionin>.
*  <extensionin>-structure = 'BAPE_VBAP'.
*  <extensionin>-valuepart1 = ls_bape_vbap.
*
*  APPEND INITIAL LINE TO extensionin ASSIGNING <extensioninx>.
*  <extensioninx>-structure = 'BAPE_VBAPX'.
*  <extensioninx>-valuepart1 = ls_bape_vbapx.
*<< End   Remark on 2023/02/24 ********************************************

*--------------------------------------------------------------------*
*<< End Customer PO , add on 2023/01/12
*--------------------------------------------------------------------*
  IF gw_data2-zzbstkd_e IS NOT INITIAL.
*    CLEAR: ls_bape_vbap , ls_bape_vbapx.   "remark on 2023/02/24

    ls_bape_vbap-vbeln = gw_data2-vbeln.
    ls_bape_vbap-posnr = g_soitem.
    ls_bape_vbap-zzbstkd_e = gw_data2-zzbstkd_e.

    ls_bape_vbapx-vbeln = gw_data2-vbeln.
    ls_bape_vbapx-posnr = g_soitem.
    ls_bape_vbapx-zzbstkd_e = 'X'.

*<< Begin Remark on 2023/02/24 ********************************************
*    APPEND INITIAL LINE TO extensionin ASSIGNING <extensionin>.
*    <extensionin>-structure = 'BAPE_VBAP'.
*    <extensionin>-valuepart1 = ls_bape_vbap.
*
*    APPEND INITIAL LINE TO extensionin ASSIGNING <extensioninx>.
*    <extensioninx>-structure = 'BAPE_VBAPX'.
*    <extensioninx>-valuepart1 = ls_bape_vbapx.
*<< End   Remark on 2023/02/24 ********************************************

  ENDIF.

*<< Begin add on 2023/02/24 ********************************************
  IF ( ls_bape_vbap IS NOT INITIAL AND ls_bape_vbapx IS NOT INITIAL ).
    APPEND INITIAL LINE TO extensionin ASSIGNING <extensionin>.
    <extensionin>-structure = 'BAPE_VBAP'.
    <extensionin>-valuepart1 = ls_bape_vbap.

    APPEND INITIAL LINE TO extensionin ASSIGNING <extensioninx>.
    <extensioninx>-structure = 'BAPE_VBAPX'.
    <extensioninx>-valuepart1 = ls_bape_vbapx.
  ENDIF.
*<< End   add on 2023/02/24 ********************************************


*  PERFORM fill_x_strcture USING    ls_bape_vbap
*                          CHANGING ls_bape_vbapx.

*  PERFORM move_structure_to_extension USING ls_bape_vbap
*                                      CHANGING <extensionin>.
*  PERFORM move_structure_to_extension USING ls_bape_vbapx
*                                      CHANGING <extensioninx>.


ENDFORM.                    " BAPI_SO_CRE_PREPARE_I_EXT
*&---------------------------------------------------------------------*
*&      Form  FILL_X_STRCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_BAPE_VBAP  text
*      <--P_LS_BAPE_VBAPX  text
*----------------------------------------------------------------------*
FORM fill_x_strcture  USING    datastructure
                      CHANGING xstructure.
  DATA sdescr TYPE REF TO cl_abap_structdescr.
  FIELD-SYMBOLS: <data>,
                 <x>,
                 <comp> LIKE LINE OF sdescr->components.
  TRY .
      sdescr ?= cl_abap_typedescr=>describe_by_data( xstructure ).
    CATCH cx_root.
      RETURN.
  ENDTRY.
  LOOP AT sdescr->components ASSIGNING <comp>.
    IF <comp>-length = 2 AND
       <comp>-type_kind = cl_abap_typedescr=>typekind_char.
      ASSIGN COMPONENT <comp>-name OF STRUCTURE datastructure TO <data>.
      CHECK sy-subrc = 0.
      CHECK <data> IS NOT INITIAL.
      ASSIGN COMPONENT <comp>-name OF STRUCTURE xstructure TO <x>.
      CHECK sy-subrc = 0.
      <x> = true.
    ENDIF.
  ENDLOOP.
ENDFORM.                       " FILL_X_STRCTURE
*&---------------------------------------------------------------------*
*&      Form  MOVE_STRUCTURE_TO_EXTENSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_BAPE_VBAP  text
*      <--P_<EXTENSIONIN>  text
*----------------------------------------------------------------------*
FORM move_structure_to_extension  USING    is_struct
                                  CHANGING extensionin TYPE bapiparex .
  cl_abap_container_utilities=>fill_container_c(
    EXPORTING
      im_value               = is_struct
    IMPORTING
      ex_container           = extensionin+30(*)
    EXCEPTIONS
      OTHERS                 = 0 ).
ENDFORM.                    " MOVE_STRUCTURE_TO_EXTENSION
*&---------------------------------------------------------------------*
*&      Form  CHECK_ALL_FIELD2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_DATA2  text
*      <--P_L_ERROR  text
*      <--P_L_MESSAGE  text
*----------------------------------------------------------------------*
FORM check_all_field2  USING    pw_data       TYPE ty_data2
                       CHANGING p_error       TYPE char1
                                p_message     TYPE bapi_msg
                                p_duplicated  TYPE vbeln
                                p_sold2_x     TYPE char1
                                p_zp_x        TYPE char1
                                p_zq_x        TYPE char1.

  DATA: ls_kna1   TYPE kna1.
  DATA: ls_pa0001 TYPE pa0001.

  "Set default
  p_error = ''.

  "missing required data
  IF pw_data-vbeln     IS INITIAL OR
     pw_data-posnr     IS INITIAL OR
     pw_data-sold_to   IS INITIAL OR
     pw_data-end_cust  IS INITIAL OR
     pw_data-zzappcode IS INITIAL OR
     pw_data-kdmat     IS INITIAL OR
     pw_data-sales_employee IS INITIAL OR
     pw_data-inside_sales   IS INITIAL.

    p_error = 'X'.
    p_message = text-e02.
    EXIT.
  ENDIF.

  "檢查SO2、SO2 item：有重複時輸出the so2 no + item is duplicated
  "檢查如果SO2相同時，其sold-to也要相同：不一致時輸出the same so2 only have one sold-to
  "檢查如果SO2相同時，其sales employee也要相同：不一致時輸出the same so2 only have one sales employee
  "檢查如果SO2相同時，其inside sales也要相同：不一致時輸出the same so2 only have one inside sales
  IF g_vbeln IS INITIAL.
    g_vbeln = pw_data-vbeln.
    g_posnr = pw_data-posnr.
    g_sold2 = pw_data-sold_to.
    g_sales_employee = pw_data-sales_employee.
    g_inside_sales = pw_data-inside_sales.
  ELSE.
    IF ( g_vbeln = pw_data-vbeln AND g_posnr = pw_data-posnr ).
      p_duplicated = pw_data-vbeln.
      p_error = 'X'.
      p_message = text-e05.
      EXIT.

    ELSEIF g_vbeln <> pw_data-vbeln.
      g_vbeln = pw_data-vbeln.
      g_posnr = pw_data-posnr.
      g_sold2 = pw_data-sold_to.
      g_sales_employee = pw_data-sales_employee.
      g_inside_sales = pw_data-inside_sales.

    ELSEIF ( g_vbeln = pw_data-vbeln AND g_posnr <> pw_data-posnr ).

      IF g_sold2 <> pw_data-sold_to.
        p_error = 'X'.
        p_sold2_x = 'X'.
        p_message = text-e08.
        EXIT.
      ELSEIF g_sales_employee <> pw_data-sales_employee.
        p_error = 'X'.
        p_zp_x = 'X'.
        p_message = text-e13.
        EXIT.

      ELSEIF g_inside_sales <> pw_data-inside_sales.
        p_error = 'X'.
        p_zq_x = 'X'.
        p_message = text-e14.
        EXIT.

      ELSE.
        g_vbeln = pw_data-vbeln.
        g_posnr = pw_data-posnr.
        g_sold2 = pw_data-sold_to.
        g_sales_employee = pw_data-sales_employee.
        g_inside_sales = pw_data-inside_sales.

      ENDIF.

    ELSE.
      g_vbeln = pw_data-vbeln.
      g_posnr = pw_data-posnr.
      g_sold2 = pw_data-sold_to.
      g_sales_employee = pw_data-sales_employee.
      g_inside_sales = pw_data-inside_sales.
    ENDIF.
  ENDIF.

  "檢查so2和item必須存在系統
  SELECT * UP TO 1 ROWS FROM vbap WHERE vbeln = pw_data-vbeln
                                  AND   posnr = pw_data-posnr.
  ENDSELECT.
  IF sy-subrc <> 0.
    p_error = 'X'.
    p_message = text-e10.
    EXIT.
  ENDIF.


  SELECT SINGLE * INTO ls_kna1 FROM kna1 WHERE kunnr = pw_data-sold_to.
  IF sy-subrc <> 0.
    p_error = 'X'.
    p_message = text-e06.
    EXIT.
  ENDIF.

  SELECT SINGLE * INTO ls_kna1 FROM kna1 WHERE kunnr = pw_data-end_cust.
  IF sy-subrc <> 0.
    p_error = 'X'.
    p_message = text-e07.
    EXIT.
  ENDIF.

  "檢查sales employee(ZP) (PA0001-PERNR)：不存在時輸出the sales employee does not exist
  SELECT SINGLE * INTO ls_pa0001 FROM pa0001
    WHERE pernr = pw_data-sales_employee
    AND   begda <= sy-datum
    AND   endda >= sy-datum.
  IF sy-subrc <> 0.
    p_error = 'X'.
    p_message = text-e11.
    EXIT.
  ENDIF.

  "檢查inside sales(ZQ) (PA0001-PERNR)：不存在時輸出the inside sales does not exist
  SELECT SINGLE * INTO ls_pa0001 FROM pa0001
    WHERE pernr = pw_data-inside_sales
    AND   begda <= sy-datum
    AND   endda >= sy-datum.
  IF sy-subrc <> 0.
    p_error = 'X'.
    p_message = text-e12.
    EXIT.
  ENDIF.


ENDFORM.                    " CHECK_ALL_FIELD2
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
  ASSIGN new_table->* TO <g_table>.
  CREATE DATA new_line LIKE LINE OF <g_table>.
  ASSIGN new_line->* TO <g_line>.

  CASE 'X'.
    WHEN p_r1. "no use

    WHEN p_r2. "Mapping SO1,PO,SO2

      LOOP AT gt_alv INTO gs_alv.
        ASSIGN COMPONENT 'ROW' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv-row.
        ENDIF.
        ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv-message.
        ENDIF.
        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv-vbeln.
        ENDIF.
        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv-ebeln.
        ENDIF.
        ASSIGN COMPONENT 'VKORG_SO2' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv-vkorg_so2.
        ENDIF.

        INSERT <g_line> INTO TABLE <g_table>.
        CLEAR : <g_line> ,<g_field>.
      ENDLOOP.

    WHEN p_r3. "Update SO2

      LOOP AT gt_alv2 INTO gs_alv2.
        ASSIGN COMPONENT 'ROW' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-row.
        ENDIF.
        ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-message.
        ENDIF.
        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-vbeln.
        ENDIF.
        ASSIGN COMPONENT 'POSNR' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-posnr.
        ENDIF.
        ASSIGN COMPONENT 'SOLD_TO' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-sold_to.
        ENDIF.
        ASSIGN COMPONENT 'END_CUST' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-end_cust.
        ENDIF.
        ASSIGN COMPONENT 'ZZAPPCODE' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-zzappcode.
        ENDIF.
        ASSIGN COMPONENT 'KDMAT' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-kdmat.
        ENDIF.
        ASSIGN COMPONENT 'SALES_EMPLOYEE' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-sales_employee.
        ENDIF.
        ASSIGN COMPONENT 'INSIDE_SALES' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-inside_sales.
        ENDIF.
*<< Begin add 2023/01/12 *****************************************************
        ASSIGN COMPONENT 'ZZBSTKD_E' OF STRUCTURE <g_line> TO <g_field>.
        IF sy-subrc = 0.
          <g_field> = gs_alv2-zzbstkd_e.
        ENDIF.
*<< End   add 2023/01/12 *****************************************************
        INSERT <g_line> INTO TABLE <g_table>.
        CLEAR : <g_line> ,<g_field>.
      ENDLOOP.

  ENDCASE.

ENDFORM.                    " ASSIGN_DATA
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

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDFORM.                    "user_command_event
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .

  CLEAR: gs_layout,
         gt_fcat[],
         gt_events.

  IF <g_table> IS ASSIGNED.
    FREE <g_table>.
  ENDIF.

ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_text .

*--------------------------------------------------------------------*
*Modify Text
*--------------------------------------------------------------------*
  DATA: gt_lines  LIKE tline OCCURS 0 WITH HEADER LINE.
  DATA: gs_header LIKE thead.

  CLEAR gs_header.
  gs_header-tdid = 'Z021'.
  gs_header-tdspras = sy-langu.
  gs_header-tdobject = 'VBBP'.
  CONCATENATE gw_data2-vbeln g_soitem INTO gs_header-tdname.

  CLEAR gt_lines[].

  CLEAR gt_lines.
  gt_lines-tdformat = '*'.
  gt_lines-tdline = `&FINAL_BUYER&`.
  APPEND gt_lines.

  CLEAR gt_lines.
  gt_lines-tdformat = '*'.
  gt_lines-tdline = `&END_USER_ORDER_NO&`.
  APPEND gt_lines.

  CLEAR gt_lines.
  gt_lines-tdformat = '*'.
  gt_lines-tdline = `&VBAK-BSTNK&`.
  APPEND gt_lines.

  CLEAR gt_lines.
  gt_lines-tdformat = '*'.
  gt_lines-tdline = `DIFFUSION DONE IN &COUNTRY_OF_ORIGINW&`.
  APPEND gt_lines.

  CLEAR gt_lines.
  gt_lines-tdformat = '*'.
  gt_lines-tdline = `ASSEMBLED IN &COUNTRY_OF_ORIGIN&`.
  APPEND gt_lines.

  CLEAR gt_lines.
  gt_lines-tdformat = '*'.
  gt_lines-tdline = `C/No. :1-UP`.
  APPEND gt_lines.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = gs_header
      savemode_direct = 'X'
    TABLES
      lines           = gt_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE 'Modify success' TYPE 'S'.
  ENDIF.

ENDFORM.                    " MODIFY_TEXT




