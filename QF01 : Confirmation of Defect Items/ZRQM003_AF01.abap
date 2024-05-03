*&---------------------------------------------------------------------*
*& INCLUDE          ZRQM003_AF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form download_template
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
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

  CASE sy-ucomm.
    WHEN 'FC01'.
      default_file_name = '上傳範本-大量庫存移轉和瑕疵紀錄上傳'.
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
FORM upload_excel USING p_mode.


  FIELD-SYMBOLS:
    <fs1>,<fs2>,<fs3>.

  FIELD-SYMBOLS: <f> TYPE any.
  DATA l_field_type(1) TYPE c.

  DATA:
    lw_field       TYPE ty_excel,
    ls_excel       TYPE ty_excel,
    lt_excel       TYPE tt_excel,
    lf_cnt(2)      TYPE n,
    lf_len(2)      TYPE n,
    lf_pos(2)      TYPE n,
    sheetname      TYPE rlgrap-filename,
    fieldname1(30),
    fieldname2(30),
    fieldname3(30).

  DATA: lv_year(4)  TYPE c,
        lv_month(2) TYPE c,
        lv_day(2)   TYPE c.

  DATA:it_raw TYPE truxs_t_text_data.

* init
  CLEAR lt_excel.

  sheetname = SWITCH #( p_mode WHEN '1' THEN `ZRQM003_A`
                               WHEN '2' THEN `ZRQM003_A`
                               ELSE `ZRQM003_A` ).

  PERFORM frm_get_data_from_excel USING   p_file
                                          ' '
                                          sheetname
                                          1    "start Column
                                          1    "Start Row
                                          30   "End Column
                                          ''   "End Row
                                          1000 "Rows Per Time
                                 CHANGING lt_excel.

*<< Delete header
  DELETE lt_excel FROM 1 TO 1.

  IF lt_excel IS INITIAL.
    MESSAGE e398(00) WITH TEXT-e02.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CLEAR gt_data.

  LOOP AT lt_excel INTO ls_excel.

    IF ls_excel IS INITIAL.
      CONTINUE.
    ENDIF.

    APPEND INITIAL LINE TO gt_data ASSIGNING <gs_data>.

    <gs_data>-zzm6_no = ls_excel-f01.
    <gs_data>-matnr = ls_excel-f02.
    <gs_data>-menge = ls_excel-f03.     "移倉數量
    <gs_data>-umlgo = ls_excel-f04.     "儲存地點 (From)
    <gs_data>-fegrp = ls_excel-f05.     "瑕疵類型
    <gs_data>-fecod = ls_excel-f06.     "瑕疵代碼
    <gs_data>-anzfehler = ls_excel-f07. "瑕疵數量

*    CASE p_mode.
*      WHEN '1'.
*        <gs_data>-zzm6_no = ls_excel-f01.
*        <gs_data>-matnr = ls_excel-f02.
*        <gs_data>-menge = ls_excel-f03.     "移倉數量
*        <gs_data>-umlgo = ls_excel-f04.     "儲存地點 (From)
*      WHEN '2'.
*        <gs_data>-zzm6_no = ls_excel-f01.
*        <gs_data>-matnr = ls_excel-f02.
*        <gs_data>-fegrp = ls_excel-f05.     "瑕疵類型
*        <gs_data>-fecod = ls_excel-f06.     "瑕疵代碼
*        <gs_data>-anzfehler = ls_excel-f07. "瑕疵數量
*    ENDCASE.

  ENDLOOP.

ENDFORM.                    " UPLOAD_EXCEL
*&---------------------------------------------------------------------*
*& Form frm_get_data_from_excel
*&---------------------------------------------------------------------*
FORM frm_get_data_from_excel USING iv_file TYPE rlgrap-filename
                                     iv_actived    TYPE flag
                                     iv_sheet_name TYPE rlgrap-filename
                                     iv_begin_col  TYPE i
                                     iv_begin_row  TYPE i
                                     iv_end_col    TYPE i
                                     iv_end_row    TYPE i
                                     iv_rows       TYPE i
                            CHANGING ct_excel      TYPE tt_excel.


*--  Upload Excel From local PC
  CALL FUNCTION 'Z_BC_UPLOAD_EXCEL_FILE_OLE_A'
    EXPORTING
      iv_filename             = iv_file
      iv_actived              = iv_actived
      iv_sheet_name           = iv_sheet_name
      iv_begin_col            = iv_begin_col
      iv_begin_row            = iv_begin_row
      iv_end_col              = iv_end_col
      iv_end_row              = iv_end_row
      iv_rows                 = iv_rows
    TABLES
      et_tab                  = ct_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      sheet_not_found         = 3
      OTHERS                  = 4.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 3.
      WHEN OTHERS.
        MESSAGE e398(00) WITH TEXT-e03. "'上傳檔案時發生錯誤'.
    ENDCASE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM check_data  USING  p_mode.

  DATA: ls_cell_type    TYPE salv_s_int4_column.

  DATA: l_subrc TYPE sy-subrc.

  ls_cell_type-columnname = 'EXPAND'.
  ls_cell_type-value      = if_salv_c_cell_type=>hotspot.

  LOOP AT gt_data ASSIGNING <gs_data>.

    APPEND INITIAL LINE TO gt_alv ASSIGNING <gs_alv>.
    <gs_alv> = CORRESPONDING #( <gs_data> ).
    <gs_alv>-werks = p_werks.

    CASE p_mode.
*--------------------------------------------------------------------*
* 移到紅桌
*--------------------------------------------------------------------*
      WHEN '1'.

*  檢查M6單號、物料、工廠、儲存地點(From) 是否已有物料異動記錄
        SELECT SINGLE mjahr INTO @DATA(l_mjahr) FROM ztqm003_m6_a
          WHERE zzm6_no = @<gs_data>-zzm6_no
          AND   matnr = @<gs_data>-matnr
          AND   werks = @p_werks
          AND   umlgo = @<gs_data>-umlgo.
        IF ( sy-subrc = 0 AND l_mjahr IS NOT INITIAL ).
          g_message = TEXT-e06. "<M6單號> <物料> 已有移到紅桌紀錄
          REPLACE ALL OCCURRENCES OF '&1' IN g_message WITH <gs_data>-zzm6_no.
          REPLACE ALL OCCURRENCES OF '&2' IN g_message WITH <gs_data>-matnr.
          PERFORM collect_message USING g_message.
        ENDIF.

*  檢查物料號碼的物料狀態是否為使用中 ( MARC-MMSTA in {‘N’, ‘C’} )
        PERFORM check_marc.

*  檢查儲存地點(From) 是否在 Table T001L 定義
        SELECT SINGLE * INTO @DATA(l_t001l) FROM t001l
          WHERE lgort = @<gs_data>-umlgo.
        IF sy-subrc <> 0.
          g_message = TEXT-e08. "儲存地點(From) <儲存地點(From)> 代碼錯誤
          REPLACE ALL OCCURRENCES OF '&1' IN g_message WITH <gs_data>-umlgo.
          PERFORM collect_message USING g_message.
        ENDIF.

*  檢查移倉數量是否為數字型態
        PERFORM check_numeric USING <gs_data>-menge CHANGING l_subrc.
        IF l_subrc <> 0.
          g_message = TEXT-e09. "<M6單號> <物料> 移倉數量有非數字的資料
          REPLACE ALL OCCURRENCES OF '&1' IN g_message WITH <gs_data>-zzm6_no.
          REPLACE ALL OCCURRENCES OF '&1' IN g_message WITH <gs_data>-matnr.
          PERFORM collect_message USING g_message.
        ENDIF.

*  物料說明
        SELECT SINGLE maktx INTO <gs_alv>-maktx FROM makt WHERE matnr = <gs_data>-matnr AND spras = sy-langu.

*  基礎計量單位
        SELECT SINGLE meins INTO <gs_alv>-meins FROM mara WHERE matnr = <gs_data>-matnr.

*  供應商帳戶號碼
        PERFORM get_lifnr  USING <gs_alv>-matnr
                                 <gs_alv>-werks
                           CHANGING <gs_alv>-lifnr.

*--------------------------------------------------------------------*
* 瑕疵紀錄
*--------------------------------------------------------------------*
      WHEN '2'.

*   取得M6 Log
        SELECT SINGLE * INTO @DATA(ls_history) FROM ztqm003_m6_a
          WHERE zzm6_no = @<gs_data>-zzm6_no
          AND   matnr = @<gs_data>-matnr
          AND   werks = @p_werks
          AND   umlgo = @<gs_data>-umlgo.
        IF sy-subrc = 0.
*          <gs_alv> = CORRESPONDING #( ls_history ).
          <gs_alv>-UMLGO = ls_history-UMLGO.
          <gs_alv>-MENGE = ls_history-MENGE.
          <gs_alv>-MJAHR = ls_history-MJAHR.
          <gs_alv>-MBLNR = ls_history-MBLNR.
        ENDIF.

*  檢查M6單號、物料、工廠、儲存地點(From) 是否已有物料異動記錄
        IF <gs_alv>-mblnr IS INITIAL.
          g_message = TEXT-e10. "<M6單號> <物料> 尚未移到紅桌
          REPLACE ALL OCCURRENCES OF '&1' IN g_message WITH <gs_data>-zzm6_no.
          REPLACE ALL OCCURRENCES OF '&2' IN g_message WITH <gs_data>-matnr.
          PERFORM collect_message USING g_message.
        ENDIF.

*  檢查瑕疵數量是否為數字型態
        PERFORM check_numeric USING <gs_data>-anzfehler CHANGING l_subrc.
        IF l_subrc <> 0.
          g_message = TEXT-e11. "瑕疵數量有非數字的資料
          PERFORM collect_message USING g_message.
        ENDIF.

*  檢查工廠特定物料狀態是否為 ‘C’  (或 ‘N’ (MARC-MMSTA in {‘C’, ’N’} )
        PERFORM check_marc.

*  檢查物料號碼是否存已建立在 FLH 生產廠 (MARC-WERKS = ‘9110’)
        SELECT SINGLE * INTO @DATA(l_marc) FROM marc
          WHERE matnr = @<gs_data>-matnr
          AND   werks = '9110'.
        IF sy-subrc <> 0.
          g_message = TEXT-e12. "<物料號碼> 未在工廠 9110 維護
          REPLACE ALL OCCURRENCES OF '&1' IN g_message WITH <gs_data>-matnr.
          PERFORM collect_message USING g_message.
        ENDIF.

    ENDCASE.

*  檢查權限
    PERFORM authority_check.

    IF <gs_alv>-trerror IS INITIAL.
      <gs_alv>-icon = gv_led_green.
    ELSE.
      <gs_alv>-icon = gv_led_red.
    ENDIF.

    <gs_alv>-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).

    APPEND ls_cell_type TO <gs_alv>-cell_type.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form collect_message
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> L_MESSAGE
*&---------------------------------------------------------------------*
FORM collect_message  USING  p_message.

  CHECK <gs_alv> IS ASSIGNED.

  IF <gs_alv>-trerror IS INITIAL.
    <gs_alv>-trerror = p_message.
  ELSE.
    <gs_alv>-trerror = <gs_alv>-trerror && ` / ` &&  p_message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_numeric
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <GS_DATA>_MENGE
*&      <-- L_SUBRC
*&---------------------------------------------------------------------*
FORM check_numeric  USING    p_value
                    CHANGING p_subrc.

  IF p_value CO '0123456789. '.
    p_subrc = 0.
  ELSE.
    p_subrc = 4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alv_display .
  DATA: lv_icon TYPE string,
        lv_text TYPE string.

  DATA:
    lo_custom_container TYPE REF TO cl_gui_custom_container,
    lr_events           TYPE REF TO cl_salv_events_table,
    lr_event_handler    TYPE REF TO lcl_handle_events,
    lr_columns          TYPE REF TO cl_salv_columns.

  TRY.
      CREATE OBJECT lo_custom_container
        EXPORTING
          container_name = 'ALV'.

      cl_salv_table=>factory(
        EXPORTING
          r_container    = lo_custom_container
        IMPORTING
          r_salv_table   = go_salv_table
        CHANGING
          t_table        = gt_alv ).

      lr_events = go_salv_table->get_event( ).

      CREATE OBJECT lr_event_handler.
      SET HANDLER lr_event_handler->handle_added_function FOR lr_events.
      SET HANDLER lr_event_handler->handle_link_click     FOR lr_events.

      TRY.
          go_salv_table->get_columns( )->set_cell_type_column( 'CELL_TYPE' ).
        CATCH cx_salv_data_error.
      ENDTRY.
      TRY.
          go_salv_table->get_columns( )->set_optimize( 'X').
          lr_columns =  go_salv_table->get_columns( ).
          PERFORM set_columns_technical USING lr_columns.
        CATCH cx_salv_data_error.
      ENDTRY.

      TRY.
          lv_icon = lcl_handle_events=>gcs_toolbar-expall_icon.
          lv_text = TEXT-f01.
          go_salv_table->get_functions( )->add_function( name     = lcl_handle_events=>gcs_toolbar-expall_name
                                                         tooltip  = lcl_handle_events=>gcs_toolbar-expall_tooltip
                                                         text     = lv_text
                                                         icon     = lv_icon
                                                         position = lcl_handle_events=>gcs_toolbar-expall_position ).
        CATCH cx_salv_wrong_call.
        CATCH cx_salv_existing.
      ENDTRY.
      TRY.
          lv_icon = lcl_handle_events=>gcs_toolbar-colall_icon.
          lv_text = TEXT-f02.
          go_salv_table->get_functions( )->add_function( name     = lcl_handle_events=>gcs_toolbar-colall_name
                                                         tooltip  = lcl_handle_events=>gcs_toolbar-colall_tooltip
                                                         text     = lv_text
                                                         icon     = lv_icon
                                                         position = lcl_handle_events=>gcs_toolbar-colall_position ).
        CATCH cx_salv_wrong_call.
        CATCH cx_salv_existing.
      ENDTRY.

      TRY.
          go_salv_table->get_columns( )->get_column( 'EXPAND' )->set_alignment( if_salv_c_alignment=>centered ).
        CATCH cx_salv_not_found.
      ENDTRY.
      go_salv_table->get_display_settings( )->set_striped_pattern( abap_true ).
      go_salv_table->display( ).
    CATCH cx_salv_msg.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_columns_technical
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LR_COLUMNS
*&---------------------------------------------------------------------*
FORM set_columns_technical  USING ir_columns TYPE REF TO cl_salv_columns.

  DATA: lr_column TYPE REF TO cl_salv_column.

  TRY.
      lr_column = ir_columns->get_column( 'ICON' ).
      lr_column->set_output_length('5').
      lr_column->set_short_text('狀態').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = ir_columns->get_column( 'MENGE' ).
      lr_column->set_output_length('8').
      lr_column->set_short_text('數量').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = ir_columns->get_column( 'ANZFEHLER' ).
      lr_column->set_output_length('8').
      lr_column->set_short_text('瑕疵數量').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_marc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_marc .

  DATA: lr_mmsta TYPE RANGE OF mmsta.
  DATA: l_mmsta TYPE mmsta.

  IF lr_mmsta[] IS INITIAL.
    lr_mmsta[] = VALUE #( sign = 'I' option = 'EQ' ( low = 'N' )
                                                   ( low = 'C' )
                        ).
  ENDIF.

  CLEAR l_mmsta.
  SELECT SINGLE mmsta INTO @l_mmsta FROM marc
    WHERE matnr = @<gs_data>-matnr
    AND   werks = @p_werks.
  IF l_mmsta NOT IN lr_mmsta.
    g_message = TEXT-e07. "<物料> 狀態為 <MARC-MMSTA>
    REPLACE ALL OCCURRENCES OF '&1' IN g_message WITH <gs_data>-matnr.
    REPLACE ALL OCCURRENCES OF '&2' IN g_message WITH l_mmsta.
    PERFORM collect_message USING g_message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form authority_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM authority_check .

  AUTHORITY-CHECK OBJECT 'M_MSEG_WWA'
  FOR USER sy-uname
  ID 'WERKS' FIELD p_werks
  ID 'ACTVT' FIELD '01'.
  IF sy-subrc <> 0.
    g_message = TEXT-e13. "沒有工廠 <工廠> 物料異動權限
    REPLACE ALL OCCURRENCES OF '&1' IN g_message WITH p_werks.
    PERFORM collect_message USING g_message.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'M_MSEG_LGO'
  FOR USER sy-uname
  ID 'LGORT' FIELD p_lgort
  ID 'ACTVT' FIELD '01'.
  IF sy-subrc <> 0.
    g_message = TEXT-e14. "沒有儲存地點 <儲存地點 (To)> 物料異動權限
    REPLACE ALL OCCURRENCES OF '&1' IN g_message WITH p_lgort.
    PERFORM collect_message USING g_message.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'M_MSEG_BWA'
  FOR USER sy-uname
  ID 'BWART' FIELD '931'
  ID 'ACTVT' FIELD '01'.
  IF sy-subrc <> 0.
    g_message = TEXT-e15. "沒有異動類型 931 物料異動權限
    PERFORM collect_message USING g_message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_goodsmvt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_goodsmvt .

  DATA:
    header     TYPE bapi2017_gm_head_01,
    gmcode     TYPE bapi2017_gm_code,
    hdret      TYPE bapi2017_gm_head_ret,
    docnum     TYPE bapi2017_gm_head_ret-mat_doc,
    docyear    TYPE bapi2017_gm_head_ret-doc_year,
    bapi_ret   TYPE bapiret2,
    lf_sido_no TYPE zsido_no,
    lf_yyyymm  TYPE zordyyyymm,
    lf_uname   TYPE sy-uname,
    lt_item    TYPE TABLE OF bapi2017_gm_item_create,
    lt_return  TYPE TABLE OF bapiret2.

  gmcode = '04'.
*  SELECT SINGLE a~gmcode INTO gmcode
*    FROM t158g AS a INNER JOIN t158b AS b ON b~tcode = a~tcode
*    WHERE bwart = '931'.

  CLEAR: gt_qm003_m6. "M6 logs

  LOOP AT gt_alv ASSIGNING <gs_alv> WHERE trerror = ''
                                    AND    mblnr IS INITIAL.

    APPEND INITIAL LINE TO gt_qm003_m6 ASSIGNING <gs_qm003_m6> .
    <gs_qm003_m6> = CORRESPONDING #( <gs_alv> ).
    <gs_qm003_m6>-werks = p_werks.
    <gs_qm003_m6>-lgort = p_lgort.

*  init
    CLEAR: header,hdret,docnum,docyear,bapi_ret, lt_item, lt_return.

    header-pstng_date = sy-datum.
    header-doc_date   = sy-datum.
    header-ref_doc_no = <gs_alv>-zzm6_no.
    header-header_txt = <gs_alv>-lifnr.

    APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
    <ls_item>-material             = <gs_alv>-matnr.
    <ls_item>-material_long        = <gs_alv>-matnr.
    <ls_item>-plant                = <gs_alv>-werks.
    <ls_item>-move_type            = '931'.
    <ls_item>-entry_qnt            = <gs_alv>-menge.
    <ls_item>-entry_uom            = <gs_alv>-meins.
    <ls_item>-stge_loc             = <gs_alv>-umlgo.  "儲存地點
    <ls_item>-move_stloc           = p_lgort.         "收貨/發貨儲存地點


    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = header
        goodsmvt_code    = gmcode
        testrun          = ''
      IMPORTING
        goodsmvt_headret = hdret
        materialdocument = docnum
        matdocumentyear  = docyear
      TABLES
        goodsmvt_item    = lt_item
        return           = lt_return.

    LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CO 'EAX'.
    ENDLOOP.
    IF sy-subrc = 0.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      <gs_alv>-icon = gv_led_red.

      LOOP AT lt_return INTO DATA(ls_return) WHERE type CO 'EAX'.
        PERFORM collect_message USING ls_return-message.
      ENDLOOP.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      <gs_alv>-icon = gv_led_green.
      <gs_alv>-mblnr = <gs_qm003_m6>-mblnr = docnum.   "物料文件號碼
      <gs_alv>-mjahr = <gs_qm003_m6>-mjahr = docyear.  "物料文件年度

    ENDIF.

  ENDLOOP.

  IF gt_qm003_m6 IS NOT INITIAL.
    MODIFY ztqm003_m6_a FROM TABLE gt_qm003_m6.
    COMMIT WORK AND WAIT.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_LIFNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <GS_ALV>_LIFNR
*&---------------------------------------------------------------------*
FORM get_lifnr   USING p_matnr
                       p_werks
                 CHANGING p_lifnr.

  DATA: lt_ekbe TYPE TABLE OF ekbe.
  CLEAR lt_ekbe.

  SELECT * FROM ekbe INTO TABLE lt_ekbe
    WHERE vgabe = '1'
    AND   bwart IN ( '101', '109' )
    AND   xwsbr = '' "排除已迴轉
    AND   matnr = p_matnr
    AND   werks = p_werks.
  IF sy-subrc = 0.

    SORT lt_ekbe BY budat DESCENDING.
    READ TABLE lt_ekbe INTO DATA(ls_ekbe) INDEX 1.
    IF sy-subrc = 0.

      SELECT SINGLE lifnr FROM ekko INTO p_lifnr
        WHERE ebeln = ls_ekbe-ebeln.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form show_upload_process_report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM show_upload_process_report .

  IF gt_alv IS NOT INITIAL.

    CLEAR: gt_message.
    LOOP AT gt_alv ASSIGNING <gs_alv>.
      APPEND INITIAL LINE TO gt_message ASSIGNING <gs_message>.
      <gs_message> = CORRESPONDING #( <gs_alv> ).

      CLEAR <gs_message>-expand.
      CLEAR <gs_alv>-trerror.
    ENDLOOP.

*  Show Upload Process Report
    CALL SCREEN 100.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_PRUEFLOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_prueflos .

  IF gt_alv IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(lt_qals) FROM qals
      FOR ALL ENTRIES IN @gt_alv
      WHERE mjahr = @gt_alv-mjahr
      AND   mblnr = @gt_alv-mblnr.
    IF sy-subrc = 0.
      SORT lt_qals BY prueflos.
      DELETE ADJACENT DUPLICATES FROM lt_qals COMPARING prueflos.
      SORT lt_qals BY mjahr mblnr.
    ENDIF.
  ENDIF.


  LOOP AT gt_alv ASSIGNING <gs_alv>.

    READ TABLE lt_qals INTO DATA(ls_qals) WITH KEY mjahr = <gs_alv>-mjahr
                                                   mblnr = <gs_alv>-mblnr
                                          BINARY SEARCH.
    IF sy-subrc = 0.
      <gs_alv>-prueflos = ls_qals-prueflos.
    ENDIF.

    UPDATE ztqm003_m6_a
      SET prueflos = <gs_alv>-prueflos
    WHERE zzm6_no = <gs_alv>-zzm6_no
    AND   matnr = <gs_alv>-matnr
    AND   werks = <gs_alv>-werks.

    COMMIT WORK AND WAIT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form QF01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM qf01 .

  DATA:
    ls_qals TYPE qals.

  DATA:
    i_subsys  TYPE  qiwl-subsys.
  DATA:
    lt_qmifetab TYPE TABLE OF qmife,
    lt_qierrtab TYPE TABLE OF qierr.


  LOOP AT gt_alv ASSIGNING <gs_alv> WHERE prueflos IS NOT INITIAL
                                    AND   trerror = ''.

    CLEAR ls_qals.
    SELECT SINGLE * INTO ls_qals FROM qals
      WHERE prueflos = <gs_alv>-prueflos.


    CLEAR: i_subsys,lt_qmifetab, lt_qierrtab.

    i_subsys = '000001'.

    APPEND INITIAL LINE TO lt_qmifetab ASSIGNING FIELD-SYMBOL(<ls_qmifetab>).

    <ls_qmifetab> = CORRESPONDING #( <gs_alv> ).
    <ls_qmifetab>-satzart = 'Q90'.      "檢驗批缺點
    <ls_qmifetab>-posnr = 000001.       "項目排列號碼
    <ls_qmifetab>-fekat = '9'.          "瑕疵類型
    <ls_qmifetab>-fenam = sy-uname.
    <ls_qmifetab>-fedat = sy-datlo.
    <ls_qmifetab>-fzeit = sy-timlo.

    CALL FUNCTION 'QIRF_GET_DEFECT_ITEMS2'
      EXPORTING
*       I_SEND_PROTOCOL_MAIL       = ' '
        i_subsys     = i_subsys
      TABLES
        t_qmifetab   = lt_qmifetab  "Table for Defect Items
        t_qierrtab   = lt_qierrtab  "Error Log
      EXCEPTIONS
        no_authority = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.

      <gs_alv>-icon = gv_led_red.

      LOOP AT lt_qierrtab INTO DATA(ls_return) WHERE msgtype CO 'EAX'.
        PERFORM collect_message USING ls_return-msgtext.
      ENDLOOP.

    ELSE.

      <gs_alv>-icon = gv_led_green.

      UPDATE ztqm003_m6_a
      SET fegrp = <gs_alv>-fegrp
          fecod = <gs_alv>-fecod
          anzfehler = <gs_alv>-anzfehler
      WHERE zzm6_no = <gs_alv>-zzm6_no
      AND   matnr = <gs_alv>-matnr
      AND   werks = <gs_alv>-werks.

      COMMIT WORK AND WAIT.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form retrieve_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM retrieve_data .

  CLEAR gt_qm003_m6.
  SELECT * INTO TABLE gt_qm003_m6
    FROM ztqm003_m6_a
    WHERE zzm6_no IN s_zzm6
    AND   matnr IN s_matnr.
  IF sy-subrc <> 0.
    MESSAGE s398(00) WITH TEXT-e01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    CLEAR gt_alv2.
    LOOP AT gt_qm003_m6 INTO gs_qm003_m6.
      APPEND INITIAL LINE TO gt_alv2 ASSIGNING <gs_alv2>.
      <gs_alv2> = CORRESPONDING #( gs_qm003_m6 ).

*  物料說明
      SELECT SINGLE maktx INTO <gs_alv2>-maktx FROM makt WHERE matnr = <gs_alv2>-matnr AND spras = sy-langu.

*  基礎計量單位
      SELECT SINGLE meins INTO <gs_alv2>-meins FROM mara WHERE matnr = <gs_alv2>-matnr.

*  供應商帳戶號碼
      PERFORM get_lifnr  USING <gs_alv2>-matnr
                               <gs_alv2>-werks
                         CHANGING <gs_alv2>-lifnr.

    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form show_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM show_alv .

  PERFORM build_layout.
  PERFORM build_fieldcat.

  DATA(l_repid) = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = l_repid
*     i_bypassing_buffer = 'X'
*     i_callback_pf_status_set = 'MENU_SET'
*     i_callback_top_of_page = 'TOP-OF-PAGE'
*     i_html_height_top  = g_html_height_top
*     i_callback_user_command  = 'USER_COMMAND_EVENT'
      is_layout_lvc      = gs_layout
      it_fieldcat_lvc    = gt_fcat
      it_events          = gt_events
*     it_sort_lvc        = gt_sort
      i_default          = 'X'
      i_save             = 'A'
    TABLES
      t_outtab           = gt_alv2
    EXCEPTIONS
      OTHERS             = 0.
  CASE sy-subrc.
    WHEN 0.
      "ALV show success
    WHEN 1.
      "program_error
    WHEN 2.
      "other issue
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_layout
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_layout .

*  gs_layout-stylefname = 'STYLE'.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-zebra      = abap_true.
  gs_layout-detailinit = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_fieldcat .

  DATA: l_check(1)        TYPE c,
        l_decimals_out(6) TYPE c.

*<<  get fieldcatalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSQM003_2_A'
    CHANGING
      ct_fieldcat      = gt_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  menu_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RE_EXTAB   text
*----------------------------------------------------------------------*
FORM menu_set USING re_extab TYPE slis_t_extab.


ENDFORM.
