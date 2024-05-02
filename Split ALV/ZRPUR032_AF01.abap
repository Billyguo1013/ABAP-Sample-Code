*&---------------------------------------------------------------------*
*& INCLUDE          ZRPUR032_AF01
*&---------------------------------------------------------------------*
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
      default_file_name = '上傳範本-採購單'.
*      CONCATENATE default_file_name '\ZBSD001_Template' INTO default_file_name.
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
*& Form init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init .

  CLEAR: gt_poh, gt_poi.                  "Table data
  CLEAR: gt_dfies_tab_h, gt_dfies_tab_i.  "Table field description
  CLEAR: gt_alv, gt_alv_1, gt_alv_2.      "Screen data

  gv_filename = |{ gc_name }{ sy-datlo+2(6) }{ sy-timlo }|.  "Defailt upload file name

  PERFORM ddif_fieldinfo_get USING 'ZTPUR032_POH_A' CHANGING gt_dfies_tab_h.
  PERFORM ddif_fieldinfo_get USING 'ZTPUR032_POI_A' CHANGING gt_dfies_tab_i.

ENDFORM.
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

  sheetname = SWITCH #( p_mode WHEN 'H' THEN `表頭資料 (Header)`
                               WHEN 'I' THEN `表身資料 (Item)`
                               ELSE ``
                      ).

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
  DELETE lt_excel FROM 1 TO 4.

  gv_error_msg = TEXT-e02.
  REPLACE ALL OCCURRENCES OF '&1' IN gv_error_msg WITH sheetname.

  CASE p_mode.

    WHEN 'H'.

      IF lt_excel IS INITIAL.
        MESSAGE e398(00) WITH gv_error_msg.
        LEAVE LIST-PROCESSING.
      ENDIF.

      LOOP AT lt_excel INTO ls_excel.

        IF ls_excel IS INITIAL.
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO gt_poh ASSIGNING <gs_poh>.
        <gs_poh>-filename = gv_filename.
        <gs_poh>-dataseq =  ls_excel-f01.
        <gs_poh>-unsez =  ls_excel-f02.
        <gs_poh>-bukrs =  ls_excel-f03.
        <gs_poh>-bsart =  ls_excel-f04.
        <gs_poh>-lifnr =  ls_excel-f05.
        <gs_poh>-ekorg =  ls_excel-f06.
        <gs_poh>-ekgrp =  ls_excel-f07.
        <gs_poh>-parvw =  ls_excel-f08.
        <gs_poh>-partner =  ls_excel-f09.
        <gs_poh>-zterm =  ls_excel-f10.
        <gs_poh>-waers =  ls_excel-f11.
        <gs_poh>-wkurs =  ls_excel-f12.
        IF ls_excel-f13 IS NOT INITIAL.
          SPLIT ls_excel-f13 AT `/` INTO lv_year lv_month lv_day.
          <gs_poh>-bedat = |{ lv_year ALPHA = IN }{ lv_month ALPHA = IN }{ lv_day ALPHA = IN }|.
        ELSE.
          <gs_poh>-bedat = '00000000'.
        ENDIF.
        <gs_poh>-ihrez =  ls_excel-f14.

        "補0
        PERFORM alpha_in CHANGING: <gs_poh>-lifnr,
                                   <gs_poh>-partner.

      ENDLOOP.

      IF gt_poh IS INITIAL.
        MESSAGE e398(00) WITH gv_error_msg.
        LEAVE LIST-PROCESSING.
      ENDIF.

    WHEN 'I'.

      IF lt_excel IS INITIAL.
        MESSAGE e398(00) WITH gv_error_msg.
        LEAVE LIST-PROCESSING.
      ENDIF.

      LOOP AT lt_excel INTO ls_excel.

        IF ls_excel IS INITIAL.
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO gt_poi ASSIGNING <gs_poi>.
        <gs_poi>-filename = gv_filename.
        <gs_poi>-dataseq = ls_excel-f01.
        <gs_poi>-unsez = ls_excel-f02.
        <gs_poi>-lifnr = ls_excel-f03.
        <gs_poi>-ebelp = ls_excel-f04.
        <gs_poi>-knttp = ls_excel-f05.
        <gs_poi>-epstp = ls_excel-f06.
        <gs_poi>-matnr = ls_excel-f07.
        <gs_poi>-txz01 = ls_excel-f08.
        <gs_poi>-menge = ls_excel-f09.
        <gs_poi>-meins = ls_excel-f10.
        <gs_poi>-eeind = ls_excel-f11.
        <gs_poi>-werks = ls_excel-f12.
        <gs_poi>-lgort = ls_excel-f13.
        <gs_poi>-matkl = ls_excel-f14.
        <gs_poi>-repos = ls_excel-f15.
        <gs_poi>-webre = ls_excel-f16.
        <gs_poi>-mwskz = ls_excel-f17.
        <gs_poi>-netpr = ls_excel-f18.  "淨價 - 金額
*        <gs_poi>-waers = "Reference header currency
        <gs_poi>-peinh = 100.
        <gs_poi>-bprme = 'EA'.
        <gs_poi>-anln1 = ls_excel-f19.
        <gs_poi>-sakto = ls_excel-f20.  "總帳科目
        <gs_poi>-kostl = ls_excel-f21.
        <gs_poi>-prctr = ls_excel-f22.
        <gs_poi>-aufnr = ls_excel-f23.
        <gs_poi>-bstae = ls_excel-f24.

        "補0
        PERFORM alpha_in CHANGING: <gs_poi>-anln1,
                                   <gs_poi>-sakto,
                                   <gs_poi>-kostl,
                                   <gs_poi>-prctr,
                                   <gs_poi>-aufnr,
                                   <gs_poi>-bstae.

      ENDLOOP.

      IF gt_poi IS INITIAL.
        MESSAGE e398(00) WITH gv_error_msg.
        LEAVE LIST-PROCESSING.
      ENDIF.

  ENDCASE.

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
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_data .

  DATA: l_factor        TYPE isoc_factor.
  DATA: ls_cell_type    TYPE salv_s_int4_column.
  DATA: l_split_count   TYPE i.
  FIELD-SYMBOLS: <lv_object> TYPE trobjtype.

  ls_cell_type-columnname = 'EXPAND'.
  ls_cell_type-value      = if_salv_c_cell_type=>hotspot.

  LOOP AT gt_poh ASSIGNING <gs_poh>.

    gs_testrun = CORRESPONDING #( <gs_poh> ).

*  initialization
    gs_testrun-trerror = ''.

*  check exists
    SELECT SINGLE * INTO @DATA(wa) FROM ztpur032_poh_a  WHERE unsez = @<gs_poh>-unsez AND trstatus = 'S'.
    IF sy-subrc = 0.
      gv_error_msg = TEXT-e06.
      REPLACE ALL OCCURRENCES OF '&1' IN gv_error_msg WITH <gs_poh>-unsez.

      gv_error_msg = |{ gv_error_msg } { TEXT-h01 }|.
      PERFORM collect_message USING gv_error_msg.
    ENDIF.

*  check mandatory
    PERFORM mandatory_field_check_h USING : 'DATASEQ',
                                            'BUKRS',
                                            'BSART',
                                            'LIFNR',
                                            'EKORG',
                                            'EKGRP',
                                            'BEDAT',
                                            'IHREZ'.

    IF <gs_poh>-lifnr IS NOT INITIAL.
      PERFORM check_bp_exist USING <gs_poh>-lifnr TEXT-h01.
    ENDIF.

    IF <gs_poh>-waers IS INITIAL.
      SELECT SINGLE waers INTO <gs_poh>-waers FROM lfm1
        WHERE lifnr = <gs_poh>-lifnr AND ekorg = <gs_poh>-ekorg.
    ENDIF.

    IF <gs_poh>-bsart = 'ZIMC'.
      PERFORM mandatory_field_check_h USING : 'PARTNER'.
    ENDIF.

    IF <gs_poh>-partner IS NOT INITIAL.
      PERFORM check_bp_exist USING <gs_poh>-partner TEXT-h01.
    ENDIF.


    LOOP AT gt_poi ASSIGNING <gs_poi> WHERE filename = <gs_poh>-filename AND dataseq = <gs_poh>-dataseq.

*  check mandatory
      PERFORM mandatory_field_check_i USING : 'DATASEQ',
                                              'LIFNR',
                                              'EBELP',
*                                              'MATNR',
                                              'MENGE',
                                              'WERKS',
*                                              'IHREZ',
                                              'REPOS',
                                              'WEBRE'.

      IF <gs_poi>-knttp = 'A'. "當科目指派種類為A類

        PERFORM mandatory_field_check_i USING : 'ANLN1'.
        PERFORM mandatory_field_check_i USING : 'AUFNR'.

*  當科目指派種類為A類時，該欄位為必填欄位，需要先完成固定資產編碼的建立，才能開立採購訂單
        IF ( <gs_poh>-bukrs IS NOT INITIAL AND <gs_poi>-anln1 IS NOT INITIAL ).
          SELECT SINGLE * FROM anla INTO @DATA(ls_anla) WHERE bukrs = @<gs_poh>-bukrs AND anln1 = @<gs_poi>-anln1.
          IF sy-subrc <> 0.
            g_error = 'X'.
            gv_error_msg = TEXT-e10.
            REPLACE ALL OCCURRENCES OF '&1' IN gv_error_msg WITH <gs_poh>-bukrs.
            REPLACE ALL OCCURRENCES OF '&2' IN gv_error_msg WITH <gs_poi>-anln1.
            PERFORM collect_message USING gv_error_msg.
          ELSE.

            SELECT SINGLE * INTO @DATA(ls_anlz) FROM anlz
              WHERE bukrs = @ls_anla-bukrs
              AND   anln1 = @ls_anla-anln1
              AND   anln2 = @ls_anla-anln2.
            IF sy-subrc = 0.
              <gs_poi>-kostl = ls_anlz-kostl.
            ENDIF.
          ENDIF.
        ENDIF.

        IF ( <gs_poi>-matnr IS INITIAL AND <gs_poi>-txz01 IS INITIAL ).
          gv_error_msg = TEXT-e11.
          PERFORM collect_message USING gv_error_msg.
        ENDIF.

      ELSEIF <gs_poi>-knttp = 'K'. "當科目指派種類為K類
        PERFORM mandatory_field_check_i USING : 'SAKTO'.
        IF ( <gs_poi>-matnr IS INITIAL AND <gs_poi>-txz01 IS INITIAL ).
          gv_error_msg = TEXT-e11.
          PERFORM collect_message USING gv_error_msg.
        ENDIF.
      ENDIF.

      IF <gs_poi>-kostl IS NOT INITIAL. "成本中心
        "成本中心主檔資料
        SELECT SINGLE * INTO @DATA(ls_csks) FROM csks
          WHERE kostl = @<gs_poi>-kostl.
        IF sy-subrc = 0.
          "利潤中心主檔資料表
          SELECT SINGLE * INTO @DATA(ls_cepc) FROM cepc
            WHERE kokrs = @ls_csks-kokrs.
          IF sy-subrc = 0.
            <gs_poi>-prctr = ls_cepc-prctr.  "利潤中心
          ENDIF.
        ENDIF.
      ENDIF.

      IF <gs_poi>-lifnr IS NOT INITIAL.
        PERFORM check_bp_exist USING <gs_poi>-lifnr TEXT-h02.
      ENDIF.

*<< Define  Material  *********************************************************************
      IF <gs_poi>-matnr IS NOT INITIAL.
        PERFORM check_material_exist USING <gs_poi>-matnr.
*      ELSEIF <gs_poi>-matnr IS INITIAL AND <gs_poi>-txz01 IS NOT INITIAL.
*        SELECT SINGLE matnr INTO <gs_poi>-matnr FROM makt WHERE maktx = <gs_poi>-txz01.
      ENDIF.
*>> Define  Material  *********************************************************************

      IF <gs_poi>-matnr IS NOT INITIAL.
        SELECT SINGLE * FROM mara INTO @DATA(ls_mara) WHERE matnr = @<gs_poi>-matnr.
        IF sy-subrc = 0.
          <gs_poi>-matkl = ls_mara-matkl.
          <gs_poi>-meins = ls_mara-meins.
        ENDIF.
      ELSE.
        <gs_poi>-menge = 1.
      ENDIF.

*<< Define  採購資訊記錄 ******************************************************************
      IF <gs_poi>-infnr IS INITIAL.
        SELECT SINGLE * INTO @DATA(ls_eina) FROM eina
          WHERE matnr = @<gs_poi>-matnr
*          AND   matkl = @<gs_poi>-matkl
          AND   lifnr = @<gs_poi>-lifnr.
        IF sy-subrc = 0.
          <gs_poi>-infnr = ls_eina-infnr.
        ENDIF.
      ENDIF.
*>> Define  Material  *********************************************************************
      IF <gs_poi>-infnr IS NOT INITIAL.
        SELECT SINGLE * INTO @DATA(ls_eine) FROM eine
          WHERE infnr = @<gs_poi>-infnr
          AND   ekorg = @<gs_poh>-ekorg
          AND   werks = @<gs_poi>-werks.
        IF sy-subrc = 0.
          <gs_poi>-netpr = ls_eine-netpr.
          <gs_poi>-waers = ls_eine-waers.
          <gs_poi>-peinh = ls_eine-peinh.
          <gs_poi>-bprme = ls_eine-bprme.
        ELSE.
*          g_error = 'X'.
*          gv_error_msg = TEXT-e09.
*          PERFORM collect_message USING gv_error_msg.
        ENDIF.
      ELSE.
        PERFORM mandatory_field_check_i USING 'NETPR'.
        IF <gs_poi>-netpr IS NOT INITIAL.
*          <gs_poi>-netpr = <gs_poi>-netpr * 100.
          PERFORM mandatory_field_check_h USING 'WAERS'.
          <gs_poi>-waers = <gs_poh>-waers.

          IF <gs_poi>-waers IS NOT INITIAL.
            PERFORM currency_converting_factor USING <gs_poi>-waers CHANGING l_factor.
            IF l_factor <> 0.
              <gs_poi>-netpr = <gs_poi>-netpr / l_factor.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.

      "若無採購資訊記錄，淨價 - 金額則為必填欄位
      IF ( <gs_poi>-menge IS INITIAL AND <gs_poi>-meins IS INITIAL ).
        IF <gs_poi>-netpr IS INITIAL.
          g_error = 'X'.
          gv_error_msg = TEXT-e09.

          gv_error_msg = |{ gv_error_msg } { TEXT-h02 }|.
          PERFORM collect_message USING gv_error_msg.
        ENDIF.
      ENDIF.

*   確認控制
      PERFORM determine_bstae USING <gs_poh>-bsart
                              CHANGING <gs_poi>-bstae.

    ENDLOOP.

    IF gs_testrun-trerror IS INITIAL.
      gs_testrun-icon = gv_led_green.
    ELSE.
      gs_testrun-icon = gv_led_red.
    ENDIF.

    gs_testrun-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).

    APPEND ls_cell_type TO gs_testrun-cell_type.

    APPEND gs_testrun TO gt_testrun.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MANDATORY_FIELD_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0547   text
*----------------------------------------------------------------------*
FORM mandatory_field_check_h  USING  p_fieldname TYPE fieldname.

  FIELD-SYMBOLS: <value> TYPE any.
  FIELD-SYMBOLS: <fs_struc> TYPE any.
  DATA: lv_structure_name TYPE text30 VALUE '<GS_POH>'.
  DATA l_message TYPE string.

  "init
  UNASSIGN: <value>,
            <fs_struc>.

  TRANSLATE p_fieldname TO UPPER CASE.

  ASSIGN (lv_structure_name) TO <fs_struc>.
  ASSIGN COMPONENT p_fieldname OF STRUCTURE <fs_struc> TO <value>.

  CHECK <value> IS ASSIGNED.

  IF <value> IS INITIAL.
    TRY.
        DATA(ls_ddic) = gt_dfies_tab_h[ fieldname = p_fieldname ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR ls_ddic.
    ENDTRY.
    g_error = 'X'.
    l_message = TEXT-e04.
    REPLACE ALL OCCURRENCES OF '&1' IN l_message WITH ls_ddic-fieldtext.

    l_message = |{ l_message } { TEXT-h01 }|.
    PERFORM collect_message USING l_message.
  ENDIF.

ENDFORM.                    " MANDATORY_FIELD_CHECK
*&---------------------------------------------------------------------*
*&      Form  MANDATORY_FIELD_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0547   text
*----------------------------------------------------------------------*
FORM mandatory_field_check_i  USING  p_fieldname TYPE fieldname.

  FIELD-SYMBOLS: <value> TYPE any.
  FIELD-SYMBOLS: <fs_struc> TYPE any.
  DATA: lv_structure_name TYPE text30 VALUE '<GS_POI>'.
  DATA l_message TYPE string.

  "init
  UNASSIGN: <value>,
            <fs_struc>.

  TRANSLATE p_fieldname TO UPPER CASE.

  ASSIGN (lv_structure_name) TO <fs_struc>.
  ASSIGN COMPONENT p_fieldname OF STRUCTURE <fs_struc> TO <value>.

  CHECK <value> IS ASSIGNED.

  IF <value> IS INITIAL.
    TRY.
        DATA(ls_ddic) = gt_dfies_tab_i[ fieldname = p_fieldname ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR ls_ddic.
    ENDTRY.
    g_error = 'X'.
    l_message = SWITCH #( p_fieldname WHEN 'NETPR' THEN TEXT-e09
                                      ELSE TEXT-e04
                        ).
    CASE p_fieldname.
      WHEN 'NETPR'.
        "do nothing
      WHEN OTHERS.
        REPLACE ALL OCCURRENCES OF '&1' IN l_message WITH ls_ddic-fieldtext.
    ENDCASE.

    l_message = |{ l_message } { TEXT-h02 }|.
    PERFORM collect_message USING l_message.
  ENDIF.

ENDFORM.                    " MANDATORY_FIELD_CHECK
*&---------------------------------------------------------------------*
*& Form collect_message
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> L_MESSAGE
*&---------------------------------------------------------------------*
FORM collect_message  USING  p_message.

  IF gs_testrun-trerror IS INITIAL.
    gs_testrun-trerror = p_message.
  ELSE.
    gs_testrun-trerror = gs_testrun-trerror && ` / ` &&  p_message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DDIF_FIELDINFO_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- GT_DFIES_TAB
*&---------------------------------------------------------------------*
FORM ddif_fieldinfo_get  USING  pv_tabname TYPE ddobjname
                         CHANGING ct_dfies_tab  LIKE gt_dfies_tab_h.

  DATA: lt_dfies_tab TYPE TABLE OF dfies.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = pv_tabname
      langu          = 'M'
    TABLES
      dfies_tab      = lt_dfies_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
    ct_dfies_tab[] = lt_dfies_tab[].
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_bp_exist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <GS_POH>_LIFNR
*&---------------------------------------------------------------------*
FORM check_bp_exist  USING  p_lifnr
                             p_text.

  SELECT SINGLE partner INTO @DATA(lv_partner)
    FROM but000
    WHERE partner = @p_lifnr.
  IF sy-subrc <> 0.
    g_error = 'X'.
    gv_error_msg = TEXT-e07.
    REPLACE ALL OCCURRENCES OF '&1' IN gv_error_msg WITH p_lifnr.

    gv_error_msg = |{ gv_error_msg } { p_text }|.
    PERFORM collect_message USING gv_error_msg.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_material_exist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <GS_POI>_MATNR
*&---------------------------------------------------------------------*
FORM check_material_exist  USING p_matnr.

  SELECT SINGLE * INTO @DATA(ls_mara)
    FROM mara
    WHERE matnr = @p_matnr.
  IF sy-subrc <> 0.
    g_error = 'X'.
    gv_error_msg = TEXT-e08.
    REPLACE ALL OCCURRENCES OF '&1' IN gv_error_msg WITH p_matnr.

    gv_error_msg = |{ gv_error_msg } { TEXT-h02 }|.
    PERFORM collect_message USING gv_error_msg.

  ELSE.

    IF <gs_poi>-txz01 IS INITIAL.
      SELECT SINGLE maktx INTO <gs_poi>-txz01 FROM makt
        WHERE matnr = ls_mara-matnr
        AND   spras = sy-langu.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form show_test_run_report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM show_test_run_report .

* If have error data
  IF line_exists( gt_testrun[ icon = gv_led_red ] ).

*  Build ALV Data
    gt_alv = CORRESPONDING #( gt_testrun ).
    LOOP AT gt_alv ASSIGNING <alv>.
      CLEAR <alv>-trerror.
    ENDLOOP.

*  Show Test Run report
    CALL SCREEN 100.

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
          go_salv_table->get_columns( )->set_optimize( '').
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
      lr_column = ir_columns->get_column( 'FILENAME' ).
      lr_column->set_output_length('25').
      lr_column->set_short_text('檔案名稱').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = ir_columns->get_column( 'DATASEQ' ).
      lr_column->set_output_length('5').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = ir_columns->get_column( 'UNSEZ' ).
      lr_column->set_output_length('12').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = ir_columns->get_column( 'PO_NO' ).
      lr_column->set_output_length('10').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = ir_columns->get_column( 'BSART' ).
      lr_column->set_output_length('8').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = ir_columns->get_column( 'EXPAND' ).
      lr_column->set_output_length('10').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = ir_columns->get_column( 'TRERROR' ).
      lr_column->set_output_length('30').
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_po
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM create_po  USING  p_testrun.

  DATA: lv_ebelp    TYPE ebelp,
        lv_matnr    TYPE matnr,
        lv_trstatus TYPE ztrstatus.

  DATA: exppurchaseorder LIKE  bapimepoheader-po_number,
        lt_return        TYPE TABLE OF bapiret2.

  DATA: lv_row   TYPE bapiret2-row.
  DATA: lt_eket  TYPE TABLE OF eket.

  CHECK NOT line_exists( gt_testrun[ icon = gv_led_red ] ).

  gv_testrun = p_testrun.

  LOOP AT gt_poh ASSIGNING <gs_poh> WHERE ebeln IS INITIAL.

*  initialization
    CLEAR: header, headerx, item, itemx, poschedule, poschedulex, poaccount, poaccountx, popartner, lt_return.

*--------------------------------------------------------------------*
*  HEADER DATA FOR PO
*--------------------------------------------------------------------*
    header-comp_code = <gs_poh>-bukrs.     "公司代碼
    header-doc_type = <gs_poh>-bsart.      "訂單類型(採購)
    header-doc_date = <gs_poh>-bedat.      "文件日期
    header-vendor = <gs_poh>-lifnr.        "供應商
    header-purch_org = <gs_poh>-ekorg.     "採購組織
    header-pur_group = <gs_poh>-ekgrp.     "採購群組
    header-creat_date = sy-datlo.

    headerx-comp_code = c_x.
    headerx-doc_type = c_x.
    headerx-doc_date = c_x.
    headerx-vendor = c_x.
    headerx-purch_org = c_x.
    headerx-pur_group = c_x.
    headerx-creat_date = c_x.

    IF <gs_poh>-waers IS NOT INITIAL.
      header-currency = <gs_poh>-waers.    "幣別
      headerx-currency = c_x.
    ENDIF.
    IF <gs_poh>-zterm IS NOT INITIAL.     "付款條件
      header-pmnttrms = <gs_poh>-zterm.
      headerx-pmnttrms = c_x.
    ENDIF.
    IF <gs_poh>-unsez IS NOT INITIAL.
      header-our_ref = <gs_poh>-unsez.     "我們的參考
      headerx-our_ref = c_x.
    ENDIF.
    IF <gs_poh>-ihrez IS NOT INITIAL.
      header-ref_1 = <gs_poh>-ihrez.       "您的參考
      headerx-ref_1 = c_x.
    ENDIF.
    IF <gs_poh>-wkurs IS NOT INITIAL.
      header-exch_rate = <gs_poh>-wkurs.   "匯率
      headerx-exch_rate = c_x.
    ENDIF.

*--------------------------------------------------------------------*
*  Partner
*--------------------------------------------------------------------*
    IF ( <gs_poh>-parvw IS NOT INITIAL AND <gs_poh>-partner IS NOT INITIAL ).
      APPEND INITIAL LINE TO popartner ASSIGNING FIELD-SYMBOL(<ls_popartner>).
      <ls_popartner>-partnerdesc = <gs_poh>-parvw.
      <ls_popartner>-buspartno = <gs_poh>-partner.
      <ls_popartner>-langu = sy-langu.
    ENDIF.

    lv_ebelp = 0.
    LOOP AT gt_poi ASSIGNING <gs_poi> WHERE filename = <gs_poh>-filename AND dataseq = <gs_poh>-dataseq.

      APPEND INITIAL LINE TO item ASSIGNING FIELD-SYMBOL(<ls_item>).
      APPEND INITIAL LINE TO itemx ASSIGNING FIELD-SYMBOL(<ls_itemx>).

      lv_ebelp = lv_ebelp + 10.

*---------------------------------------------------------------------*
*POPULATE ITEM DATA.
*---------------------------------------------------------------------*
      <ls_item>-po_item = <gs_poi>-ebelp.                "項目號碼
      <ls_item>-acctasscat = <gs_poi>-knttp.             "科目指派種類
      <ls_item>-item_cat = <gs_poi>-epstp.               "採購文件項目種類
      <ls_item>-plant = <gs_poi>-werks.                  "工廠
      <ls_item>-quantity = <gs_poi>-menge.               "採購單數量
      <ls_item>-po_unit = <gs_poi>-meins.                "採購單計量單位
      <ls_item>-info_rec = <gs_poi>-infnr.               "採購資訊記錄
      <ls_item>-ir_ind = <gs_poi>-repos.                 "標識：發票收據
      <ls_item>-gr_basediv = <gs_poi>-webre.             "標識：GR基礎IV

      IF <gs_poi>-sakto IS NOT INITIAL.
        <ls_item>-gl_account = <gs_poi>-sakto.           "總帳科目號碼
        <ls_itemx>-gl_account = c_x.
      ENDIF.

      IF <gs_poi>-kostl IS NOT INITIAL.
        <ls_item>-costcenter = <gs_poi>-kostl.           "成本中心
        <ls_itemx>-costcenter = c_x.
      ENDIF.

      IF <gs_poi>-matnr IS NOT INITIAL.
        <ls_item>-material = <gs_poi>-matnr.               "物料號碼
        <ls_item>-material_long = <gs_poi>-matnr.          "物料號碼
        <ls_itemx>-material = c_x.
        <ls_itemx>-material_long = c_x.
      ENDIF.

      IF <gs_poi>-txz01 IS NOT INITIAL.
        <ls_item>-short_text = <gs_poi>-txz01.           "短文
        <ls_itemx>-short_text = c_x.
      ENDIF.

      IF <gs_poi>-matkl IS NOT INITIAL.
        <ls_item>-matl_group = <gs_poi>-matkl .
        <ls_itemx>-matl_group = c_x.
      ENDIF.

      <ls_item>-conf_ctrl = <gs_poi>-bstae.              "確認控制
      <ls_item>-net_price = <gs_poi>-netpr.              "採購資訊記錄中的淨價
      <ls_item>-price_unit = <gs_poi>-peinh.             "價格單位
      <ls_item>-orderpr_un = <gs_poi>-bprme.             "採購單的計價單位〈採購〉

      <ls_itemx>-po_item = <gs_poi>-ebelp.
      <ls_itemx>-acctasscat = c_x.
      <ls_itemx>-item_cat = c_x.
      <ls_itemx>-plant = c_x.
      <ls_itemx>-quantity = c_x.
      <ls_itemx>-po_unit = c_x.
      <ls_itemx>-info_rec = c_x.
      <ls_itemx>-ir_ind = c_x.
      <ls_itemx>-gr_basediv = c_x.
      <ls_itemx>-conf_ctrl = c_x.
      <ls_itemx>-net_price = c_x.
      <ls_itemx>-price_unit = c_x.
      <ls_itemx>-orderpr_un = c_x.

      IF <gs_poi>-mwskz IS NOT INITIAL.
        <ls_item>-tax_code = <gs_poi>-mwskz.            "營業稅代碼
        <ls_itemx>-tax_code = c_x.
      ENDIF.

*---------------------------------------------------------------------*
* Delivery Schedule
*---------------------------------------------------------------------*
      APPEND INITIAL LINE TO poschedule ASSIGNING FIELD-SYMBOL(<ls_poschedule>).
      APPEND INITIAL LINE TO poschedulex ASSIGNING FIELD-SYMBOL(<ls_poschedulex>).

      <ls_poschedule>-po_item    = <ls_item>-po_item.
      <ls_poschedulex>-po_item   = <ls_item>-po_item.
      <ls_poschedule>-sched_line = 1.
      <ls_poschedulex>-sched_line = 1.

      IF NOT <gs_poi>-eeind IS INITIAL.
        WRITE <gs_poi>-eeind TO <ls_poschedule>-delivery_date.
      ELSE.
        "由物料主檔帶出
*        WRITE sy-datum TO <ls_poschedule>-delivery_date.
      ENDIF.

      <ls_poschedulex>-po_itemx = c_x.
      <ls_poschedulex>-sched_linex = c_x.
      <ls_poschedulex>-delivery_date = c_x.

*---------------------------------------------------------------------*
* Account Assignment Fields
*---------------------------------------------------------------------*
      APPEND INITIAL LINE TO poaccount ASSIGNING FIELD-SYMBOL(<ls_poaccount>).
      APPEND INITIAL LINE TO poaccountx ASSIGNING FIELD-SYMBOL(<ls_poaccountx>).

      <ls_poaccount>-po_item =  <ls_item>-po_item.
      <ls_poaccountx>-po_item =  <ls_item>-po_item.

      IF <gs_poi>-sakto IS NOT INITIAL.
        <ls_poaccount>-gl_account = <gs_poi>-sakto.   "總帳科目
        <ls_poaccountx>-gl_account = c_x.
      ENDIF.

      IF <gs_poi>-kostl IS NOT INITIAL.
        <ls_poaccount>-costcenter = <gs_poi>-kostl.   "成本中心
        <ls_poaccountx>-costcenter = c_x.
      ENDIF.

      IF <gs_poi>-prctr IS NOT INITIAL.
        <ls_poaccount>-profit_ctr = <gs_poi>-prctr.   "利潤中心
        <ls_poaccountx>-profit_ctr = c_x.
      ENDIF.

      IF <gs_poi>-anln1 IS NOT INITIAL.
        <ls_poaccount>-asset_no = <gs_poi>-anln1.   "主資產
        <ls_poaccountx>-asset_no = c_x.
      ENDIF.

      IF <gs_poi>-aufnr IS NOT INITIAL.
        <ls_poaccount>-orderid = <gs_poi>-aufnr.    "訂單號
        <ls_poaccountx>-orderid = c_x.
      ENDIF.

    ENDLOOP.

*---------------------------------------------------------------------*
*BAPI CALL
*---------------------------------------------------------------------*
    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = header
        poheaderx        = headerx
        testrun          = gv_testrun
      IMPORTING
        exppurchaseorder = exppurchaseorder
      TABLES
        return           = lt_return
        poitem           = item
        poitemx          = itemx
        poschedule       = poschedule
        poschedulex      = poschedulex
        poaccount        = poaccount
        poaccountx       = poaccountx
        popartner        = popartner.

    LOOP AT lt_return INTO DATA(ls_return) WHERE type CO 'EAX'.
    ENDLOOP.
    IF sy-subrc <> 0.

      IF gv_testrun = ''. "it's real run

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        lv_trstatus = 'S'.

*<<  Find EINDT (項目交貨日期) from EKET
        CLEAR lt_eket.
        DO 3 TIMES.
          SELECT * FROM eket INTO TABLE lt_eket WHERE ebeln = exppurchaseorder.
          IF sy-subrc <> 0.
            WAIT UP TO `0.1` SECONDS.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
        IF lt_eket IS NOT INITIAL.
          LOOP AT gt_poi ASSIGNING <gs_poi> WHERE filename = <gs_poh>-filename AND dataseq = <gs_poh>-dataseq.
            READ TABLE lt_eket INTO DATA(ls_eket) WITH KEY ebelp = <gs_poi>-ebelp.
            IF sy-subrc = 0.
              WRITE ls_eket-eindt TO <gs_poi>-eeind DD/MM/YYYY.
            ENDIF.
          ENDLOOP.
        ENDIF.

      ELSE. "it's test run

        lv_trstatus = ' '.

      ENDIF.

      <gs_poh>-ebeln = exppurchaseorder.
      <gs_poh>-trstatus = lv_trstatus.
      <gs_poh>-trerror = ''.
      LOOP AT gt_poi ASSIGNING <gs_poi> WHERE filename = <gs_poh>-filename AND dataseq = <gs_poh>-dataseq.
        <gs_poi>-ebeln = exppurchaseorder.
        <gs_poi>-trstatus = lv_trstatus.
        <gs_poi>-trerror = ''.
      ENDLOOP.

    ELSE.

      IF gv_testrun = ''.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      <gs_poh>-trstatus = 'E'.
      LOOP AT lt_return INTO ls_return WHERE type CO 'EAX' AND parameter = 'POHEADER'.
        IF <gs_poh>-trerror IS INITIAL..
          <gs_poh>-trerror = ls_return-message.
        ELSE.
          <gs_poh>-trerror = <gs_poh>-trerror && ` ; ` && ls_return-message.
        ENDIF.
      ENDLOOP.

      lv_row = 0.
      LOOP AT gt_poi ASSIGNING <gs_poi> WHERE filename = <gs_poh>-filename AND dataseq = <gs_poh>-dataseq.
        lv_row = lv_row + 1.
        LOOP AT lt_return INTO ls_return WHERE type CO 'EAX' AND ( parameter = 'POITEM' OR parameter = 'POSCHEDULE' OR parameter = 'POACCOUNT' ) AND row = lv_row.
          <gs_poi>-trstatus = 'E'.
          IF <gs_poi>-trerror IS INITIAL.
            <gs_poi>-trerror = ls_return-message.
          ELSE.
            <gs_poi>-trerror = <gs_poi>-trerror && ` ; ` && ls_return-message.
          ENDIF.
        ENDLOOP.
        IF sy-subrc <> 0.
          <gs_poi>-trstatus = ' '.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDLOOP.
  IF sy-subrc = 0.
    IF ( gt_poh IS NOT INITIAL AND gt_poi IS NOT INITIAL ).
      MODIFY ztpur032_poh_a FROM TABLE gt_poh.
      MODIFY ztpur032_poi_a FROM TABLE gt_poi.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF gv_testrun = 'X'.

* If have error data
    IF line_exists( gt_poh[ trstatus = 'E' ] ).
*  Show Upload Result Report
      PERFORM show_upload_result_report.

    ELSE.

      LOOP AT gt_poh TRANSPORTING NO FIELDS WHERE ebeln IS INITIAL.
      ENDLOOP.
      IF sy-subrc = 0.
        PERFORM show_popup_window CHANGING g_answer.  "Show Popup Window
        IF g_answer = '1'.
          PERFORM create_po USING ''.                 "Create PO (Real Run)
        ENDIF.
      ELSE.
        PERFORM create_goodsmvt.
      ENDIF.

    ENDIF.

  ELSE.

    PERFORM create_goodsmvt.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form show_popup_window
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM show_popup_window CHANGING c_answer.

  DATA: answer(1) TYPE c.
  DATA: text1 TYPE string,
        text2 TYPE string.

  CHECK NOT line_exists( gt_poh[ trstatus = 'E' ] ).

  text1 = TEXT-001. "資料已通過檢查。
  text2 = TEXT-002. "您是否要進行採購單建立?
  DATA(text_question) =  |{ text1 }| & |{ text2 }| .

*<< Show Popup message
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-f03
      text_question         = text_question
      text_button_1         = TEXT-f04
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = TEXT-f05
      icon_button_2         = 'ICON_CANCEL'
      display_cancel_button = ' '
    IMPORTING
      answer                = answer   "1 or 2
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    c_answer = answer.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form show_Upload_Result_Report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM show_upload_result_report .

  gt_alv_1 = CORRESPONDING #( gt_poh ).
  CALL SCREEN 200.

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

  SELECT * INTO TABLE gt_poh
    FROM ztpur032_poh_a
    WHERE lifnr IN s_lifnr
    AND   unsez IN s_unsez
    AND   ebeln IN s_ebeln
    AND   ekorg IN s_ekorg
    AND   ekgrp IN s_ekgrp
    AND   bedat IN s_bedat.
  IF sy-subrc <> 0.
    MESSAGE s398(00) WITH TEXT-e01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT * INTO TABLE gt_poi
      FROM ztpur032_poi_a
      FOR ALL ENTRIES IN gt_poh
      WHERE filename = gt_poh-filename
      AND   dataseq = gt_poh-dataseq
      AND   matnr IN s_matnr.
    IF sy-subrc <> 0..
      MESSAGE s398(00) WITH TEXT-e01 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  LOOP AT gt_poh ASSIGNING <gs_poh>.
    DATA(lv_tabix) = sy-tabix.
    IF NOT line_exists( gt_poi[ filename = <gs_poh>-filename  dataseq = <gs_poh>-dataseq ] ).
      DELETE gt_poh INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

  IF gt_poh IS INITIAL.
    MESSAGE s398(00) WITH TEXT-e01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_display2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alv_display2 .

  DATA: lr_column_1 TYPE REF TO cl_salv_column_list,
        lr_column_2 TYPE REF TO cl_salv_column_list.

  DATA: lr_events        TYPE REF TO cl_salv_events_table,
        lr_event_handler TYPE REF TO lcl_handle_events.

* Create ALV screens

  TRY.

* Splitter - it is neccessary to specify the default_screen as parent
      gr_splitter = NEW #( parent = cl_gui_container=>default_screen
                           no_autodef_progid_dynnr = abap_true
                           rows = 2
                           columns = 1 ).


* Container 1 for header
      gr_container_1 = gr_splitter->get_container( row = 1
                                                   column = 1 ).

* Container 2 for detail
      gr_container_2 = gr_splitter->get_container( row = 2
                                                   column = 1 ).


*<< ALV1- gt_alv_1 is the table with the data
      cl_salv_table=>factory( EXPORTING r_container = gr_container_1
                              IMPORTING r_salv_table = gr_salv_1
                              CHANGING t_table = gt_alv_1 ).

* Set ALV functions - should you wish to include any
      DATA(lr_salv_func_1) = gr_salv_1->get_functions( ).
      lr_salv_func_1->set_all( abap_true ).

* Set display settings as usual
      DATA(lr_display_1) = gr_salv_1->get_display_settings( ).
      lr_display_1->set_list_header('採購單（表頭）上傳記錄').

* Selection - set as usual
      gr_salv_1->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>multiple ).

* Layout - set as usual
      DATA(ls_key_1) = VALUE salv_s_layout_key( report = sy-cprog
                                                handle = '0001' ).
      DATA(lr_layout_1) = gr_salv_1->get_layout( ).
      lr_layout_1->set_key( ls_key_1 ).


*<< ALV_2 - details - gt_alv_2 is the table with data for the second ALV
      cl_salv_table=>factory( EXPORTING r_container = gr_container_2
                              IMPORTING r_salv_table = gr_salv_2
                              CHANGING t_table = gt_alv_2 ).

* ALV functions
      gr_salv_2->get_functions( )->set_all( abap_true ).

* Display settings - set as usual
      DATA(lr_display_2) = gr_salv_2->get_display_settings( ).
      lr_display_2->set_list_header('採購單（表身）上傳記錄').

* Layout - set as usual
      DATA(ls_key_2) = VALUE salv_s_layout_key( report = sy-cprog
                                                handle = '0002' ).


* Set hotspot on particular column
      DATA(lr_columns_1) = gr_salv_1->get_columns( ).
      lr_columns_1->set_optimize( abap_true ).
      lr_column_1 ?= lr_columns_1->get_column('DATASEQ').
      lr_column_1->set_cell_type( if_salv_c_cell_type=>hotspot ).

      lr_column_1 ?= lr_columns_1->get_column('MANDT').
      lr_column_1->set_technical( if_salv_c_bool_sap=>true ).

      DATA(lr_columns_2) = gr_salv_2->get_columns( ).
      lr_columns_2->set_optimize( abap_true ).

      lr_column_2 ?= lr_columns_2->get_column('MANDT').
      lr_column_2->set_technical( if_salv_c_bool_sap=>true ).

* ALV events - the implementation of the on link click event follows further in the post
      lr_events = gr_salv_1->get_event( ).
      CREATE OBJECT lr_event_handler.
      SET HANDLER lr_event_handler->handle_link_click  FOR lr_events.
      SET HANDLER lr_event_handler->handle_double_click  FOR lr_events.

*<< Display ALV_1
      gr_salv_1->display( ).

*<< Display ALV_2,
      gr_salv_2->display( ).


    CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error cx_salv_access_error INTO DATA(lx_alv).
      DATA(lv_message) = lx_alv->get_text( ).
      MESSAGE e398(00) WITH lv_message.
  ENDTRY.


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

*  gmcode = '01'.
  SELECT SINGLE a~gmcode INTO gmcode
    FROM t158g AS a INNER JOIN t158b AS b ON b~tcode = a~tcode
    WHERE bwart = '101'.

  LOOP AT gt_poh ASSIGNING <gs_poh> WHERE ebeln IS NOT INITIAL.
*  購單類型（ZTPUR032_POH-BSART）為‘ZDPR’的HB Build採購單，
*  程式判斷物料文件號碼（ZTPUR032_POH- MBLNR）是否為空
*  若為空，則需完成入庫作業（TCODE：MIGO ，異動類型：MVT923）
    IF NOT ( <gs_poh>-bsart = 'ZDPR' AND <gs_poh>-mblnr IS INITIAL ).
      CONTINUE.
    ENDIF.

    CLEAR: header,hdret,docnum,docyear,bapi_ret, lt_item, lt_return.

    header-pstng_date = sy-datum.
    header-doc_date   = <gs_poh>-bedat.
    header-ref_doc_no = 'R01'.

    LOOP AT gt_poi ASSIGNING <gs_poi> WHERE filename = <gs_poh>-filename AND dataseq = <gs_poh>-dataseq.
      APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
      <ls_item>-material             = <gs_poi>-matnr.
      <ls_item>-material_long        = <gs_poi>-matnr.
      <ls_item>-plant                = <gs_poi>-werks.
      <ls_item>-stge_loc             = '1111'.   "fixed value '1111', 20240307
      <ls_item>-move_type            = '101'.    "changed 923 to 101 , 20240307
      <ls_item>-entry_qnt            = <gs_poi>-menge.
      <ls_item>-entry_uom            = <gs_poi>-meins.
      <ls_item>-po_number            = <gs_poi>-ebeln.
      <ls_item>-po_item              = <gs_poi>-ebelp.
      <ls_item>-mvt_ind              = 'B'.     "採購單的物料異動
*      <ls_item>-costcenter           = <gs_poi>-kostl.    "成本中心
*      <ls_item>-gl_account           = <gs_poi>-sakto.    "總帳科目號碼
      <ls_item>-profit_ctr           = <gs_poi>-prctr.    "利潤中心
    ENDLOOP.

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

    READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
    IF sy-subrc = 0.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      <gs_poh>-trstatus = 'E'.
      <gs_poh>-trerror = `Post Goods Movements Error: `.
      <gs_poh>-trerror = <gs_poh>-trerror && ls_return-message.

      UPDATE ztpur032_poh_a
      SET trstatus = <gs_poh>-trstatus
          trerror = <gs_poh>-trerror
      WHERE filename = <gs_poh>-filename
      AND   dataseq = <gs_poh>-dataseq.

      COMMIT WORK AND WAIT.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      <gs_poh>-mblnr = hdret-mat_doc.

      UPDATE ztpur032_poh_a
      SET mblnr = <gs_poh>-mblnr
          trerror = 'S'
      WHERE filename = <gs_poh>-filename
      AND   dataseq = <gs_poh>-dataseq.

      COMMIT WORK AND WAIT.

    ENDIF.
  ENDLOOP.

  PERFORM show_upload_result_report.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_error_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_error_data .

  SELECT * INTO TABLE @gt_poh
    FROM ztpur032_poh_a
    WHERE ( trstatus = '' OR trstatus = 'E' )
    AND   filename IN @s_file.
  IF sy-subrc <> 0.
    MESSAGE s398(00) WITH TEXT-e01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT * INTO TABLE @gt_poi
      FROM ztpur032_poi_a
      FOR ALL ENTRIES IN @gt_poh
      WHERE filename = @gt_poh-filename
      AND   dataseq = @gt_poh-dataseq.
    IF sy-subrc <> 0.
      MESSAGE s398(00) WITH TEXT-e01 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  LOOP AT gt_poh ASSIGNING <gs_poh>.

*<< inititialize status
    CLEAR: <gs_poh>-trstatus, <gs_poh>-trerror.

    LOOP AT gt_poi ASSIGNING <gs_poi> WHERE filename = <gs_poh>-filename
                                        AND dataseq = <gs_poh>-dataseq.

*<< inititialize status
      CLEAR: <gs_poi>-trstatus, <gs_poi>-trerror.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form refresh_alv_2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ROW
*&---------------------------------------------------------------------*
FORM refresh_alv_2  USING  p_row.

  CLEAR gt_alv_2.
  READ TABLE gt_alv_1 ASSIGNING <gs_alv_1> INDEX p_row.
  IF sy-subrc = 0.
    LOOP AT gt_poi INTO gs_poi WHERE filename = <gs_alv_1>-filename AND dataseq = <gs_alv_1>-dataseq.
      APPEND INITIAL LINE TO gt_alv_2 ASSIGNING <gs_alv_2>.
      <gs_alv_2> = CORRESPONDING #( gs_poi ).
    ENDLOOP.
  ENDIF.

*<< It's stardand bug...do disable and then re-enable optimization columns to enable optimize cloumns method
  DATA(lr_columns_2) = gr_salv_2->get_columns( ).
  lr_columns_2->set_optimize( abap_false ).
  lr_columns_2->set_optimize( abap_true ).

  gr_salv_2->refresh( refresh_mode = if_salv_c_refresh=>full ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALPHA_IN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <GS_POI>_ANLN1
*&---------------------------------------------------------------------*
FORM alpha_in  CHANGING p_value.

  p_value = |{ p_value ALPHA = IN } |.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form determine_BSTAE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <GS_POI>_BSTAE
*&---------------------------------------------------------------------*
FORM determine_bstae  USING p_bsart
                       CHANGING p_bstae.

  CHECK p_bstae IS INITIAL.

  SELECT SINGLE bstae FROM ztpur032_v1_a INTO p_bstae
    WHERE bsart = p_bsart.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form currency_converting_factor
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <GS_POI>_WAERS
*&      <-- L_FACTOR
*&---------------------------------------------------------------------*
FORM currency_converting_factor  USING    p_waers
                                 CHANGING c_factor TYPE  isoc_factor.

  CALL FUNCTION 'CURRENCY_CONVERTING_FACTOR'
    EXPORTING
      currency          = CONV tcurr_curr( p_waers )
    IMPORTING
      factor            = c_factor
    EXCEPTIONS
      too_many_decimals = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
