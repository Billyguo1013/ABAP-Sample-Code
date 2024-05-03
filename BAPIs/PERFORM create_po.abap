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
