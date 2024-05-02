*&---------------------------------------------------------------------*
*&  Include           ZRCRM0001_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_f4 .

  TYPES: BEGIN OF ty_bukrs,
          bukrs TYPE bukrs,
         END OF ty_bukrs.
  DATA: lt_bukrs TYPE TABLE OF ty_bukrs,
        ls_bukrs TYPE ty_bukrs.
  DATA: r_bukrs  TYPE TABLE OF ddshretval WITH HEADER LINE.

  CLEAR ls_bukrs.
  ls_bukrs-bukrs  = 'WEHQ'.
  APPEND ls_bukrs TO lt_bukrs.

  CLEAR ls_bukrs.
  ls_bukrs-bukrs  = 'NJP1'.
  APPEND ls_bukrs TO lt_bukrs.

  CLEAR ls_bukrs.
  ls_bukrs-bukrs  = 'NSG1'.
  APPEND ls_bukrs TO lt_bukrs.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LOW'
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'P_SUPPL'
      window_title    = 'SUPPLIER'
      value_org       = 'S'
    TABLES
      value_tab       = lt_bukrs[]
      return_tab      = r_bukrs[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " BUILD_F4
*&---------------------------------------------------------------------*
*&      Form  GET_SALES_ORG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sales_org .

*  DATA: l_pernr   TYPE pa0105-pernr,
*        l_usrid   TYPE sy-uname.
*
*  DATA: ls_usr05  TYPE usr05.
*
*  l_usrid = sy-uname.
*
*  SELECT SINGLE * FROM usr05
*  INTO ls_usr05
*  WHERE bname = l_usrid
*   AND  parid = 'VKO'.
*  IF sy-subrc EQ 0.
*    p_vkorg = ls_usr05-parva.
*    CLEAR ls_usr05.
*  ENDIF.
*
*  SELECT SINGLE pernr INTO l_pernr
*  FROM pa0105
*  WHERE subty = '0001'
*    AND endda >= sy-datum
*    AND begda <= sy-datum
*    AND usrid = l_usrid.
*  IF sy-subrc <> 0.
*  ELSE.
*    g_sales = l_pernr.
**    PERFORM get_all_plans USING l_pernr sy-datum sy-datum.
*  ENDIF.

ENDFORM.                    " GET_SALES_ORG
*&---------------------------------------------------------------------*
*&      Form  RETRIEVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM retrieve_data .

  DATA: l_amount        TYPE komv-kbetr,
        l_knumv         TYPE knumv,
        l_kbetr         TYPE kbetr,
        l_rate          LIKE konv-kbetr,
        l_subrc         LIKE sy-subrc,
        l_supplier      TYPE bukrs,
        l_bill          TYPE vbeln_von,
        l_bill_itm      TYPE posnr_von,
        l_bill_dat      TYPE erdat,
        l_netwr         TYPE netwr_fp,
        l_chg_back_amt  TYPE kbetr.

  REFRESH gt_vbak.

  SELECT * INTO TABLE gt_vbak
    FROM vbak
    WHERE auart = 'ZCR2'
    AND   vbeln IN s_vbeln
    AND   erdat IN s_erdat
    AND   vkorg EQ p_vkorg
    AND   kunnr IN s_disti
    AND   vbeln LIKE '065%'.
  IF gt_vbak IS NOT INITIAL.

    SELECT * INTO TABLE gt_vbfa
      FROM vbfa
      FOR ALL ENTRIES IN gt_vbak
      WHERE vbelv = gt_vbak-vbeln
      AND   vbeln LIKE '097%'.

    LOOP AT gt_vbak ASSIGNING <vbak>.
      READ TABLE gt_vbfa TRANSPORTING NO FIELDS WITH KEY vbelv = <vbak>-vbeln.
      IF sy-subrc <> 0.
        DELETE gt_vbak.
        CONTINUE.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF gt_vbak IS INITIAL.
    MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*Get Sales Document: Item Data
  SELECT * INTO TABLE gt_vbap
    FROM vbap
    FOR ALL ENTRIES IN gt_vbak
    WHERE vbeln = gt_vbak-vbeln.

**Get Mara
*  SELECT * INTO TABLE gt_mara
*    FROM mara
*    FOR ALL ENTRIES IN gt_vbap
*    WHERE matnr = gt_vbap-matnr.
**    AND   raube <> ''.

* 檢查ztable有沒有已經建過grouping number且cancel flag為空
* cancel flag= ‘X’才可以列入計算 ; 反之則不列入計算。
  SELECT * INTO TABLE gt_ztcrm0003
    FROM ztcrm0003
    FOR ALL ENTRIES IN gt_vbak
    WHERE vbeln = gt_vbak-vbeln
    AND   cancel_flag = ''.

**Get VBKD
*  SELECT * INTO TABLE gt_vbkd
*    FROM vbkd
*    FOR ALL ENTRIES IN gt_vbak
*    WHERE vbeln = gt_vbak-vbeln
*    AND   fkdat IN s_fkdat.  "Add 2022/10/06

  LOOP AT gt_vbak INTO gs_vbak.

    "Check Ztable
    READ TABLE gt_ztcrm0003 INTO gs_ztcrm0003 WITH KEY vbeln = gs_vbak-vbeln.
    IF sy-subrc = 0.
*      IF gs_ztcrm0003-cancel_flag = ''.
      CONTINUE.
*      ENDIF.
    ENDIF.

    "Check SO1 SO2
    CLEAR l_supplier.
    READ TABLE gt_vbap INTO gs_vbap WITH KEY vbeln = gs_vbak-vbeln.
    IF sy-subrc = 0.
      PERFORM check_so1so2 USING gs_vbap
                           CHANGING l_subrc
                                    l_supplier.
      IF l_subrc <> 0.
        CONTINUE.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

*    "Check Original billing date : 當該欄位有輸入值時,則忽略該日期前的billing,不列入計算
*    READ TABLE gt_vbkd TRANSPORTING NO FIELDS WITH KEY vbeln = gs_vbak-vbeln.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.

*--------------------------------------------------------------------*
* Build Common data
*--------------------------------------------------------------------*
    APPEND INITIAL LINE TO gt_data ASSIGNING <data>.

    <data>-vbeln    = gs_vbak-vbeln.
    <data>-vkorg    = p_vkorg.
    <data>-supplier = l_supplier.  "=> ZTSD0084-VKORG_SO2
    <data>-disti    = gs_vbak-kunnr.

*    "get Amount
    CLEAR l_amount.
    SELECT SINGLE * INTO gs_konv
      FROM konv
      WHERE knumv = gs_vbak-knumv
      AND kschl = 'PR00'
      AND kappl = 'V'.
    IF sy-subrc = 0.
*      l_amount = gs_konv-kbetr.
*      <data>-orig_credit_amt = l_amount.
      <data>-waers = gs_konv-waers.
    ENDIF.

    CLEAR:
*           l_knumv,
*           l_kbetr,
           l_amount,
           l_chg_back_amt.
    LOOP AT gt_vbap INTO gs_vbap WHERE vbeln = gs_vbak-vbeln.


      "Check Original billing date : 當該欄位有輸入值時,則忽略該日期前的billing,不列入計算
      IF p_fkdat IS NOT INITIAL.
        SELECT * UP TO 1 ROWS FROM vbfa
             WHERE vbeln = gs_vbap-zzvgbel
             AND   posnn = gs_vbap-zzvgpos.
        ENDSELECT.
        IF sy-subrc <> 0.
          CONTINUE.
        ELSE.
          IF vbfa-erdat < p_fkdat.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      "get Discount rate
      CLEAR: l_knumv,
             l_kbetr,
             l_rate.
*    READ TABLE gt_vbap INTO gs_vbap INDEX 1.
*    IF sy-subrc = 0.
      SELECT SINGLE knumv INTO l_knumv FROM vbrk
        WHERE vbeln = gs_vbap-zzvgbel.
      IF sy-subrc = 0.
        SELECT SINGLE kbetr INTO l_kbetr FROM konv
          WHERE knumv = l_knumv
          AND kschl = 'ZTPD'
          AND kappl = 'V'.

        l_rate = l_rate + ( l_kbetr / 1000 ).
      ENDIF.
*    ENDIF.

*--------------------------------------------------------------------*
*Sales要向Supplier進行charge-back應要透過billing1(FIELD14、15)的amount(VBRP-NETWR)
* >>Original sales billing document
* >>Original sales billing item
*--------------------------------------------------------------------*
      CLEAR: l_bill , l_bill_itm, l_bill_dat.
      PERFORM get_so1so2 USING gs_vbap
                        CHANGING l_bill       "Original sales billing document ( SO2 的billing )
                                 l_bill_itm   "Original sales billing item
                                 l_bill_dat . "Original sales billing date

*<< Begin : Actual Claimed Amount => ZTCRM0003的credit memo(097開頭的單+item)得到VBRP-NETWR ****
*      "get Amount
*      CLEAR gs_vbrp.
*      SELECT SINGLE * INTO gs_vbrp FROM vbrp
*        WHERE vbeln = l_bill
*        AND   posnr = l_bill_itm.
*      IF sy-subrc = 0.
*        l_amount = l_amount + gs_vbrp-netwr.
*        l_chg_back_amt = l_chg_back_amt + ( gs_vbrp-netwr * l_rate ).
*      ENDIF.

      "get Actual Claimed Amount
      READ TABLE gt_vbfa INTO gs_vbfa WITH KEY vbelv = gs_vbap-vbeln
                                               posnv = gs_vbap-posnr.
      IF sy-subrc = 0.
        SELECT SINGLE * INTO gs_vbrp FROM vbrp
          WHERE vbeln = gs_vbfa-vbeln  "Credit memo number
          AND   posnr = gs_vbfa-posnn. "Credit memo item
        IF sy-subrc = 0.
          l_amount = l_amount + gs_vbrp-netwr.

          CLEAR l_kbetr.
          l_netwr = gs_vbrp-netwr * ( 1 - l_rate ).
          l_chg_back_amt = l_chg_back_amt + l_netwr.
        ENDIF.
      ENDIF.
*<< End   : Actual Claimed Amount **************************************************************

    ENDLOOP.

    <data>-orig_credit_amt = l_amount.
*    <data>-chg_back_amt = l_amount * l_rate.
    <data>-chg_back_amt = l_chg_back_amt.
    CLEAR: l_amount,
           l_rate,
           l_chg_back_amt.

  ENDLOOP.

*沒有資料應該可以直接leave list-processing
*錯誤訊息帶 all data searching had been grouped
  IF gt_data IS INITIAL.
    MESSAGE text-e03 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
*<< Begon add on 2023/02/24 ***************************************
*<< 相同 supplier & disti & waers 要做 Summary, 要先排序正確
    SORT gt_data[] BY supplier
                      disti
                      waers.
*<< End   add on 2023/02/24  **************************************
  ENDIF.


ENDFORM.                    " RETRIEVE_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_SUPPLIER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_supplier USING    p_raube
                    CHANGING p_retrun.

  DATA: ls_t142t TYPE t142t.
  DATA: patt TYPE string,
        text TYPE string,
        off  TYPE i,
        moff TYPE i,
        mlen TYPE i.

  "set defult
  p_retrun = 0.

  CLEAR ls_t142t.
  SELECT SINGLE * INTO ls_t142t
    FROM t142t
    WHERE spras = 'E'
      AND raube = p_raube.

  patt = p_suppl.

  FIND patt IN SECTION OFFSET off OF ls_t142t-rbtxt
  MATCH OFFSET moff
  MATCH LENGTH mlen.
  IF sy-subrc <> 0.
    p_retrun = 4.
  ENDIF.

ENDFORM.                    " CHECK_SUPPLIER
*&---------------------------------------------------------------------*
*&      Form  SHOW_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_summary .

  PERFORM build_layout.
  PERFORM build_fieldcat.

  DATA: l_repid LIKE sy-repid.
  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_buffer_active          = 'X'
      i_callback_program       = l_repid
      i_callback_pf_status_set = 'MENU_SET'
      i_callback_user_command  = 'USER_COMMAND_EVENT'
      it_fieldcat              = gt_fcat
      is_layout                = gs_layout
      i_default                = 'X'
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_summary
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  CASE sy-subrc.
    WHEN 0.
      "ALV show success
    WHEN 1.
      "program_error
    WHEN 2.
      "other issue
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " SHOW_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra             = 'X'.
  gs_layout-box_fieldname     = 'SEL'.
  gs_layout-window_titlebar   = 'Summary Report'.


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

  field_desc_i 'SEL'             'SEL'         'SEL'         'SELECT'.
  field_desc_i 'VKORG'           'Sales'       'Sales'       'Sales'.
  field_desc_i 'SUPPLIER'        'Supplier'    'Supplier'    'Supplier'.
  field_desc_i 'DISTI'           'Disti'       'Disti'       'Disti'.
  field_desc_i 'ORIG_CREDIT_AMT' 'Ori.Cre.Amt' 'Ori.Cre.Amt' 'Original credit amount'.
  field_desc_i 'CHG_BACK_AMT'    'Chg-Bk Amt'  'Chg-Bk Amt'  'Change-back amount'.
  field_desc_i 'WAERS'           'Curr'        'Curr'        'Currency'.
  field_desc_i 'GROUPING_NO'     'GRP No.'     'GRP No.'     'Grouping number'.


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
*&      Form  user_command_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command_event USING r_ucomm     LIKE sy-ucomm
                              rs_selfield TYPE slis_selfield..
  CASE r_ucomm.
    WHEN '&IC1'.

    WHEN 'ZDETAIL'.

      PERFORM build_detail_alv.
      CALL SCREEN 9001.

    WHEN 'ZGROUPING'.

      PERFORM do_grouping.
      rs_selfield-refresh = 'X'.

    WHEN 'ZSAVE'.

      PERFORM save.

    WHEN 'ZNOTE'.

      PERFORM download_debitnote.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDFORM.                    "user_command_event
*&---------------------------------------------------------------------*
*&      Form  BUILD_DETAIL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_detail_data .

  DATA: l_amount   TYPE komv-kbetr,
        l_knumv    TYPE knumv,
        l_kbetr    TYPE kbetr,
        l_rate     LIKE konv-kbetr,
        l_zzpartno LIKE mara-zzpartno,
        l_bill     TYPE vbeln_von,
        l_bill_itm TYPE posnr_von,
        l_bill_dat TYPE erdat.


  REFRESH gt_detail.

  LOOP AT gt_summary INTO gs_summary. "WHERE sel = 'X'.

    LOOP AT gt_data INTO gs_data WHERE supplier = gs_summary-supplier
                                      AND disti = gs_summary-disti
                                      AND waers = gs_summary-waers.

      READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = gs_data-vbeln.
      CHECK sy-subrc = 0.

      LOOP AT gt_vbap INTO gs_vbap WHERE vbeln = gs_vbak-vbeln.

        "Check Original billing date : 當該欄位有輸入值時,則忽略該日期前的billing,不列入計算
        CLEAR vbfa.
        SELECT * UP TO 1 ROWS FROM vbfa
             WHERE vbeln = gs_vbap-zzvgbel
             AND   posnn = gs_vbap-zzvgpos.
        ENDSELECT.
        IF p_fkdat IS NOT INITIAL.
          IF sy-subrc <> 0.
            CONTINUE.
          ELSE.
            IF vbfa-erdat < p_fkdat.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND INITIAL LINE TO gt_detail ASSIGNING <detail>.
        <detail>-orig_disti_date  = vbfa-erdat.    "Original disti billing date
        <detail>-grouping_no = gs_summary-grouping_no.
        <detail>-disti = gs_vbak-kunnr.
        <detail>-vbeln = gs_vbak-vbeln.
        <detail>-posnr = gs_vbap-posnr.

        CLEAR l_zzpartno.
        SELECT SINGLE zzpartno INTO l_zzpartno FROM mara
          WHERE matnr = gs_vbap-matnr.
        IF sy-subrc = 0.
          <detail>-zzpartno = l_zzpartno.
        ENDIF.

        <detail>-supplier = gs_data-supplier.

*--------------------------------------------------------------------*
*Sales要向Supplier進行charge-back應要透過billing1(FIELD14、15)的amount(VBRP-NETWR)
* >>Original sales billing document
* >>Original sales billing item
*--------------------------------------------------------------------*
        "get amount
*        CLEAR gs_konv.
*        SELECT SINGLE * INTO gs_konv
*          FROM konv
*          WHERE knumv = gs_vbak-knumv
*          AND kschl = 'PR00'
*          AND kappl = 'V'.
*        IF sy-subrc = 0.
*          l_amount = gs_konv-kbetr.
*        ENDIF.
        CLEAR: l_bill , l_bill_itm, l_bill_dat.
        PERFORM get_so1so2 USING gs_vbap
                          CHANGING l_bill       "Original sales billing document ( SO2 的billing )
                                   l_bill_itm   "Original sales billing item
                                   l_bill_dat . "Original sales billing date

*<< Begin : Actual Claimed Amount => ZTCRM0003的credit memo(097開頭的單+item)得到VBRP-NETWR ****
*        "get Amount
*        CLEAR gs_vbrp.
*        CLEAR l_amount.
*        SELECT SINGLE * INTO gs_vbrp FROM vbrp
*          WHERE vbeln = l_bill
*          AND   posnr = l_bill_itm.
*        IF sy-subrc = 0.
*          l_amount = gs_vbrp-netwr.
*        ENDIF.

        "get Actual Claimed Amount
        CLEAR gs_vbrp.
        CLEAR l_amount.
        READ TABLE gt_vbfa INTO gs_vbfa WITH KEY vbelv = <detail>-vbeln
                                                 posnv = <detail>-posnr.
        IF sy-subrc = 0.
          SELECT SINGLE * INTO gs_vbrp FROM vbrp
            WHERE vbeln = gs_vbfa-vbeln  "Credit memo number
            AND   posnr = gs_vbfa-posnn. "Credit memo item
          IF sy-subrc = 0.
            l_amount = gs_vbrp-netwr.
          ENDIF.
        ENDIF.
*<< End   : Actual Claimed Amount **************************************************************

        "get Discount rate
        CLEAR: l_knumv,
               l_kbetr,
               l_rate.
        SELECT SINGLE knumv INTO l_knumv FROM vbrk
          WHERE vbeln = gs_vbap-zzvgbel.
        IF sy-subrc = 0.
          SELECT SINGLE kbetr INTO l_kbetr FROM konv
            WHERE knumv = l_knumv
            AND kschl = 'ZTPD'
            AND kappl = 'V'.

          l_rate = l_rate + ( l_kbetr / 1000 ).
        ENDIF.

        <detail>-waers = gs_konv-waers.
        <detail>-amount = l_amount.
        <detail>-discount_rate = l_rate.
        <detail>-chg_back_amt = l_amount * ( 1 - l_rate ).   "Field6 * ( 1 - discount rate )

        "Unit Price => VBAP-NETPR/1000
        <detail>-unit_price =  gs_vbap-netpr / 100 * ( 1 - l_rate ). "vbap-netpr / 100 * ( 1 - discount rate )


        <detail>-orig_disti_vbeln = gs_vbap-zzvgbel. "Original disti billing document
        <detail>-orig_disti_posnr = gs_vbap-zzvgpos. "Original disti billing document item

*        SELECT * UP TO 1 ROWS FROM vbfa
*             WHERE vbeln = gs_vbap-zzvgbel
*             AND   posnn = gs_vbap-zzvgpos.
*        ENDSELECT.
*        IF sy-subrc = 0.
*          <detail>-orig_disti_date  = vbfa-erdat.    "Original disti billing date
*        ENDIF.

        "<detail>-belnr = BKPF-BELNR                 "Accounting document number
        "<detail>-buzei = BSEG-BUZEI                 "Accounting document item

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.


ENDFORM.                    " BUILD_DETAIL_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .

  IF go_grid1 IS NOT BOUND.

    CREATE OBJECT go_custom_container
      EXPORTING
        container_name              = g_container  "CONTAINER1
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT go_grid1
      EXPORTING
        i_parent          = go_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM prepare_fld_catlog.

    gs_layout2-zebra      = 'X'.
    gs_layout2-cwidth_opt = 'X'.
    gs_layout2-no_toolbar = ' '.
    gs_layout2-sel_mode   = 'D'.
    gs_layout2-box_fname  = 'SEL'.

    CALL METHOD go_grid1->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout2
      CHANGING
        it_outtab                     = gt_detail_alv
        it_fieldcatalog               = g_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CALL METHOD go_grid1->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
  ENDIF.

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FLD_CATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_fld_catlog.

  PERFORM append_fcat USING: 'SEL'               1  '5'  'SEL'.
  PERFORM append_fcat USING: 'GROUPING_NO'       2  '15' 'Grouping number'.
  PERFORM append_fcat USING: 'DISTI'             3  '5' 'Disti'.
  PERFORM append_fcat USING: 'VBELN'             4  '5' 'Credit memo request number'.
  PERFORM append_fcat USING: 'POSNR'             5  '5' 'Credit memo request item'.
  PERFORM append_fcat USING: 'ZZPARTNO'          6  '5' 'Material[PART NUMBER]'.
  PERFORM append_fcat USING: 'SUPPLIER'          7  '5' 'Supplier'.
  PERFORM append_fcat USING: 'AMOUNT'            8  '5' 'Actual Claimed Amount'. "2022/09/20
  PERFORM append_fcat USING: 'CHG_BACK_AMT'      9  '5' 'Charge-back amount'.
  PERFORM append_fcat USING: 'WAERS'             10 '5' 'Currency'.
  PERFORM append_fcat USING: 'UNIT_PRICE'        11 '5' 'Unit Price'.
  PERFORM append_fcat USING: 'DISCOUNT_RATE'     12 '5' 'Discount rate'.
  PERFORM append_fcat USING: 'ORIG_DISTI_VBELN'  13 '5' 'Original disti billing document'.
  PERFORM append_fcat USING: 'ORIG_DISTI_POSNR'  14 '5' 'Original disti billing document item'.
  PERFORM append_fcat USING: 'ORIG_DISTI_DATE'   15 '5' 'Original disti billing date'.
  PERFORM append_fcat USING: 'BELNR'             16 '5' 'Accounting document number'.
  PERFORM append_fcat USING: 'BUZEI'             17 '5' 'Accounting document item'.

ENDFORM.                    " PREPARE_FLD_CATLOG
*&---------------------------------------------------------------------*
*&      Form  append_fcat
*&---------------------------------------------------------------------*
FORM append_fcat USING fname
                       col_pos
                       outl
                       ctext .

  DATA: ls_fcat TYPE lvc_s_fcat.

  CLEAR ls_fcat.

  ls_fcat-fieldname = fname.
  ls_fcat-col_pos   = col_pos.
  ls_fcat-outputlen = outl.
  ls_fcat-coltext   = ctext.
  ls_fcat-seltext   = ctext.

  IF ls_fcat-fieldname = 'SEL'.
    ls_fcat-tech = 'X'.
    ls_fcat-checkbox = 'X'.
  ENDIF.

  IF  ls_fcat-fieldname = 'BELNR'.
    ls_fcat-no_out = 'X'.
  ENDIF.

  IF  ls_fcat-fieldname = 'BUZEI'.
    ls_fcat-no_out = 'X'.
  ENDIF.

  APPEND ls_fcat TO g_fcat.

ENDFORM.                    " append_fcat
*&---------------------------------------------------------------------*
*&      Form  DO_GROUPING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_grouping .

  "當人員點選”grouping” button,根據supplier+disti+currency產生流水碼[2.3.1
  "ZGP+當天日期+流水碼 (ex.ZGP2022052600001)

  DATA: l_suppl       TYPE bukrs,
        l_disti       TYPE kunnr,
        l_waers       TYPE waers,
        l_key(20)     TYPE c,
        l_new_case    TYPE flag,
        l_zcrm01(5)   TYPE c,
        l_grouping_no TYPE zgrouping_no,
        l_subrc       LIKE sy-subrc.

  DATA: ls_vbrp       TYPE vbrp.
  DATA: lt_data       TYPE TABLE OF ty_data.


  PERFORM check_select.

*<< Avoid duplicate grouping of screen data
  READ TABLE gt_summary TRANSPORTING NO FIELDS WITH KEY sel = 'X'
                                                        group_flag = 'X'.
  IF sy-subrc = 0.
    MESSAGE text-e05 TYPE 'E'.
  ENDIF.

  LOOP AT gt_summary ASSIGNING <summary> WHERE sel = 'X'
                                         AND group_flag = ''.

    CLEAR l_new_case.
    IF ( l_suppl IS INITIAL AND l_disti IS INITIAL AND l_waers IS INITIAL ).
      l_suppl = <summary>-supplier.
      l_disti = <summary>-disti.
      l_waers = <summary>-waers.
      l_new_case = 'X'.
    ELSE.
      IF ( l_suppl <> <summary>-supplier OR l_disti <> <summary>-disti OR l_waers <> <summary>-waers ).
        l_suppl = <summary>-supplier.
        l_disti = <summary>-disti.
        l_waers = <summary>-waers.
        l_new_case = 'X'.
      ENDIF.
    ENDIF.

*--------------------------------------------------------------------*
* one new case = one new Grouping number
*--------------------------------------------------------------------*
    IF l_new_case = 'X'.

*<< initial
      CLEAR: l_grouping_no, l_zcrm01.

*<< Get grouping number
      PERFORM get_groupingno CHANGING l_zcrm01
                                      l_subrc.
      IF l_subrc <> 0.
        MESSAGE text-e01 TYPE 'E'.
      ELSE.

        CONCATENATE 'ZGP'
                    sy-datum
                    l_zcrm01 INTO l_grouping_no.

        <summary>-grouping_no = l_grouping_no.
        <summary>-group_flag = 'X'.

*<< Submit FB01
*存檔=>產生會計文件單號回壓 detail 畫面, field12跟field13
*        PERFORM fb01.


*<< Update common data
        LOOP AT gt_data ASSIGNING <data> WHERE supplier = l_suppl
                                         AND   disti    = l_disti
                                         AND   waers    = l_waers.
          <data>-grouping_no = l_grouping_no.

          READ TABLE gt_vbak INTO gs_vbak WITH KEY vbeln = <data>-vbeln.
          CHECK sy-subrc = 0.

*<< Update Ztable
*--------------------------------------------------------------------*
* Build ZTCRM0001 : Master table
*--------------------------------------------------------------------*
          APPEND INITIAL LINE TO gt_ztcrm0001 ASSIGNING <crm0001>.
          <crm0001>-grouping_no = l_grouping_no.
          <crm0001>-supplier    = <data>-supplier.
          <crm0001>-vkorg       = <data>-vkorg.
          <crm0001>-period      = gs_vbak-erdat+0(6).
          <crm0001>-erdat       = sy-datum.
          <crm0001>-erzet       = sy-uzeit.
          <crm0001>-ernam       = sy-uname.
          <crm0001>-cancel_flag = ''.
          <crm0001>-aedat       = ''.
          <crm0001>-aezet       = ''.
          <crm0001>-aenam       = ''.
          <crm0001>-status      = '1'.  "unprocess

*--------------------------------------------------------------------*
* Build ZTCRM0002 : Summary table
*--------------------------------------------------------------------*
          APPEND INITIAL LINE TO gt_ztcrm0002 ASSIGNING <crm0002>.
          <crm0002>-grouping_no     = l_grouping_no.
          <crm0002>-disti           = <summary>-disti.
          <crm0002>-orig_credit_amt = <summary>-orig_credit_amt.
          <crm0002>-chg_back_amt    = <summary>-chg_back_amt.
          <crm0002>-waers           = <summary>-waers.
          <crm0002>-zzcmrappno      = ''.
          <crm0002>-supplier        = <summary>-supplier.

*--------------------------------------------------------------------*
* Build ZTCRM0003 : Detail table
*--------------------------------------------------------------------*
          LOOP AT gt_detail ASSIGNING <detail> WHERE vbeln = <data>-vbeln.

            <detail>-grouping_no = l_grouping_no.

            APPEND INITIAL LINE TO gt_ztcrm0003 ASSIGNING <crm0003>.
            <crm0003>-grouping_no = l_grouping_no.
            <crm0003>-vbeln = <detail>-vbeln.       "Credit memo request number
            <crm0003>-posnr = <detail>-posnr.       "Credit memo request item
            <crm0003>-zzpartno = <detail>-zzpartno. "Part Number

            READ TABLE gt_vbfa INTO gs_vbfa WITH KEY vbelv = <detail>-vbeln
                                                     posnv = <detail>-posnr.
            IF sy-subrc = 0.
              <crm0003>-vbelv = gs_vbfa-vbeln.  "Credit memo number
              <crm0003>-posnv = gs_vbfa-posnn.  "Credit memo item
            ENDIF.

            <crm0003>-orig_credit_amt  = <detail>-amount.
            <crm0003>-chg_back_amt     = <detail>-chg_back_amt.
            <crm0003>-waers            = <detail>-waers.
            <crm0003>-unit_price       = <detail>-unit_price.
            <crm0003>-discount_rate    = <detail>-discount_rate.
            <crm0003>-orig_disti_vbeln = <detail>-orig_disti_vbeln. "Original disti billing document
            <crm0003>-orig_disti_posnr = <detail>-orig_disti_posnr. "Original disti billing item
            <crm0003>-orig_disti_date  = <detail>-orig_disti_date . "Original disti billing date

*            <crm0003>-BELNR "Accounting document number (FB01單號)
*            <crm0003>-BUZEI "Accounting document number (FB01 item)

            CLEAR gs_vbap.
            READ TABLE gt_vbap INTO gs_vbap WITH KEY vbeln = <detail>-vbeln
                                                     posnr = <detail>-posnr.
            IF sy-subrc = 0.
              PERFORM get_so1so2 USING gs_vbap
                                 CHANGING <crm0003>-orig_sales_vbelv  "Original sales billing document ( SO2 的billing )
                                          <crm0003>-orig_sales_posnv  "Original sales billing item
                                          <crm0003>-orig_sales_erdat. "Original sales billing date
            ENDIF.

*FKIMG  Actual Invoiced Quantity  Billing數量
*VRKME  Sales Unit  Billing單位
*UNIT_CHG_AMT	Unit Price
            CLEAR ls_vbrp.
            SELECT SINGLE * INTO ls_vbrp FROM vbrp WHERE vbeln = <crm0003>-orig_sales_vbelv
                                                   AND   posnr = <crm0003>-orig_sales_posnv.
            IF sy-subrc = 0.
              <crm0003>-fkimg = ls_vbrp-fkimg.
              <crm0003>-vrkme = ls_vbrp-vrkme.

              IF <crm0003>-fkimg <> 0.
                <crm0003>-unit_chg_amt = <crm0003>-chg_back_amt / <crm0003>-fkimg.
              ENDIF.
            ENDIF.

*根據ZTCRM0003-ORIG_SALES_VBELV ZTCRM0003-ORIG_SALES_POSNV
*串VBRP-VBELN+POSNR抓VBRK-XBLNR
            SELECT SINGLE xblnr INTO <crm0003>-orig_xblnr FROM vbrk WHERE vbeln = <crm0003>-orig_sales_vbelv.

            <crm0003>-cancel_flag = ''.

          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

*--------------------------------------------------------------------*
* Final save data
*--------------------------------------------------------------------*
  PERFORM save.


ENDFORM.                    " DO_GROUPING
*&---------------------------------------------------------------------*
*&      Form  BUILD_SUMMARY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_summary_data .

  DATA: l_suppl   TYPE bukrs,
        l_disti   TYPE kunnr,
        l_waers   TYPE waers.

  DATA: lt_data   TYPE TABLE OF ty_data,
        ls_data   LIKE LINE OF lt_data.

  DATA: l_new_case TYPE flag.

  REFRESH gt_summary.
  LOOP AT gt_data INTO gs_data.

    CLEAR l_new_case.
    IF ( l_suppl IS INITIAL AND l_disti IS INITIAL AND l_waers IS INITIAL ).
      l_suppl = gs_data-supplier.
      l_disti = gs_data-disti.
      l_waers = gs_data-waers.
      l_new_case = 'X'.
    ELSE.
      IF ( l_suppl <> gs_data-supplier OR l_disti <> gs_data-disti OR l_waers <> gs_data-waers ).
        l_suppl = gs_data-supplier.
        l_disti = gs_data-disti.
        l_waers = gs_data-waers.
        l_new_case = 'X'.
      ENDIF.
    ENDIF.

    IF l_new_case = 'X'.
      REFRESH lt_data[].
      lt_data[] = gt_data[].
      LOOP AT lt_data INTO ls_data WHERE supplier = l_suppl
                                     AND disti    = l_disti
                                     AND waers    = l_waers.
        CLEAR gs_summary.
        gs_summary-vkorg            = ls_data-vkorg.
        gs_summary-supplier         = ls_data-supplier.
        gs_summary-disti            = ls_data-disti.
        gs_summary-orig_credit_amt  = ls_data-orig_credit_amt.
        gs_summary-chg_back_amt     = ls_data-chg_back_amt.
        gs_summary-waers            = ls_data-waers.
        COLLECT gs_summary INTO gt_summary.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " BUILD_SUMMARY_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_select .

  READ TABLE gt_summary TRANSPORTING NO FIELDS WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE text-e04 TYPE 'E'.
  ENDIF.

ENDFORM.                    " CHECK_SELECT
*&---------------------------------------------------------------------*
*&      Form  GET_SNRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_NEXTNUM  text
*----------------------------------------------------------------------*
FORM get_groupingno  CHANGING p_nextnum TYPE char05
                              p_subrc   LIKE sy-subrc.

  DATA: current_year LIKE  inri-toyear.

  current_year = sy-datum+0(4).

*LOCK
  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
    EXPORTING
      object           = 'ZCRM01'
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.
  IF sy-subrc = 0.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZCRM01'
        toyear                  = current_year
      IMPORTING
        number                  = p_nextnum
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
*UNLOCK
      CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
        EXPORTING
          object           = 'ZCRM01'
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
    ENDIF.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_subrc = sy-subrc.

ENDFORM.                    " GET_SNRO
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save .

  IF gt_ztcrm0001 IS INITIAL.
    MESSAGE 'no data to save' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  MODIFY ztcrm0001 FROM TABLE gt_ztcrm0001.

  IF gt_ztcrm0002 IS NOT INITIAL.
    MODIFY ztcrm0002 FROM TABLE gt_ztcrm0002.
  ENDIF.

  IF gt_ztcrm0003 IS NOT INITIAL.
    MODIFY ztcrm0003 FROM TABLE gt_ztcrm0003.
  ENDIF.

  COMMIT WORK AND WAIT.

  MESSAGE 'Saved successfully' TYPE 'S'.

  REFRESH: gt_ztcrm0001,
           gt_ztcrm0002,
           gt_ztcrm0003.

ENDFORM.                    " SAVE
*&---------------------------------------------------------------------*
*&      Form  BUILD_DETAIL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_detail_alv .

  REFRESH gt_detail_alv.

  PERFORM check_select.

*<< focus choosing data
  LOOP AT gt_summary INTO gs_summary WHERE sel = 'X'.
    LOOP AT gt_data INTO gs_data WHERE supplier = gs_summary-supplier
                                      AND disti = gs_summary-disti
                                      AND waers = gs_summary-waers.

      LOOP AT gt_detail INTO gs_detail WHERE vbeln = gs_data-vbeln.
        APPEND INITIAL LINE TO gt_detail_alv ASSIGNING <detail_alv>.
        MOVE-CORRESPONDING gs_detail TO <detail_alv>.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " BUILD_DETAIL_ALV
*&---------------------------------------------------------------------*
*&      Form  FB01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fb01 .

ENDFORM.                    " FB01
*&---------------------------------------------------------------------*
*&      Form  CHECK_SALES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_sales.

  DATA: l_error TYPE flag.

  CLEAR l_error.

  CASE sy-tcode.
    WHEN 'ZRCRM0001_WHK1'.
      IF p_vkorg <> 'WHK1'.
        l_error = 'X'.
      ENDIF.
    WHEN 'ZRCRM0001_WEHQ'.
      IF p_vkorg <> 'WEHQ'.
        l_error = 'X'.
      ENDIF.
    WHEN 'ZRCRM0001_WCA1'.
      IF p_vkorg <> 'WCA1'.
        l_error = 'X'.
      ENDIF.
    WHEN 'ZRCRM0001_NSZ1'.
      IF p_vkorg <> 'NSZ1'.
        l_error = 'X'.
      ENDIF.
    WHEN OTHERS. "SE38
      "do nothing
  ENDCASE.

  IF l_error = 'X'.
    MESSAGE text-e02 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " CHECK_SALES
*&---------------------------------------------------------------------*
*&      Form  GET_SO1SO2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_VBAP  text
*----------------------------------------------------------------------*
FORM check_so1so2   USING  ps_vbap LIKE gs_vbap
                    CHANGING c_subrc    TYPE sy-subrc
                             c_supplier TYPE  bukrs.

  DATA: l_so1       TYPE vbeln_va,
        l_so1_item  TYPE posnr_von,
        l_s02       TYPE zvbeln.

  DATA: ls_vbfa     TYPE vbfa,
        t_ret_msg	  TYPE TABLE OF	bapireturn,
        ew_so1so2	  TYPE ztsd0084,
        e_ret_code  LIKE ztwf0004-ret_code.

  DATA: e_so1no LIKE  vbak-vbeln,
        e_so2no LIKE  vbak-vbeln.


  CLEAR: l_so1,
         l_s02,
         ls_vbfa.

  "Get SO1
  SELECT SINGLE * INTO ls_vbfa FROM vbfa
    WHERE vbeln = ps_vbap-zzvgbel
    AND   posnn = ps_vbap-zzvgpos
    AND   vbtyp_n = 'M'
    AND   vbtyp_v = 'C'.
  IF sy-subrc = 0.
    l_so1 = ls_vbfa-vbelv.
    l_so1_item = ls_vbfa-posnv.
    CALL FUNCTION 'Z_FSD_GETICMORDERINFO'
      EXPORTING
        i_vbeln    = l_so1
      IMPORTING
        e_so1no    = e_so1no
        e_so2no    = e_so2no
        ew_so1so2  = ew_so1so2
        e_ret_code = e_ret_code
      TABLES
        pt_ret_msg = t_ret_msg.

    IF e_ret_code = 'SUCCESS' AND ( e_so1no <> e_so2no ).
      c_subrc = 0.
      c_supplier = ew_so1so2-vkorg_so2.

      IF p_suppl IS NOT INITIAL.
        IF p_suppl <> ew_so1so2-vkorg_so2. "比對畫面所輸入的
          c_subrc = 4.
        ENDIF.
      ENDIF.
    ELSE.
      c_subrc = 4.
    ENDIF.

  ELSE.
    c_subrc = 4.
  ENDIF.

ENDFORM.                    " GET_SO1SO2
*&---------------------------------------------------------------------*
*&      Form  GET_SO1SO2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_VBAP  text
*----------------------------------------------------------------------*
FORM get_so1so2   USING    ps_vbap LIKE gs_vbap
                  CHANGING p_so2_billing      LIKE vbfa-vbeln
                           p_so2_billing_item LIKE vbfa-posnn
                           p_so2_billing_date LIKE vbfa-erdat.

  DATA: l_so1        TYPE vbeln_va,
        l_so1_item   TYPE posnr_von,
        l_s02        TYPE zvbeln,
        l_dn         TYPE vbeln_von.

  DATA: ls_vbfa      TYPE vbfa,
        ls_vbfa_dn   TYPE vbfa,
        ls_vbfa_bill TYPE vbfa,
        t_ret_msg    TYPE TABLE OF  bapireturn,
        ew_so1so2	   TYPE ztsd0084,
        e_ret_code   LIKE ztwf0004-ret_code.

  DATA: e_so1no LIKE  vbak-vbeln,
        e_so2no LIKE  vbak-vbeln.

  CLEAR: l_so1,
         l_s02,
         ls_vbfa,
         l_dn.

*C : Order
*J : Delivery
*--------------------------------------------------------------------*
* Base on Delivery
*--------------------------------------------------------------------*
  "Get SO1
  SELECT SINGLE * INTO ls_vbfa FROM vbfa
    WHERE vbeln = ps_vbap-zzvgbel
    AND   posnn = ps_vbap-zzvgpos
    AND   vbtyp_n = 'M'
    AND   vbtyp_v = 'J'.  "vbtyp_v = 'C'.
  IF sy-subrc = 0.

    CLEAR: ls_vbfa_dn,
           ls_vbfa_bill.
    CONCATENATE 'I'  ls_vbfa-vbelv+1(9) INTO l_dn. "ex :I800863951

    SELECT SINGLE * INTO ls_vbfa_dn FROM vbfa
      WHERE vbelv = l_dn
      AND   posnv = ls_vbfa-posnv
      AND   vbtyp_n = 'M'
      AND   vbtyp_v = 'J'.
    IF sy-subrc = 0.
      SELECT SINGLE * INTO ls_vbfa_bill FROM vbfa
        WHERE vbeln = ls_vbfa_dn-vbeln
        AND   posnn = ls_vbfa_dn-posnn
        AND   vbtyp_n = 'M'
        AND   vbtyp_v = 'J'.
      IF sy-subrc = 0.
        p_so2_billing       = ls_vbfa_bill-vbeln.
        p_so2_billing_item  = ls_vbfa_bill-posnn.
        p_so2_billing_date  = ls_vbfa_bill-erdat.
      ENDIF.
    ENDIF.


*    l_so1 = ls_vbfa-vbelv.
*    l_so1_item = ls_vbfa-posnv.
*    CALL FUNCTION 'Z_FSD_GETICMORDERINFO'
*      EXPORTING
*        i_vbeln    = l_so1
*      IMPORTING
*        e_so1no    = e_so1no
*        e_so2no    = e_so2no
*        ew_so1so2  = ew_so1so2
*        e_ret_code = e_ret_code
*      TABLES
*        pt_ret_msg = t_ret_msg.
*    IF e_ret_code = 'SUCCESS' AND ( e_so1no <> e_so2no ).
**<< Begin DNRK940645 2022/08/10 **********************************
**      CLEAR ls_vbfa.
**      SELECT SINGLE * INTO ls_vbfa FROM vbfa
**        WHERE vbelv = ew_so1so2-zso2
**        AND   posnv = ps_vbap-zzvgpos
**        AND   vbtyp_n = 'M'
**        AND   vbtyp_v = 'C'.
**      IF sy-subrc = 0.
**        p_so2_billing       = ls_vbfa-vbeln.
**        p_so2_billing_item  = ls_vbfa-posnn.
**        p_so2_billing_date  = ls_vbfa-erdat.
**      ENDIF.
*
**<< 透過SO2的DN+item(VBFA-VBELV,POSNV)
**<< 去VBFA抓SO2的Billing(VBFA-VBELN,POSNN[M/J])
*      CLEAR: ls_vbfa_dn,
*             ls_vbfa_bill.
*
*      SELECT SINGLE * INTO ls_vbfa_dn FROM vbfa
*        WHERE vbelv = ew_so1so2-zso2
*        AND   posnv = ps_vbap-zzvgpos
*        AND   vbtyp_n = 'M'
*        AND   vbtyp_v = 'J'.
*      IF sy-subrc = 0.
*        SELECT SINGLE * INTO ls_vbfa_bill FROM vbfa
*          WHERE vbeln = ls_vbfa_dn-vbeln
*          AND   posnn = ls_vbfa_dn-posnn
*          AND   vbtyp_n = 'M'
*          AND   vbtyp_v = 'J'.
*        IF sy-subrc = 0.
*          p_so2_billing       = ls_vbfa_bill-vbeln.
*          p_so2_billing_item  = ls_vbfa_bill-posnn.
*          p_so2_billing_date  = ls_vbfa_bill-erdat.
*        ENDIF.
*      ENDIF.
**<< End   DNRK940645 2022/08/10 **********************************
*    ENDIF.
  ENDIF.


ENDFORM.                    " GET_SO1SO2
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DEBITNOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_debitnote .

  DATA: pe_code   TYPE  ztwf0004-ret_code,
        pe_return TYPE  bapiret2_t,
        s_return  LIKE LINE OF pe_return.

  DATA: t_message_tab	TYPE esp1_message_tab_type.
  FIELD-SYMBOLS: <message> LIKE LINE OF t_message_tab.

*<< initial
  REFRESH: t_message_tab.

*<< Check select
  PERFORM check_select.

*<< Check grouping number
  READ TABLE gt_summary TRANSPORTING NO FIELDS WITH KEY sel = 'X'
                                                        grouping_no = ' '.
  IF sy-subrc = 0.
    MESSAGE text-e06 TYPE 'E'.
  ENDIF.

  LOOP AT gt_summary INTO gs_summary WHERE sel = 'X'.

    CALL FUNCTION 'Z_FCRM_DOWNLOAD_DEBITNOTE'
      EXPORTING
        i_grouping_no = gs_summary-grouping_no
      IMPORTING
        pe_code       = pe_code
        pe_return     = pe_return.

    READ TABLE pe_return INTO s_return INDEX 1.
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO t_message_tab ASSIGNING <message>.
      <message>-msgid = 'ZSD'.
      <message>-msgty = pe_code.
      <message>-msgno = 000.
      <message>-msgv1 = s_return-message.
    ENDIF.

  ENDLOOP.

*<< Popup message
  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
    TABLES
      i_message_tab = t_message_tab.

ENDFORM.                    " DOWNLOAD_DEBITNOTE
