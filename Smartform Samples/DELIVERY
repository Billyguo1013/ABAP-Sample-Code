FUNCTION z_sd_delivery_print_a.
*"----------------------------------------------------------------------
*"*"區域介面：
*"  IMPORTING
*"     VALUE(IV_VBELN) TYPE  VBELN_VL
*"     VALUE(IV_PREVIEW) TYPE  FLAG DEFAULT ' '
*"  EXPORTING
*"     VALUE(ET_OTFDATA) TYPE  TSFOTF
*"     VALUE(SPOOLIDS) TYPE  TSFSPOOLID
*"  EXCEPTIONS
*"      FIND_NO_DATA
*"      FORMATTING_ERROR
*"      INTERNAL_ERROR
*"      SEND_ERROR
*"      USER_CANCELED
*"      OTHERS_ERROR
*"      NO_SMARTFORMS
*"      NO_SMARTFORMS_ZSFSD016
*"----------------------------------------------------------------------

***********************************************************************
* SMARTFORM PARAMETERS
***********************************************************************
  CONSTANTS: lc_formname TYPE tdsfname VALUE 'ZSFSD004_A'.

*--------------------------------------------------------------------*
* Smartforms data structure
*--------------------------------------------------------------------*
  DATA: ls_head       TYPE zssd004_h_a.
  DATA: lt_item       TYPE TABLE OF zssd004_i_a.
  DATA: lt_lips_his   TYPE TABLE OF lips.
  DATA: lv_lfimg_his  TYPE lfimg.


  SELECT SINGLE * INTO @DATA(ls_likp) FROM likp WHERE vbeln = @iv_vbeln.
  IF sy-subrc <> 0.
    RAISE find_no_data.
  ELSE.
    SELECT * INTO TABLE @DATA(lt_lips) FROM lips WHERE vbeln = @ls_likp-vbeln.
    IF sy-subrc <> 0.
      RAISE find_no_data.
    ENDIF.
  ENDIF.

  READ TABLE lt_lips INTO DATA(ls_lips) INDEX 1.

*--------------------------------------------------------------------*
* Build Header
*--------------------------------------------------------------------*
  ls_head = CORRESPONDING #( ls_likp ).

* Get Company info.
  SELECT SINGLE * FROM tvko INTO @DATA(ls_tvko) WHERE vkorg = @ls_likp-vkorg.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM t001 INTO @DATA(ls_t001) WHERE bukrs = @ls_tvko-bukrs.
    IF sy-subrc = 0.
      ls_head-butxt = ls_t001-butxt.
      SELECT SINGLE * FROM adrc INTO @DATA(ls_adrc) WHERE addrnumber = @ls_t001-adrnr.
      IF sy-subrc = 0.
        ls_head-house_num1 = ls_adrc-house_num1.
        ls_head-street = ls_adrc-street.
        ls_head-city1 = ls_adrc-city1.
        ls_head-country = ls_adrc-country.
      ENDIF.
    ENDIF.
  ENDIF.

* 訂單類型
  SELECT SINGLE * FROM vbak INTO @DATA(ls_vbak) WHERE vbeln = @ls_lips-vgbel.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM tvakt INTO @DATA(ls_tvakt) WHERE auart = @ls_vbak-auart AND spras = 'M'.
    IF sy-subrc = 0.
      ls_head-bezei = ls_tvakt-bezei.
    ENDIF.
  ENDIF.

* Get Sold-to
  PERFORM get_customer_master USING ls_head-kunnr
                                    'SOLD2'
                              CHANGING ls_head.

* Get Ship-to
  PERFORM get_customer_master USING ls_head-kunag
                                   'SHIP2'
                              CHANGING ls_head.

*--------------------------------------------------------------------*
* Build Item
*--------------------------------------------------------------------*
  LOOP AT lt_lips INTO ls_lips WHERE vbeln = ls_head-vbeln.

    APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
    <ls_item> = CORRESPONDING #( ls_lips  ).

*  Get Material
    SELECT SINGLE * FROM mara INTO @DATA(ls_mara) WHERE matnr = @ls_lips-matnr.
    IF sy-subrc = 0.
      <ls_item>-zzsp_prefix = ls_mara-zzsp_prefix.
      <ls_item>-zzsp_base = ls_mara-zzsp_base.
      <ls_item>-zzsp_suffix = ls_mara-zzsp_suffix.
    ENDIF.

*  物料描述
    SELECT SINGLE maktx INTO <ls_item>-maktx FROM makt WHERE matnr = ls_lips-matnr AND spras = 'M'.

*  累計訂單數量〈銷售單位〉
    SELECT SINGLE kwmeng INTO <ls_item>-kwmeng FROM vbap WHERE vbeln = ls_lips-vgbel AND posnr = ls_lips-vgpos.


    SELECT SINGLE * FROM vbkd INTO @DATA(ls_vbkd) WHERE vbeln = @ls_lips-vgbel AND posnr = @ls_lips-vgpos.
    IF sy-subrc = 0.
*      <ls_item>-bstkd = ls_vbkd-bstkd.
      <ls_item>-ihrez = ls_vbkd-ihrez.
    ENDIF.

    SELECT SINGLE * FROM vbkd INTO @ls_vbkd WHERE vbeln = @ls_lips-vgbel AND posnr = 000000.
    IF sy-subrc = 0.
      <ls_item>-bstkd = ls_vbkd-bstkd.
    ENDIF.

    <ls_item>-zzsp = |{ <ls_item>-zzsp_prefix } { <ls_item>-zzsp_base } { <ls_item>-zzsp_suffix }|.

    SHIFT <ls_item>-posnr LEFT DELETING LEADING '0'.

*  待出貨數量
*<< 回找歷史發貨數量
    CLEAR: lt_lips_his, lv_lfimg_his.
    SELECT * INTO TABLE lt_lips_his FROM lips WHERE vgbel = ls_lips-vgbel AND vgpos = ls_lips-vgpos.
    IF sy-subrc = 0.
      DELETE lt_lips_his WHERE vbeln >= ls_lips-vbeln.
      lv_lfimg_his = REDUCE lfimg( INIT x = 0 FOR wa IN lt_lips_his NEXT x = x + wa-lfimg ).
    ENDIF.

    <ls_item>-shipped_qty = <ls_item>-kwmeng - <ls_item>-lfimg - lv_lfimg_his.

  ENDLOOP.

* 請依照Field 13料號中段排序
  SORT lt_item BY zzsp_base.

*--------------------------------------------------------------------*
* Build SUM
*--------------------------------------------------------------------*
*  SELECT * INTO TABLE @DATA(lt_ztewm022d_a)
*    FROM ztewm022d_a
*    WHERE vbeln = @ls_likp-vbeln.
*  IF sy-subrc = 0.
*    SORT lt_ztewm022d_a BY lgnum tanum.
*    READ TABLE lt_ztewm022d_a INTO DATA(ls_ztewm022d_a) INDEX 1.
*    IF sy-subrc = 0.
*      ls_head-box_count = ls_ztewm022d_a-anzpk.
*      ls_head-volum = ls_ztewm022d_a-volum.
*    ENDIF.
*  ENDIF.

*  包裝件數
  ls_head-box_count = ls_likp-anzpk.

*  材積
  ls_head-volum = ls_likp-volum.

  SELECT * INTO TABLE @DATA(lt_ztewm022b_a)
    FROM ztewm022b_a
    WHERE vbeln = @ls_likp-vbeln.
  IF sy-subrc = 0.
*    DATA(l_lines) = lines( lt_ztewm022b_a ).
*    ls_head-box_count = l_lines.
    SELECT * INTO TABLE @DATA(lt_ztewm022a_a)
      FROM ztewm022a_a
      FOR ALL ENTRIES IN @lt_ztewm022b_a
      WHERE zpack_type = @lt_ztewm022b_a-zpack_type.
    IF sy-subrc = 0.
*      LOOP AT lt_ztewm022a_a INTO DATA(ls_ztewm022a_a).
*        ls_head-volum = ls_head-volum + ls_ztewm022a_a-volum.
*      ENDLOOP.
      READ TABLE lt_ztewm022a_a INTO DATA(ls_ztewm022a_a) INDEX 1.
      IF sy-subrc = 0.
        ls_head-voleh = ls_ztewm022a_a-voleh.
      ENDIF.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
*  Print Smartforms
*--------------------------------------------------------------------*
  DATA:
    fm_name  TYPE rs38l_fnam.
  DATA:
    lv_control_parameters TYPE ssfctrlop,
    ls_job_options        TYPE ssfcresop,
    ls_document           TYPE ssfcrespd,
    ls_job_info           TYPE ssfcrescl.
  DATA:
    l_output_options TYPE ssfcompop.
  DATA:
    lv_tabix     TYPE sy-tabix,
    l_amount(17) TYPE c.
  DATA:
    lt_spoolids TYPE tsfspoolid,
    ls_spool    LIKE LINE OF lt_spoolids.
  DATA:
    l_subject            TYPE so_obj_des,
    l_attachment_subject TYPE so_obj_des,
    pdf_xstring          TYPE xstring,
    pi_skip_commit       TYPE flag.
  DATA:
    lt_receivers TYPE TABLE OF zsemail_receiver_a,
    lt_body      TYPE soli_tab,
    ls_body      TYPE soli.

  lv_control_parameters-device = 'PRINTER'.
  lv_control_parameters-no_dialog  = 'X'.
  lv_control_parameters-langu      = 'M'.
  l_output_options-tddest          = 'PDF1'.

  CASE abap_true.
    WHEN iv_preview.
      lv_control_parameters-preview = ' '.
      lv_control_parameters-getotf   = 'X'.
      l_output_options-tdnewid =  ' '.
    WHEN OTHERS. "default
      lv_control_parameters-preview    = ''.
      lv_control_parameters-getotf     = 'X'.
      l_output_options-tdnewid =  ' '.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lc_formname
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    RAISE no_smartforms_zsfsd016.
  ENDIF.

  CALL FUNCTION fm_name
    EXPORTING
      control_parameters   = lv_control_parameters
      output_options       = l_output_options
      user_settings        = ' '
      is_head              = ls_head
    IMPORTING
      document_output_info = ls_document
      job_output_info      = ls_job_info
      job_output_option    = ls_job_options
    TABLES
      it_item              = lt_item
    EXCEPTIONS
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      OTHERS               = 5.
  CASE sy-subrc.
    WHEN 0.

      et_otfdata[] = ls_job_info-otfdata[].
      IF ls_job_info-spoolids IS NOT INITIAL.
        MOVE ls_job_info-spoolids TO spoolids.
      ENDIF.

      IF iv_preview = abap_false.
*-----Send PDF to SPOOL
        PERFORM send_spool_by_pdf USING ls_job_info-otfdata CHANGING ls_spool pdf_xstring.
        CLEAR lt_spoolids. APPEND ls_spool TO lt_spoolids.

        DATA(l_kunag) = |{ ls_head-kunag ALPHA = OUT }|.
        DATA(l_vbeln) = |{ ls_head-vbeln ALPHA = OUT }|.
        CONDENSE: l_kunag NO-GAPS, l_vbeln NO-GAPS.

*-----郵件標題
        l_subject = TEXT-001 && |{ l_kunag }_{ l_vbeln }_{ ls_head-wadat_ist }|.
*-----夾檔名稱
        l_attachment_subject = TEXT-002 && `_` && |{ sy-datlo }{ sy-timlo }|.
*-----郵件內文
        CLEAR lt_body[].
        lt_body[] = VALUE #( ( line = |To { ls_head-ship2name1 },<BR>| )
                             ( line = |<BR>| )
                             ( line = |交貨單 { l_vbeln } 已經出貨，詳細請參考附件。謝謝！<BR>| )
                             ( line = |<BR>| )
                             ( line = |敬啓,<BR>| )
                             ( line = |顧客服務處<BR>| )
                             ( line = |福特六和汽車<BR>| )
                           ).
*-----寄送地址
        CLEAR lt_receivers.
        SELECT SINGLE * INTO @DATA(ls_kna1) FROM kna1 WHERE kunnr = @ls_head-kunnr .
        IF sy-subrc = 0.
          SELECT * INTO TABLE @DATA(lt_adr6) FROM adr6
            WHERE addrnumber = @ls_kna1-adrnr
            AND   ( consnumber = '1' OR consnumber = '2' ).
          IF sy-subrc = 0.
            LOOP AT lt_adr6 INTO DATA(ls_adr6).
              APPEND INITIAL LINE TO lt_receivers ASSIGNING FIELD-SYMBOL(<ls_receiver>).
              <ls_receiver>-receiver = ls_adr6-smtp_addr.
              <ls_receiver>-receiver_type = '1'.
            ENDLOOP.
          ENDIF.
        ENDIF.

*-----Send Mail with PDF
        CALL FUNCTION 'Z_BC_SEND_EMAIL_WITH_PDF_A'
          EXPORTING
            pi_subject            = l_subject
*           PI_SENDER             =
            pt_spoolids           = lt_spoolids
            pi_pdf_xstring        = pdf_xstring
            pi_attachment_subject = l_attachment_subject
            pi_skip_commit        = 'X'
          TABLES
            pt_receivers          = lt_receivers
            pt_body               = lt_body
          EXCEPTIONS
            document_not_sent     = 1
            find_no_spoolid       = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
*     Implement suitable error handling here
        ENDIF.

      ENDIF.

      IF iv_preview = abap_true.
        CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'    "'Z_BC_SSFCOMP_PDF_PREVIEW'
          EXPORTING
            i_otf                    = ls_job_info-otfdata
          EXCEPTIONS
            convert_otf_to_pdf_error = 1
            cntl_error               = 2
            OTHERS                   = 3.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        RETURN.
      ENDIF.

    WHEN 1.
*      MESSAGE e001(00) WITH 'FORMATTING_ERROR'.
      RAISE formatting_error.
    WHEN 2.
*      MESSAGE e001(00) WITH 'INTERNAL_ERROR'.
      RAISE internal_error.
    WHEN 3.
*      MESSAGE e001(00) WITH 'SEND_ERROR'.
      RAISE send_error.
    WHEN 4.
*      MESSAGE e001(00) WITH 'USER_CANCELED'.
      RAISE user_canceled.
    WHEN OTHERS.
*      MESSAGE e001(00) WITH 'OTHERS_ERROR'.
      RAISE others_error.
  ENDCASE.

ENDFUNCTION.
