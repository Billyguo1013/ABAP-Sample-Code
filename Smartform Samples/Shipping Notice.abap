FUNCTION z_mm_052_shippingnotice_a.
*"----------------------------------------------------------------------
*"*"區域介面：
*"  IMPORTING
*"     VALUE(P_VBELN) TYPE  VBELN_VL OPTIONAL
*"     VALUE(P_LIFEX) TYPE  LIFEX OPTIONAL
*"     VALUE(P_RESEND) TYPE  FLAG DEFAULT ' '
*"     VALUE(P_PREVIEW) TYPE  FLAG DEFAULT ' '
*"     VALUE(P_SKIP_COMMIT) TYPE  FLAG DEFAULT ' '
*"  EXCEPTIONS
*"      FIND_NO_DATA
*"      FORMATTING_ERROR
*"      INTERNAL_ERROR
*"      SEND_ERROR
*"      USER_CANCELED
*"      OTHERS_ERROR
*"      NO_SMARTFORMS_ZSFMM052_A
*"      SEND_MAIL_ERROR
*"      NO_MAIL_RECEIVER
*"----------------------------------------------------------------------

***********************************************************************
* SMARTFORM PARAMETERS
***********************************************************************
  CONSTANTS: lc_formname TYPE tdsfname VALUE 'ZSFMM052_A',
             lc_max_line TYPE i VALUE 36.
  DATA: lt_head TYPE zttmm052_head_a,
        ls_head LIKE LINE OF lt_head,
        lt_item TYPE zttmm052_item_a.

***********************************************************************
* LOCAL
***********************************************************************
  DATA: l_vbeln      TYPE vbeln_vl,
        l_z_inv_item TYPE z_inv_item.

  DATA: hhhmm TYPE string,
        days  TYPE i,
        hours TYPE i.

  DATA: lt_ztmm004_invh TYPE TABLE OF ztmm004_invh_a,
        lt_ztmm004_invi TYPE TABLE OF ztmm004_invi_a,
        lt_mail         TYPE TABLE OF ztmm052_mail_a,
        lt_log          TYPE TABLE OF ztmm052_send_a,
        lt_mard         TYPE TABLE OF mard,
        lt_lgpbe        TYPE TABLE OF ztmm052_lgpbe_a.

  DATA: lr_vbeln TYPE RANGE OF vbeln,
        lr_lifex TYPE RANGE OF lifex.

*--------------------------------------------------------------------*
* Initialize
*--------------------------------------------------------------------*
  CLEAR: l_vbeln, lt_ztmm004_invh, lt_ztmm004_invi, lt_mail, lt_log, lt_mard, lt_head, lt_item, lt_lgpbe, lr_vbeln, lr_lifex.

*--------------------------------------------------------------------*
* Check input
*--------------------------------------------------------------------*
  IF p_vbeln IS NOT INITIAL.
    lr_vbeln[] = VALUE #( ( sign = 'I' option = 'EQ' low = |{ p_vbeln ALPHA = IN }| ) ).
  ENDIF.

  IF p_lifex IS NOT INITIAL.
    lr_lifex[] = VALUE #( ( sign = 'I' option = 'EQ' low = |{ p_lifex ALPHA = IN }| ) ).
  ENDIF.

  IF ( p_vbeln IS INITIAL AND p_lifex IS INITIAL ).
    RETURN.
  ENDIF.

*--------------------------------------------------------------------*
* Retrieve Data
*--------------------------------------------------------------------*
  CASE p_resend.

    WHEN ''.

      SELECT a~*
        FROM ztmm004_invh_a AS a INNER JOIN lfa1 AS b ON a~lifnr = b~lifnr AND b~land1 = 'TW'  "國內供應商
        WHERE vbeln IN @lr_vbeln
        AND   lifex IN @lr_lifex
        AND   z_source = 'S'
        AND   zvl31n_status = 'S'
        AND NOT EXISTS ( SELECT * FROM ztmm052_send_a WHERE z_txn_id = a~z_txn_id
                                                      AND   lifex = a~lifex
                                                      AND   itm_vygid = a~itm_vygid
                       )
        INTO TABLE @lt_ztmm004_invh.

    WHEN 'X'.

      SELECT a~*
        FROM ztmm004_invh_a AS a INNER JOIN lfa1 AS b ON a~lifnr = b~lifnr AND b~land1 = 'TW' "國內供應商
        WHERE vbeln IN @lr_vbeln
        AND   lifex IN @lr_lifex
        AND   z_source = 'S'
        AND   zvl31n_status = 'S'
        AND EXISTS ( SELECT * FROM ztmm052_send_a WHERE z_txn_id = a~z_txn_id
                                                  AND   lifex = a~lifex
                                                  AND   itm_vygid = a~itm_vygid
                       )
        INTO TABLE @lt_ztmm004_invh.

  ENDCASE.

  IF sy-subrc <> 0.
    RAISE find_no_data.
  ENDIF.

  IF lt_ztmm004_invh IS NOT INITIAL.

    SELECT *
      INTO TABLE lt_ztmm004_invi
      FROM ztmm004_invi_a
      FOR ALL ENTRIES IN lt_ztmm004_invh
      WHERE z_txn_id = lt_ztmm004_invh-z_txn_id
      AND   lifex = lt_ztmm004_invh-lifex
      AND   itm_vygid = lt_ztmm004_invh-itm_vygid.

    SELECT *
      INTO TABLE lt_mail
      FROM ztmm052_mail_a
      FOR ALL ENTRIES IN lt_ztmm004_invh
      WHERE lifnr = lt_ztmm004_invh-lifnr.

  ENDIF.

*--------------------------------------------------------------------*
* 需按照ASN上傳之"交貨日期"及"交貨地點"進行跳頁
*--------------------------------------------------------------------*
  SORT lt_ztmm004_invi BY z_txn_id
                          lifex
                          itm_vygid
                          z_inv_item
                          matnr
                          werks
                          lgort.

  IF lt_ztmm004_invi IS NOT INITIAL.
    SELECT * INTO TABLE lt_mard FROM mard
      FOR ALL ENTRIES IN lt_ztmm004_invi
      WHERE matnr = lt_ztmm004_invi-matnr
      AND   werks = lt_ztmm004_invi-werks
      AND   lgort = lt_ztmm004_invi-lgort.
    IF sy-subrc = 0.
      SORT lt_mard.
      DELETE ADJACENT DUPLICATES FROM lt_mard COMPARING matnr werks lgort.

      SELECT * INTO TABLE lt_lgpbe FROM ztmm052_lgpbe_a
        FOR ALL ENTRIES IN lt_mard
        WHERE lgpbe = lt_mard-lgpbe.

    ENDIF.
  ENDIF.

  lt_item[] = CORRESPONDING #( lt_ztmm004_invi[] ).

  LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<it_item>).
    READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY matnr = <it_item>-matnr
                                                   werks = <it_item>-werks
                                                   lgort = <it_item>-lgort.
    IF sy-subrc = 0.
      <it_item>-lgpbe = ls_mard-lgpbe.
    ENDIF.
  ENDLOOP.

  SORT lt_item BY lgpbe
                  z_txn_id
                  lifex
                  itm_vygid
                  z_inv_item.

*--------------------------------------------------------------------*
* Build Smartform Parameters
*--------------------------------------------------------------------*
  LOOP AT lt_item INTO DATA(wa) GROUP BY ( lgpbe = wa-lgpbe
                                           z_txn_id = wa-z_txn_id
                                           lifex = wa-lifex
                                           itm_vygid = wa-itm_vygid
                                         )
                                ASCENDING
                                ASSIGNING FIELD-SYMBOL(<group>).

*--------------------------------------------------------------------*
* Build Smartform Header
*--------------------------------------------------------------------*
    CLEAR ls_head.

    ls_head = CORRESPONDING #( <group> ).
    ls_head-next_page = abap_true.
    READ TABLE lt_ztmm004_invh INTO DATA(ls_ztmm004_invh) WITH KEY z_txn_id = ls_head-z_txn_id
                                                                   lifex = ls_head-lifex
                                                                   itm_vygid = ls_head-itm_vygid.
    IF sy-subrc = 0.
      ls_head-lifnr = ls_ztmm004_invh-lifnr. "供應商編號
      ls_head-print_date = sy-datlo.   "列印日期
      ls_head-z_ship_date = ls_ztmm004_invh-z_ship_date.   "交貨日期
      SELECT SINGLE name1 INTO ls_head-vendor_name FROM lfa1 WHERE lifnr = ls_head-lifnr.   "供應商姓名

      "預計到達日 & 預計到達時段
      SELECT SINGLE * FROM /sapapo/loc INTO @DATA(ls_/sapapo/loc) WHERE locno = @ls_head-lifnr.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM /sapapo/trm INTO @DATA(ls_/sapapo/trm) WHERE locfr = @ls_/sapapo/loc-locid.
        IF sy-subrc = 0.
          CALL FUNCTION 'Z_MM_ETA_CAL_A'
            EXPORTING
              p_erdat    = ls_ztmm004_invh-erdat
              p_erzet    = ls_ztmm004_invh-erzet
              p_durat    = ls_/sapapo/trm-durat
            IMPORTING
              eta_date_c = ls_head-eta_date_c
              eta_time_c = ls_head-eta_time_c.
        ENDIF.
      ENDIF.
      IF ls_head-eta_date_c IS INITIAL.
        ls_head-eta_date = ls_ztmm004_invh-lfdat.
      ENDIF.
    ENDIF.

    "儲格地點說明
    READ TABLE lt_lgpbe INTO DATA(ls_lgpbe) WITH KEY lgpbe = ls_head-lgpbe.
    IF sy-subrc = 0.
      ls_head-location = ls_lgpbe-location.
    ENDIF.

* Date convert
    PERFORM date_convert USING ls_head-z_ship_date CHANGING ls_head-z_ship_date_c.
    PERFORM date_convert USING ls_head-print_date CHANGING ls_head-print_date_c.
    IF ls_head-eta_date_c IS INITIAL.
      PERFORM date_convert USING ls_head-eta_date CHANGING ls_head-eta_date_c.
    ENDIF.

*--------------------------------------------------------------------*
* Build Smartform Item
*--------------------------------------------------------------------*
    ls_head-item[] = VALUE #( FOR x IN lt_item WHERE ( lgpbe = ls_head-lgpbe AND
                                                       z_txn_id = ls_head-z_txn_id AND
                                                       lifex = ls_head-lifex AND
                                                       itm_vygid = ls_head-itm_vygid )
                                                     ( CORRESPONDING #( x ) ) ).

* Insert Enpty fields
    DATA(l_count) = lines( ls_head-item[] ).
    IF l_count < lc_max_line.
      DATA(l_times) = lc_max_line - l_count.
      DO l_times TIMES.
        APPEND INITIAL LINE TO ls_head-item[] ASSIGNING FIELD-SYMBOL(<ls_item>).
      ENDDO.
    ENDIF.

*  依交貨地點重新編號
    l_z_inv_item = 0.
    LOOP AT ls_head-item[] ASSIGNING <ls_item>.
      l_z_inv_item = l_z_inv_item + 1.
      <ls_item>-z_inv_item = l_z_inv_item.
    ENDLOOP.

    APPEND ls_head TO lt_head.
  ENDLOOP.

  DATA(l_index_max) = lines( lt_head ).
  READ TABLE lt_head ASSIGNING FIELD-SYMBOL(<ls_head>) INDEX l_index_max.
  IF sy-subrc = 0.
    <ls_head>-next_page = abap_false.
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
  DATA:
    lv_footer    TYPE char10.

  DO 2 TIMES.  "一式二聯

    CASE sy-index.
      WHEN 1.
        lv_control_parameters-no_open  = space.
        lv_control_parameters-no_close = 'X'.
        lv_footer = `（一）福特聯`.
      WHEN 2.
        lv_control_parameters-no_open  = 'X'.
        lv_control_parameters-no_close = space.
        lv_footer = `（二）廠商聯`.
    ENDCASE.

    lv_control_parameters-device = 'PRINTER'.
    lv_control_parameters-no_dialog  = 'X'.
    lv_control_parameters-langu      = 'M'.
    l_output_options-tddest          = 'PDF1 '.
*  l_output_options-tdcopies        = 2.

    CASE p_preview.
      WHEN ''.
        lv_control_parameters-preview = ' '.
        lv_control_parameters-getotf   = 'X'.
        l_output_options-tdnewid =  ''.
      WHEN 'X'.
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
      RAISE no_smartforms_zsfmm052_a.
    ENDIF.

    CALL FUNCTION fm_name
      EXPORTING
        control_parameters   = lv_control_parameters
        output_options       = l_output_options
        user_settings        = ' '
        it_head              = lt_head
        iv_footer            = lv_footer
      IMPORTING
        document_output_info = ls_document
        job_output_info      = ls_job_info
        job_output_option    = ls_job_options
      EXCEPTIONS
        formatting_error     = 1
        internal_error       = 2
        send_error           = 3
        user_canceled        = 4
        OTHERS               = 5.

  ENDDO.


  CASE sy-subrc.
    WHEN 0.

      IF p_preview = abap_false.
***-----Send PDF to SPOOL
        CLEAR lt_spoolids.
        PERFORM send_spool_by_pdf USING ls_job_info-otfdata CHANGING ls_spool pdf_xstring.
        APPEND ls_spool TO lt_spoolids.

        READ TABLE lt_head INTO ls_head INDEX 1.
        IF sy-subrc = 0.
          DATA(l_lifex) = |{ ls_head-lifex ALPHA = OUT }|.
          CONDENSE l_lifex.
        ENDIF.

*-----郵件標題
        l_subject = TEXT-001.
        REPLACE ALL OCCURRENCES OF '&1' IN l_subject WITH l_lifex.
*-----夾檔名稱
        l_attachment_subject = TEXT-002 && `_` && |{ sy-datlo }{ sy-timlo }|.
*-----郵件內文
        CLEAR lt_body[].
        lt_body[] = VALUE #( ( line = |附檔為您先前上傳之隨車交貨單號產出之交貨明細 ，<BR>| )
                             ( line = |請依據此文件內容安排交貨並進行出貨作業。<BR>| )
                             ( line = | <BR>| )
                             ( line = |此為系統自動通知信，請勿直接回信！<BR>| )
                             ( line = |<BR>| )
                             ( line = |祝 順頌商祺<BR>| )
                             ( line = |福特六和<BR>| )
                           ).
*-----寄送地址
        CLEAR lt_receivers.
        LOOP AT lt_mail INTO DATA(ls_mail).
          APPEND INITIAL LINE TO lt_receivers ASSIGNING FIELD-SYMBOL(<ls_receiver>).
          <ls_receiver>-receiver = ls_mail-smtp_addr.
          <ls_receiver>-receiver_type = '1'.
        ENDLOOP.
        IF sy-subrc = 0.
*-----Send Mail with PDF
          CALL FUNCTION 'Z_BC_SEND_EMAIL_WITH_PDF_A'
            EXPORTING
              pi_subject            = l_subject
*             PI_SENDER             =
              pt_spoolids           = lt_spoolids
              pi_pdf_xstring        = pdf_xstring
              pi_attachment_subject = l_attachment_subject
              pi_skip_commit        = p_skip_commit
            TABLES
              pt_receivers          = lt_receivers
              pt_body               = lt_body
            EXCEPTIONS
              document_not_sent     = 1
              find_no_spoolid       = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
*     Implement suitable error handling here
            RAISE send_mail_error.
          ELSE.
*-----Save Log
            lt_log[] = CORRESPONDING #( lt_head[] ).
            LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<ls_log>).
              <ls_log>-send_user = sy-uname.
              <ls_log>-send_date = sy-datlo.
              <ls_log>-send_time = sy-timlo.
            ENDLOOP.

            MODIFY ztmm052_send_a FROM TABLE lt_log.

          ENDIF.
        ELSE.
          RAISE no_mail_receiver.
        ENDIF.


      ENDIF.

      IF p_preview = abap_true.
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
      RAISE formatting_error.
    WHEN 2.
      RAISE internal_error.
    WHEN 3.
      RAISE send_error.
    WHEN 4.
      RAISE user_canceled.
    WHEN OTHERS.
      RAISE others_error.
  ENDCASE.


ENDFUNCTION.
