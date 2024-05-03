FUNCTION z_mm_006_create_dn_in_a.
*"----------------------------------------------------------------------
*"*"區域介面：
*"  TABLES
*"      PT_ZTMM006_REC_H STRUCTURE  ZTMM006_REC_H_A
*"      PT_ZTMM006_REC_I STRUCTURE  ZTMM006_REC_I_A
*"      CT_LOG STRUCTURE  ZTMM006_LOG_A OPTIONAL
*"----------------------------------------------------------------------


*  DATA: ls_head  LIKE bbp_inbd_l,
*        ls_item  LIKE bbp_inbd_d,
*        lt_item  LIKE TABLE OF bbp_inbd_d,
*        t_return LIKE TABLE OF bapireturn.

  DATA: ls_header TYPE zsmm004_dn_header_a,
        l_test    TYPE bapie1global_data-testrun.
  DATA: lt_item TYPE TABLE OF zsmm004_dn_item_a,
        ls_item LIKE LINE OF lt_item.

  DATA lv_dn LIKE likp-vbeln.
  DATA lv_posnr TYPE posnr.

  DATA: l_err_field TYPE z_err_fields_a,
        l_err_msg   TYPE z_error_message_a.

  LOOP AT pt_ztmm006_rec_h[] ASSIGNING FIELD-SYMBOL(<lfs_h>) WHERE z_status_asn = 'S'
                                                              AND   z_status_dn <> 'S'
                                                              AND   ( z_err_fields = '' AND z_error_message = '' ).

* initialize
    CLEAR: ls_header, lt_item.

    <lfs_h>-z_status_dn = 'P'.  "Processing...

*--------------------------------------------------------------------*
* 整理建立Inbound DN Header資料
*--------------------------------------------------------------------*
    ls_header-z_txn_id  = ''.
    ls_header-lifex     = ''.                   "發票號碼
    ls_header-lifnr     = <lfs_h>-gsdb_cd.
    ls_header-itm_vygid = ''.                   "貨櫃號碼
    ls_header-lfdat     = sy-datlo.             "交貨日期
    ls_header-gts_vornu = <lfs_h>-vendor_pcode. "ASN號碼
    ls_header-vsart     = ''.                   "運輸方式
    ls_header-waers     = ''.                   "發票幣別

    lv_posnr = '000000'.
    LOOP AT pt_ztmm006_rec_i ASSIGNING FIELD-SYMBOL(<lfs_i>) WHERE vendor_pcode = <lfs_h>-vendor_pcode
                                                                AND ship_no = <lfs_h>-ship_no.


      lv_posnr = lv_posnr + 10.
      ls_item-z_inv_item = lv_posnr.
      ls_item-matnr  = <lfs_i>-part_no.
      ls_item-lfimg  = <lfs_i>-rcv_qty.

      IF ( <lfs_i>-po_no IS NOT INITIAL AND <lfs_i>-po_item IS NOT INITIAL ).
        SELECT SINGLE * FROM ekpo INTO @DATA(ls_ekpo)
          WHERE ebeln = @<lfs_i>-po_no
          AND   ebelp = @<lfs_i>-po_item.
        IF sy-subrc = 0.
          ls_item-vrkme = ls_ekpo-meins.
        ENDIF.
      ENDIF.

      IF ls_item-vrkme IS INITIAL.
        ls_item-vrkme  = 'EA'.
      ENDIF.

      ls_item-lgort = <lfs_i>-sloc.
      IF ls_item-lgort IS INITIAL.
        ls_item-lgort = '1111'.
      ENDIF.

      ls_item-vgbel = <lfs_i>-po_no.
      ls_item-vgpos = <lfs_i>-po_item.
      ls_item-werks = '9120'.

      APPEND ls_item TO lt_item.
      CLEAR ls_item.
    ENDLOOP.

*--------------------------------------------------------------------*
* 建立Inbound DN
*--------------------------------------------------------------------*
    CLEAR: g_delivery.
    CLEAR: gt_return.

    CALL FUNCTION 'Z_MM_INBOUND_CREATE_A'
      EXPORTING
        pi_header   = ls_header
        pi_test     = l_test
      IMPORTING
        pe_delivery = g_delivery
      TABLES
        pt_item     = lt_item
        pt_return   = gt_return.

    LOOP AT gt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
    ENDLOOP.
    IF sy-subrc = 0.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      <lfs_h>-z_status_dn = 'E'.

      CLEAR: l_err_msg, l_err_field.

      LOOP AT ct_log[] ASSIGNING FIELD-SYMBOL(<lfs_log>) WHERE vendor_pcode = <lfs_h>-vendor_pcode
                                                            AND ship_no = <lfs_h>-ship_no.

        CHECK gt_return IS NOT INITIAL.

        CLEAR: l_err_msg,
               l_err_field.

        LOOP AT gt_return INTO DATA(s_return) WHERE type CA 'EAX'
                                               AND   message_v1 EQ <lfs_log>-po_item.  "先讀取 item 層級的訊息

          DATA(l_index_1) =  sy-tabix.

          IF l_err_msg IS INITIAL.
            l_err_msg = `[` && s_return-message && `]`.
          ELSE.
            l_err_msg = |{ l_err_msg }/[{ s_return-message }]|.
          ENDIF.

          IF l_err_field IS INITIAL.
            l_err_field = '[DELIVERY_CREATE]'.
          ELSE.
            l_err_field = |{ l_err_field }/[DELIVERY_CREATE]|.
          ENDIF.

          DELETE gt_return INDEX l_index_1.   "串完資料後即刪除

        ENDLOOP.

        LOOP AT gt_return INTO s_return WHERE type CA 'EAX'.  "再讀取 header 層級的訊息

          IF l_err_msg IS INITIAL.
            l_err_msg = `[` && s_return-message && `]`.
          ELSE.
            l_err_msg = |{ l_err_msg }/[{ s_return-message }]|.
          ENDIF.

          IF l_err_field IS INITIAL.
            l_err_field = '[DELIVERY_CREATE]'.
          ELSE.
            l_err_field = |{ l_err_field }/[DELIVERY_CREATE]|.
          ENDIF.

        ENDLOOP.

        <lfs_log>-z_status_dn = <lfs_h>-z_status_dn.
        <lfs_log>-z_err_fields = l_err_field.
        <lfs_log>-z_error_message = l_err_msg.


      ENDLOOP.

      <lfs_h>-z_err_fields = l_err_field.
      <lfs_h>-z_error_message = l_err_msg.

    ELSE.

      IF g_delivery <> ''.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        <lfs_h>-z_status_dn = 'S'.
        <lfs_h>-z_err_fields = ''.
        <lfs_h>-n951_h_date_time = |{ sy-datlo }| && |{ sy-timlo }|.
        <lfs_h>-vbeln = g_delivery.
        <lfs_h>-lfart = 'EL'.  "進項交貨單類型；固定為:”EL”
        <lfs_h>-erdat = sy-datlo.
        <lfs_h>-erzet = sy-timlo.

        lv_posnr = '000000'.
        LOOP AT pt_ztmm006_rec_i ASSIGNING <lfs_i> WHERE vendor_pcode = <lfs_h>-vendor_pcode
                                                      AND ship_no = <lfs_h>-ship_no.

          <lfs_i>-vbeln = g_delivery.
          lv_posnr = lv_posnr + 10.
          <lfs_i>-posnr = lv_posnr.

          READ TABLE ct_log[] ASSIGNING <lfs_log> WITH KEY vendor_pcode = <lfs_i>-vendor_pcode
                                                           ship_no = <lfs_i>-ship_no
                                                           item_id = <lfs_i>-item_id.
          IF sy-subrc = 0.
            <lfs_log>-z_status_dn = <lfs_h>-z_status_dn.
            <lfs_log>-z_err_fields = <lfs_h>-z_err_fields.
            <lfs_log>-z_error_message = ''.
            <lfs_log>-vbeln = g_delivery.
            <lfs_log>-posnr = <lfs_i>-posnr.
          ENDIF.

        ENDLOOP.

      ELSE.

        <lfs_h>-z_status_dn = 'E'.
        <lfs_h>-z_err_fields = '[DELIVERY_CREATE]'.
        <lfs_h>-z_error_message = '[DELIVERY CREATE ERROR]'.

        LOOP AT ct_log[] ASSIGNING <lfs_log> WHERE vendor_pcode = <lfs_h>-vendor_pcode
                                                 AND ship_no = <lfs_h>-ship_no.
          <lfs_log>-z_status_dn = <lfs_h>-z_status_dn.
          <lfs_log>-z_err_fields = <lfs_h>-z_err_fields.
          <lfs_log>-z_error_message = <lfs_h>-z_error_message.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDLOOP.
  IF sy-subrc = 0.
    IF pt_ztmm006_rec_h[] IS NOT INITIAL.
      MODIFY ztmm006_rec_h_a FROM TABLE pt_ztmm006_rec_h[].
    ENDIF.

    IF pt_ztmm006_rec_i[] IS NOT INITIAL.
      MODIFY ztmm006_rec_i_a FROM TABLE pt_ztmm006_rec_i[].
    ENDIF.

    IF ct_log[] IS NOT INITIAL.
      MODIFY ztmm006_log_a FROM TABLE ct_log[].
    ENDIF.
  ELSE.
    "do suitable data
  ENDIF.

ENDFUNCTION.
