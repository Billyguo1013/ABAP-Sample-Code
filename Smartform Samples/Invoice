FUNCTION z_sd_invoice_print_a .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VBELN) TYPE  VBELN_VF
*"     VALUE(IV_PREVIEW) TYPE  FLAG DEFAULT ' '
*"     VALUE(IV_PDF_DOWNLOAD) TYPE  FLAG DEFAULT ' '
*"     VALUE(IV_DIR) TYPE  LOCALFILE OPTIONAL
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
  CONSTANTS: lc_formname TYPE tdsfname VALUE 'ZSFSD016'.

*--------------------------------------------------------------------*
* Common Data
*--------------------------------------------------------------------*
  DATA: lt_adrc TYPE TABLE OF adrc.
  DATA: ls_adrc TYPE adrc.
  DATA: ls_ztewm022a_a TYPE ztewm022a_a,
        ls_ztewm022b_a TYPE ztewm022b_a,
        ls_ztewm022c_a TYPE ztewm022c_a.

* Invoice Subtotal
  DATA: lv_lines_invoice   TYPE i,
        lv_ntgew_invoice   TYPE zssd016_detail_head_a-ntgew,
        lv_value_invoice   TYPE zssd016_detail_head_a-value_invoice,
        lv_pieces_invoice  TYPE zssd016_detail_head_a-pieces_invoice,
        lv_brgew_invoice   TYPE zssd016_detail_head_a-brgew_invoice,
        lv_pallets_invoice TYPE zssd016_detail_head_a-pallets_invoice,
        lv_cartons_invoice TYPE zssd016_detail_head_a-cartons_invoice.

* DN Subtotal
  DATA: lv_lines_dn     TYPE i,
        lv_value_dn     TYPE zssd016_detail_item_a-value_dn,
        lv_pieces_dn    TYPE zssd016_detail_item_a-pieces_dn,
        lv_brgew_dn     TYPE zssd016_detail_item_a-brgew_dn,
        lv_pallets_dn   TYPE zssd016_detail_item_a-pallets_dn,
        lv_cartons_dn   TYPE zssd016_detail_item_a-cartons_dn,
        lv_volum_sum_dn TYPE volum,
        lv_voleh_dn     TYPE voleh,
        lv_ntgew_dn     TYPE zssd016_detail_item_a-ntgew,
        lv_gewei_dn     TYPE gewei,
        lv_groes_dn     TYPE groes.

* Case Subtotal
  DATA: lv_lines       TYPE i,
        lv_value_case  TYPE zssd016_packing_info_h_a-value_case,
        lv_pieces_case TYPE zssd016_packing_info_h_a-pieces_case.

*--------------------------------------------------------------------*
* Smartforms data structure
*--------------------------------------------------------------------*
  DATA: ls_head TYPE zssd016_head_a.
  DATA: ls_detail_head TYPE zssd016_detail_head_a,
        lt_detail_item TYPE zttsd016_detail_item_a.


  SELECT SINGLE * INTO @DATA(ls_vbrk) FROM vbrk WHERE vbeln = @iv_vbeln.
  IF sy-subrc <> 0.
    RAISE find_no_data.
  ELSE.
    SELECT * INTO TABLE @DATA(lt_vbrp) FROM vbrp WHERE vbeln = @ls_vbrk-vbeln.
  ENDIF.

  IF lt_vbrp IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(lt_ztewm022b_a) FROM ztewm022b_a
      FOR ALL ENTRIES IN @lt_vbrp
      WHERE vbeln = @lt_vbrp-vgbel.
    IF sy-subrc = 0.
      SELECT * INTO TABLE @DATA(lt_ztewm022c_a) FROM ztewm022c_a
        FOR ALL ENTRIES IN @lt_ztewm022b_a
        WHERE vbeln = @lt_ztewm022b_a-vbeln.
    ENDIF.
  ELSE.
    RAISE find_no_data.
  ENDIF.

  READ TABLE lt_vbrp INTO DATA(ls_vbrp) INDEX 1.

*--------------------------------------------------------------------*
* Build Front Page
*--------------------------------------------------------------------*
  ls_head = CORRESPONDING #( ls_vbrk ).

  ls_head-fkdat_c = |{ ls_head-fkdat+0(4) }/{ ls_head-fkdat+4(2) }/{ ls_head-fkdat+6(2) }|.

* Get Company info.
  SELECT SINGLE * FROM t001 INTO @DATA(ls_t001) WHERE bukrs = @ls_vbrk-bukrs.
  IF sy-subrc = 0.
    ls_head-companyname = ls_t001-butxt.
    SELECT * FROM adrc INTO TABLE @lt_adrc WHERE addrnumber = @ls_t001-adrnr.
    IF sy-subrc = 0.
      CLEAR ls_adrc.
      READ TABLE lt_adrc INTO ls_adrc WITH KEY nation = ''.
      IF sy-subrc <> 0.
        READ TABLE lt_adrc INTO ls_adrc WITH KEY nation = 'I'.
      ENDIF.
      ls_head-house_num1 = ls_adrc-house_num1.
      ls_head-street = ls_adrc-street.
      ls_head-city1 = ls_adrc-city1.
      ls_head-country = ls_adrc-country.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM tvfkt INTO @DATA(ls_tvfkt) WHERE fkart = @ls_vbrk-fkart AND spras = 'E'.
  IF sy-subrc = 0.
    ls_head-vtext = ls_tvfkt-vtext.
  ENDIF.

* Get Sold-to
  PERFORM get_customer_master USING ls_vbrk-kunag
                                    'SOLD2'
                              CHANGING ls_head.

* Get Ship-to
  PERFORM get_customer_master USING ls_vbrp-kunwe_ana
                                   'SHIP2'
                              CHANGING ls_head.


  SELECT SINGLE * INTO @DATA(ls_vbkd) FROM vbkd
    WHERE vbeln = @ls_vbrp-aubel
    AND   posnr = @ls_vbrp-aupos.
  IF sy-subrc = 0.
* Order Shipping Type
    SELECT SINGLE bezei INTO ls_head-bezei FROM t173t WHERE vsart = ls_vbkd-vsart AND spras = 'E'.
  ENDIF.

* Order Payment Terms
  SELECT SINGLE text1 INTO ls_head-zterm_desc FROM t052u WHERE spras = 'M' AND zterm = ls_head-zterm .


* Unit Price Currency
  ls_head-unitpricecurr = ls_vbrp-waerk.

* Total Amount Currencyx
  ls_head-totalamtcurr = ls_vbrp-waerk.

* Sale Org. Comments
  SELECT SINGLE * INTO @DATA(ls_tvko) FROM tvko WHERE vkorg = @ls_vbrp-vkorg_auft.
  IF sy-subrc = 0.
    SELECT * INTO TABLE @lt_adrc FROM adrc WHERE addrnumber = @ls_tvko-adrnr.
    IF sy-subrc = 0.
      CLEAR ls_adrc.
      READ TABLE lt_adrc INTO ls_adrc WITH KEY nation = ''.
      IF sy-subrc <> 0.
        READ TABLE lt_adrc INTO ls_adrc WITH KEY nation = 'I'.
      ENDIF.
      ls_head-comments = |{ ls_adrc-str_suppl3 }{ ls_adrc-location }|.
    ENDIF.
  ENDIF.

* Splict Comments
  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline            = ls_head-comments
      outputlen           = 50
    IMPORTING
      out_line1           = ls_head-comment1
      out_line2           = ls_head-comment2
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

* Head Item
  LOOP AT lt_vbrp INTO ls_vbrp WHERE vbeln = ls_vbrk-vbeln.

    APPEND INITIAL LINE TO ls_head-item[] ASSIGNING FIELD-SYMBOL(<ls_head_item>).
    <ls_head_item> = CORRESPONDING #( ls_vbrp ).
    IF ls_vbrp-fkimg <> 0.
      <ls_head_item>-unit_price = ( ls_vbrp-netwr / ls_vbrp-fkimg ).
    ENDIF.

  ENDLOOP.

*--------------------------------------------------------------------*
* Build Details Page
*--------------------------------------------------------------------*

* Build Details header
  ls_detail_head = CORRESPONDING #( ls_head ).
  ls_detail_head-kunag = ls_vbrk-kunag.
  ls_detail_head-kunwe = ls_vbrp-kunwe_ana.
  ls_detail_head-inco1 = ls_vbrk-inco1.
  ls_detail_head-waerk = ls_vbrk-waerk.
  ls_detail_head-gewei = ls_vbrp-gewei.

  SORT lt_vbrp BY vbeln posnr vgbel vgpos.

* Build Details item
  LOOP AT lt_vbrp INTO ls_vbrp WHERE vbeln = ls_vbrk-vbeln.
    APPEND INITIAL LINE TO lt_detail_item ASSIGNING FIELD-SYMBOL(<ls_detail_item>).
    <ls_detail_item>-vbeln = ls_vbrp-vbeln.
    <ls_detail_item>-dn = ls_vbrp-vgbel.
  ENDLOOP.
  SORT lt_detail_item.
  DELETE ADJACENT DUPLICATES FROM lt_detail_item.

*--------------------------------------------------------------------*
* DN Item Level
*--------------------------------------------------------------------*
  LOOP AT lt_detail_item ASSIGNING <ls_detail_item>.

*--------------------------------------------------------------------*
* CASE Head Level
*--------------------------------------------------------------------*
*  Initialization
    lv_lines_dn  = lv_ntgew_dn = lv_value_dn = lv_pieces_dn = lv_brgew_dn = lv_pallets_dn = lv_cartons_dn = lv_volum_sum_dn  = 0.
    CLEAR: lv_voleh_dn, lv_gewei_dn, lv_groes_dn.

    LOOP AT lt_ztewm022b_a INTO ls_ztewm022b_a WHERE vbeln = <ls_detail_item>-dn.

      APPEND INITIAL LINE TO <ls_detail_item>-cases[] ASSIGNING FIELD-SYMBOL(<ls_case>).
      <ls_case>-zbox_id = ls_ztewm022b_a-zbox_id.

*--------------------------------------------------------------------*
* CASE Item Level
*--------------------------------------------------------------------*
*  Initialization
      lv_lines = lv_value_case = lv_pieces_case = 0.

      LOOP AT lt_ztewm022c_a INTO ls_ztewm022c_a WHERE vbeln = ls_ztewm022b_a-vbeln
                                                  AND   zbox_id = ls_ztewm022b_a-zbox_id.

        APPEND INITIAL LINE TO <ls_case>-item[] ASSIGNING FIELD-SYMBOL(<ls_item>).
        <ls_item>-zbox_id = ls_ztewm022c_a-zbox_id.

        READ TABLE lt_vbrp INTO ls_vbrp WITH KEY vgbel = ls_ztewm022c_a-vbeln
                                                 vgpos = ls_ztewm022c_a-posnr.
        IF sy-subrc = 0.
*  Cust. Reference
          SELECT SINGLE bstkd INTO <ls_item>-bstkd FROM vbkd WHERE vbeln = ls_vbrp-aubel
                                                             AND   posnr = ls_vbrp-aupos.
*  Item#
          SELECT SINGLE posex INTO <ls_item>-posex FROM vbap WHERE vbeln = ls_vbrp-aubel
                                                             AND   posnr  = ls_vbrp-aupos.

*  Unit Price
          <ls_item>-unitprice = ( ls_vbrp-netwr / ls_vbrp-fkimg ).

*  Item Value
          <ls_item>-item_value = ( ls_vbrp-netwr / ls_vbrp-fkimg ) * ls_ztewm022c_a-zqty.

*  Currency Key
          <ls_item>-waerk = ls_vbrp-waerk.
        ENDIF.

        SELECT SINGLE * INTO @DATA(ls_mara) FROM mara WHERE matnr = @ls_ztewm022c_a-matnr.
        IF sy-subrc = 0.
          <ls_item>-zzsp_prefix = ls_mara-zzsp_prefix.
          <ls_item>-zzsp_base = ls_mara-zzsp_base.
          <ls_item>-zzsp_suffix = ls_mara-zzsp_suffix.
          SELECT SINGLE maktx FROM makt INTO <ls_item>-maktx WHERE matnr = ls_mara-matnr AND spras = 'E'.

          SELECT SINGLE * INTO @DATA(ls_marc) FROM marc WHERE matnr  = @ls_mara-matnr.
          IF sy-subrc = 0.
*  Country of Origin
            <ls_item>-herkl = ls_marc-herkl.
*  Net Weight
            <ls_item>-ntgew = ( ls_mara-ntgew * ls_ztewm022c_a-zqty ).
*  Unit of Weight
            <ls_item>-gewei = ls_mara-gewei.
*  CAS No.
            <ls_item>-casnr = ls_marc-casnr.
          ENDIF.
*  Quantity
          <ls_item>-zqty = ls_ztewm022c_a-zqty.
          WRITE ls_ztewm022c_a-zqty TO <ls_item>-zqty_c UNIT ls_mara-meins.
          CONDENSE <ls_item>-zqty_c.
        ENDIF.

        lv_value_case = lv_value_case + <ls_item>-item_value.
        lv_pieces_case = lv_pieces_case + ls_ztewm022c_a-zqty.

      ENDLOOP.

*--------------------------------------------------------------------*
*  Subtotal for case
*--------------------------------------------------------------------*
*   Line Item Count for Case
      lv_lines = lines( <ls_case>-item[] ).
      <ls_case>-lines = lv_lines.

      READ TABLE lt_ztewm022b_a INTO ls_ztewm022b_a WITH KEY zbox_id = <ls_case>-zbox_id.
      IF sy-subrc = 0..

*  Net Weight of Case
        <ls_case>-ntgew = ls_ztewm022b_a-ntgew.
        <ls_case>-gewei = ls_ztewm022b_a-gewei.

*  Gross Weight
        <ls_case>-brgew_case = ls_ztewm022b_a-brgew.

        SELECT SINGLE * INTO ls_ztewm022a_a FROM ztewm022a_a WHERE zpack_type = ls_ztewm022b_a-zpack_type.
        IF sy-subrc = 0.
*  Volume of Case
          WRITE ls_ztewm022a_a-volum TO <ls_case>-volum UNIT ls_ztewm022a_a-voleh.
          CONDENSE <ls_case>-volum.
          <ls_case>-volum = |{ <ls_case>-volum } { ls_ztewm022a_a-voleh }|.
*  Size/dimensions
          <ls_case>-groes_case = ls_ztewm022a_a-groes.
          CONDENSE <ls_case>-groes_case NO-GAPS.

          lv_volum_sum_dn = lv_volum_sum_dn + ls_ztewm022a_a-volum.
          lv_voleh_dn = ls_ztewm022a_a-voleh.

          CASE  ls_ztewm022a_a-zpack_cat.
            WHEN 'P'.
              "Amount of Pallets
              lv_pallets_dn = lv_pallets_dn + 1.
            WHEN 'C'.
              "Amount of Cartons
              lv_cartons_dn = lv_cartons_dn + 1.
            WHEN OTHERS.
              "do nothing
          ENDCASE.

        ENDIF.

        lv_ntgew_dn = lv_ntgew_dn + <ls_case>-ntgew.
        lv_gewei_dn = <ls_case>-gewei.
      ENDIF.
*  Currency
      <ls_case>-waerk = ls_vbrk-waerk.

*  Total value of Case
      <ls_case>-value_case = lv_value_case.

*  Total Pieces of Case
      <ls_case>-pieces_case = lv_pieces_case.

*<< Sum calculating for DN
      lv_lines_dn = lv_lines_dn + <ls_case>-lines.
      lv_value_dn = lv_value_dn + <ls_case>-value_case.
      lv_pieces_dn = lv_pieces_dn + <ls_case>-pieces_case.
      lv_brgew_dn = lv_brgew_dn + <ls_case>-brgew_case.
      lv_groes_dn = <ls_case>-groes_case.
    ENDLOOP.

*--------------------------------------------------------------------*
*  Subtotal for DN
*--------------------------------------------------------------------*
    <ls_detail_item>-lines = lv_lines_dn.
    <ls_detail_item>-ntgew = lv_ntgew_dn.
    <ls_detail_item>-value_dn = lv_value_dn.
    WRITE lv_volum_sum_dn TO <ls_detail_item>-volum UNIT lv_voleh_dn.
    CONDENSE <ls_detail_item>-volum.
    <ls_detail_item>-volum = |{ <ls_detail_item>-volum } { lv_voleh_dn }|.
    <ls_detail_item>-waerk = ls_vbrk-waerk.
    <ls_detail_item>-gewei = lv_gewei_dn.
    <ls_detail_item>-value_dn = lv_value_dn.
    <ls_detail_item>-pieces_dn = lv_pieces_dn.
    <ls_detail_item>-brgew_dn = lv_brgew_dn.
    <ls_detail_item>-groes_dn = lv_groes_dn.
    <ls_detail_item>-pallets_dn = lv_pallets_dn.
    <ls_detail_item>-cartons_dn = lv_cartons_dn.

*<< Sum calculating for Invoice
    lv_lines_invoice = lv_lines_invoice + <ls_detail_item>-lines.
    lv_ntgew_invoice = lv_ntgew_invoice + <ls_detail_item>-ntgew.
    lv_value_invoice = lv_value_invoice + <ls_detail_item>-value_dn.
    lv_pieces_invoice = lv_pieces_invoice + <ls_detail_item>-pieces_dn.
    lv_brgew_invoice = lv_brgew_invoice + <ls_detail_item>-brgew_dn.
    lv_pallets_invoice = lv_pallets_invoice + <ls_detail_item>-pallets_dn.
    lv_cartons_invoice = lv_cartons_invoice + <ls_detail_item>-cartons_dn.
  ENDLOOP.

*--------------------------------------------------------------------*
*  Total for Invoice
*--------------------------------------------------------------------*
  ls_detail_head-lines = lv_lines_invoice.
  ls_detail_head-ntgew = lv_ntgew_invoice.
  ls_detail_head-value_invoice = lv_value_invoice.
  ls_detail_head-pieces_invoice = lv_pieces_invoice.
  ls_detail_head-brgew_invoice = lv_brgew_invoice.
  ls_detail_head-pallets_invoice = lv_pallets_invoice.
  ls_detail_head-cartons_invoice = lv_cartons_invoice.


*--------------------------------------------------------------------*
*  Print Smartforms
*--------------------------------------------------------------------*
  DATA: fm_name           TYPE rs38l_fnam.
  DATA:
    lv_control_parameters TYPE ssfctrlop,
    ls_job_options        TYPE ssfcresop,
    ls_document           TYPE ssfcrespd,
    ls_job_info           TYPE ssfcrescl.

  DATA:l_output_options TYPE ssfcompop.

  DATA: lv_tabix     TYPE sy-tabix,
        l_amount(17) TYPE c.

  lv_control_parameters-device = 'PRINTER'.
  lv_control_parameters-no_dialog  = 'X'.
  lv_control_parameters-langu      = 'M'.
  l_output_options-tddest          = 'PDF1'.

  CASE abap_true.
    WHEN iv_preview.
      lv_control_parameters-preview = ' '.
      lv_control_parameters-getotf   = 'X'.
      l_output_options-tdnewid =  ' '.
    WHEN iv_pdf_download.
      lv_control_parameters-preview = ''.
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
      is_detail_head       = ls_detail_head
      it_detail_item       = lt_detail_item
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
  CASE sy-subrc.
    WHEN 0.

      et_otfdata[] = ls_job_info-otfdata[].
      IF ls_job_info-spoolids IS NOT INITIAL.
        MOVE ls_job_info-spoolids TO spoolids.
      ENDIF.

      IF ( iv_preview = abap_false AND iv_pdf_download = abap_false ).
        PERFORM send_spool_by_pdf USING ls_job_info-otfdata.
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

      IF iv_pdf_download = abap_true.
        PERFORM download_local USING ls_job_info-otfdata
                                     iv_vbeln
                                     iv_dir.
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
