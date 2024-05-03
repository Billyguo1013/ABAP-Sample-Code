FUNCTION z_mm_inbound_create_a.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PI_HEADER) TYPE  ZSMM004_DN_HEADER_A
*"     VALUE(PI_TEST) TYPE  BAPIE1GLOBAL_DATA-TESTRUN
*"  EXPORTING
*"     VALUE(PE_DELIVERY) TYPE  LIPS-VBELN
*"  TABLES
*"      PT_ITEM STRUCTURE  ZSMM004_DN_ITEM_A OPTIONAL
*"      PT_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
*** Local Variables
* Global data declarations
  DATA:
    lf_ebeln   TYPE ekko-ebeln,
    lf_type    TYPE msgty,
    po_message TYPE bapi_msg.
  READ TABLE pt_item INDEX 1.
  IF pt_item-werks = '9110' AND pt_item-vgbel = ''.
    CALL FUNCTION 'Z_MM_PO_CREATE_A'
      EXPORTING
        pi_header  = pi_header
        pi_test    = pi_test
      IMPORTING
        pe_ebeln   = lf_ebeln
        pe_type    = lf_type
        pe_message = po_message
      TABLES
        pt_item    = pt_item
        pt_return  = pt_return.
  ENDIF.
  CHECK pt_return[] IS INITIAL.
  DATA lt_vbsk LIKE vbsk OCCURS 0 WITH HEADER LINE.
  DATA lt_komdlgn LIKE komdlgn OCCURS 0 WITH HEADER LINE.
  DATA lt_vbfs LIKE vbfs OCCURS 0 WITH HEADER LINE.
  DATA lt_vbls LIKE vbls OCCURS 0 WITH HEADER LINE.
  DATA lt_lips LIKE lips OCCURS 0 WITH HEADER LINE.
  DATA lf_len(2) TYPE n.
  DATA lf_pos(2) TYPE n.
  DATA lf_ind LIKE sy-tabix.           "Hilfsfeld Index
  DATA lf_v_vbeln LIKE likp-vbeln.     "From delivery Number
  DATA lf_b_vbeln LIKE likp-vbeln.     "To delivery Number
  DATA lt_prop LIKE wuebs OCCURS 100 WITH HEADER LINE.
**** Loading the lt_komdlgn
  LOOP AT pt_item. "into et_po_detail.
    CLEAR lt_komdlgn.
    if pi_header-lfdat = '19800101'.
      lt_komdlgn-lfdat = sy-datum.
    else.
      lt_komdlgn-lfdat = pi_header-lfdat.
    endif.
    lt_komdlgn-lifex = pi_header-lifex.
    lt_komdlgn-vornu = pi_header-gts_vornu.
    lt_komdlgn-traty = 'Z001'.              "運輸方式
    lt_komdlgn-traid = pi_header-itm_vygid. "貨櫃號碼
    lt_komdlgn-vsart = pi_header-vsart.
    lt_komdlgn-verur = pi_header-lifex.
    lt_komdlgn-lfart = 'EL'.


    lt_komdlgn-pstyv = 'ELN'.
    lt_komdlgn-posnr = pt_item-vgpos. "一定要放PO Item,不然會有錯
    lt_komdlgn-lifexpos = pt_item-z_inv_item.
    lt_komdlgn-matnr = pt_item-matnr.
    lt_komdlgn-werks = pt_item-werks.
    IF pt_item-werks = '9120'.
      lt_komdlgn-lgort = ''.
    ELSE.
      lt_komdlgn-lgort = pt_item-lgort.
    ENDIF.
    lt_komdlgn-lfimg = pt_item-lfimg.

    lf_len = strlen( pt_item-lichn ).
    IF lf_len > 15.
      lf_pos = lf_len - 14.
      lt_komdlgn-lichn = pt_item-lichn+lf_pos(15).
    ELSE.
      lt_komdlgn-lichn = pt_item-lichn.
    ENDIF.
    lt_komdlgn-vrkme = pt_item-vrkme.
    lt_komdlgn-volum = pt_item-volum.
    lt_komdlgn-voleh = pt_item-voleh.
    lt_komdlgn-brgew = pt_item-brgew.
    lt_komdlgn-gewei = pt_item-gewei.
    lt_komdlgn-ntgew = pt_item-ntgew.
    if pi_test = 'X'.
      if pt_item-vgbel <> ''.
        lt_komdlgn-vgbel = pt_item-vgbel.
        lt_komdlgn-vgpos = pt_item-vgpos.
      endif.
    else.
      lt_komdlgn-vgbel = pt_item-vgbel.
      lt_komdlgn-vgpos = pt_item-vgpos.
    endif.
    lt_komdlgn-bwart = pt_item-bwart.
    APPEND lt_komdlgn.

  ENDLOOP.


*** Code from IDOC_INPUT_DESADV1
*** Not Available in 3.1
  CALL FUNCTION 'ME_CONFIRMATION_VIA_EDI'
    TABLES
      t_kom         = lt_komdlgn
      errors        = lt_prop
    EXCEPTIONS
      error_message = 1.

  DELETE lt_prop WHERE msgno = '799' OR
                       msgno = '777'.
  READ TABLE lt_prop WITH KEY msgty = 'E'.
  IF sy-subrc = 0.
    LOOP AT lt_prop WHERE msgty = 'E'.
      PERFORM fill_bapireturn TABLES pt_return
                 USING  lt_prop-msgty lt_prop-msgid lt_prop-msgno
                        lt_prop-msgv1 lt_prop-msgv2 lt_prop-msgv3
                        lt_prop-msgv4.
    ENDLOOP.
  ELSE.
    IF pi_test = ''.
      LOOP AT lt_komdlgn.
* set default parameter
        lt_komdlgn-vgtyp = 'V'.
        lt_komdlgn-kzazu = 'X'.
        lt_komdlgn-lfart = 'EL'.
        IF lt_komdlgn-matnr = ''.
          READ TABLE pt_item WITH KEY vgbel = lt_komdlgn-vgbel
                                      vgpos = lt_komdlgn-vgpos.
          IF sy-subrc = 0.
            lt_komdlgn-matnr = pt_item-matnr.
          ENDIF.
        ENDIF.
        IF lt_komdlgn-werks = '9120'.
          CLEAR lt_komdlgn-lgort.
        ENDIF.
        MODIFY lt_komdlgn.
      ENDLOOP.

***** Create
      DATA: nrnr LIKE inri-nrrangenr.
      TABLES: tvsa.


      lt_vbsk-mandt = sy-mandt.
      lt_vbsk-ernam = sy-uname.
      lt_vbsk-erdat = sy-datum.
      lt_vbsk-uzeit = sy-uzeit.
* lt_vbsk-smart = 'L'.
      SELECT SINGLE * FROM tvsa WHERE smart = lt_vbsk-smart.
      IF sy-subrc <> 0.
*** Error Handling To be Done
* Meldung ins Protokoll
      ENDIF.

      nrnr = tvsa-numki.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = nrnr
          object      = 'RV_SAMMG'
        IMPORTING
          number      = lt_vbsk-sammg
        EXCEPTIONS
          OTHERS      = 1.
      IF sy-subrc <> 0.
*** Error Hadling TBD
* Meldung ins Protokoll
      ENDIF.

**** Call Core Function GN_DELIVERY_CREATE.
      CALL FUNCTION 'GN_DELIVERY_CREATE'
        EXPORTING
          vbsk_i      = lt_vbsk
          if_no_deque = 'X'
        IMPORTING
          vbsk_e      = lt_vbsk
        TABLES
          xkomdlgn    = lt_komdlgn
          xvbfs       = lt_vbfs
          xvbls       = lt_vbls
          xxlips      = lt_lips
        EXCEPTIONS
          OTHERS      = 1.

*** Error Handling
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        PERFORM fill_bapireturn TABLES pt_return
                                USING  sy-msgty sy-msgid sy-msgno
                                       sy-msgv1 sy-msgv2
                                       sy-msgv3 sy-msgv4.
      ELSE.
        DESCRIBE TABLE lt_vbls LINES lf_ind.
        READ TABLE lt_vbls INDEX 1.
        lf_v_vbeln = lt_vbls-vbeln_lif.
        READ TABLE lt_vbls INDEX lf_ind.
        lf_b_vbeln = lt_vbls-vbeln_lif.
        CLEAR lf_ind.
        IF lf_v_vbeln IS INITIAL.
          READ TABLE lt_lips INDEX 1.
          IF sy-subrc = 0.
            lf_v_vbeln = lt_lips-vbeln.
          ENDIF.
        ENDIF.
        IF lf_v_vbeln IS INITIAL.
          DO 3 TIMES.
            SELECT SINGLE vbeln FROM likp INTO lf_v_vbeln
              WHERE lifex     = pi_header-lifex
                AND gts_vornu = pi_header-gts_vornu
                AND traid     = pi_header-itm_vygid.
            IF sy-subrc = 0.
              EXIT.
            ELSE.
              WAIT UP TO '0.5' SECONDS.
            ENDIF.
          ENDDO.
        ENDIF.
        IF lf_v_vbeln IS NOT INITIAL.
          pe_delivery = lf_v_vbeln.
          PERFORM fill_bapireturn TABLES pt_return
                                  USING  'S'   "msgType
                                         'ME'  "msgid
                                         '780' "msgno
                                         lt_vbsk-vbnum "sy-msgv1
                                         lf_v_vbeln    "sy-msgv1
                                         lf_b_vbeln    "sy-msgv2
                                         sy-msgv4.
        ELSE.
          READ TABLE lt_vbfs WITH KEY msgty = 'E'.
          IF sy-subrc = 0.
            PERFORM fill_bapireturn TABLES pt_return
                             USING  lt_vbfs-msgty lt_vbfs-msgid lt_vbfs-msgno
                                    lt_vbfs-msgv1 lt_vbfs-msgv2 lt_vbfs-msgv3
                                    lt_vbfs-msgv4." sy-msgv4.
          ELSE.
            CLEAR pt_return.
            pt_return-type    = 'E'.
            pt_return-id      = '00'.
            pt_return-number  = '398'.
            pt_return-message = 'DN Create Fail !'.
            APPEND pt_return.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.



ENDFUNCTION.
