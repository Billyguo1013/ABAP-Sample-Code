FUNCTION z_mm_po_create_a.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PI_HEADER) TYPE  ZSMM004_DN_HEADER_A
*"     VALUE(PI_TEST) TYPE  BAPIE1GLOBAL_DATA-TESTRUN
*"  EXPORTING
*"     VALUE(PE_EBELN) TYPE  EKKO-EBELN
*"     VALUE(PE_TYPE) TYPE  MSGTY
*"     VALUE(PE_MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      PT_ITEM STRUCTURE  ZSMM004_DN_ITEM_A OPTIONAL
*"      PT_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
  DEFINE assign_value.
    IF &1 <> ''.
      &2 = &1.
      &3 = c_x.
    ENDIF.
  END-OF-DEFINITION.

  CONSTANTS:
    c_x VALUE 'X'.

  DATA:
    ls_cuky_dec      TYPE bapi1090_1,
    lf_amt_dec       TYPE i,
    lf_cnt           TYPE i,
    lf_peinh         TYPE ekpo-peinh,
    lf_amt_c(18),
    part1(18),
    part2(18),
    poheader         TYPE bapimepoheader,
    poheaderx        TYPE bapimepoheaderx,
    no_price_from_po,
    no_authority,
    lf_ebelp         TYPE ekpo-ebelp,
    exppurchaseorder TYPE bapimepoheader-po_number,
    poitem           TYPE TABLE OF bapimepoitem WITH HEADER LINE,
    poschedule       TYPE TABLE OF bapimeposchedule WITH HEADER LINE,
    poschedulex      TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,
    poitemx          TYPE TABLE OF bapimepoitemx WITH HEADER LINE,
    poaccount        TYPE TABLE OF bapimepoaccount WITH HEADER LINE,
    poaccountx       TYPE TABLE OF bapimepoaccountx WITH HEADER LINE,
    pocond           TYPE TABLE OF bapimepocond WITH HEADER LINE,
    pocondx          TYPE TABLE OF bapimepocondx WITH HEADER LINE,
    potextheader     TYPE TABLE OF bapimepotextheader WITH HEADER LINE,
    potextitem       TYPE TABLE OF bapimepotext WITH HEADER LINE,
    lf_amount        TYPE bapicurx-bapicurx,
    t_return         TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CALL FUNCTION 'BAPI_CURRENCY_GETDECIMALS'
    EXPORTING
      currency          = pi_header-waers
    IMPORTING
      currency_decimals = ls_cuky_dec.

*  ls_cuky_dec-CURDECIMALS

  assign_value:
    pi_header-lifnr poheader-vendor     poheaderx-vendor,
    'NB'            poheader-doc_type   poheaderx-doc_type,
    sy-datum        poheader-doc_date   poheaderx-doc_date,
    'FPD0'          poheader-purch_org  poheaderx-purch_org,
    'D99'           poheader-pur_group  poheaderx-pur_group,
    '4491'          poheader-comp_code  poheaderx-comp_code,
    pi_header-waers poheader-currency   poheaderx-currency.


  LOOP AT pt_item.
    CLEAR: poitem,poitemx.
    lf_ebelp = lf_ebelp + 10.
    lf_peinh = 1.
    poitemx-po_item = lf_ebelp.
    assign_value:
      lf_ebelp      poitem-po_item     poitemx-po_itemx,
      pt_item-matnr poitem-material    poitemx-material,
      pt_item-lfimg poitem-quantity    poitemx-quantity,
      pt_item-vrkme poitem-po_unit     poitemx-po_unit,
      '0004'        poitem-conf_ctrl   poitemx-conf_ctrl,
      pt_item-werks poitem-plant       poitemx-plant,
      pt_item-lgort poitem-stge_loc    poitemx-stge_loc.

    IF pt_item-netpr <> 0.
      lf_amount = pt_item-netpr.
      WRITE pt_item-netpr TO lf_amt_c.
      SPLIT lf_amt_c AT '.' INTO part1 part2.
      SHIFT part2 RIGHT DELETING TRAILING space.
      SHIFT part2 RIGHT DELETING TRAILING '0'.
      SHIFT part2 LEFT DELETING LEADING space.
      IF NOT part2 CO ' 0'.
        lf_amt_dec = strlen( part2 ).
        IF lf_amt_dec > ls_cuky_dec-curdecimals.
          lf_cnt = lf_amt_dec - ls_cuky_dec-curdecimals.
          IF lf_cnt < 5.
            DO lf_cnt TIMES.
              lf_amount = lf_amount * 10.
              lf_peinh  = lf_peinh * 10.
            ENDDO.
          ENDIF.
        ENDIF.
      ENDIF.
      assign_value:
        lf_amount poitem-net_price  poitemx-net_price,
        lf_peinh  poitem-price_unit poitemx-price_unit,
        '2'       poitem-po_price   poitemx-po_price.
      no_price_from_po = 'X'.
    ELSE.
      no_price_from_po = ''.
    ENDIF.
    APPEND poitem.
    APPEND poitemx.

*   schedule data
    CLEAR: poschedule,poschedulex.
    poschedule-po_item    = lf_ebelp.
    poschedule-sched_line = 1.
    IF NOT pi_header-lfdat IS INITIAL.
      IF pi_header-lfdat = '19800101'.
        WRITE sy-datum TO poschedule-delivery_date.
      ELSE.
        WRITE pi_header-lfdat TO poschedule-delivery_date.
      ENDIF.
    ELSE.
      WRITE sy-datum TO poschedule-delivery_date.
    ENDIF.
    APPEND poschedule.

    poschedulex-po_item    = lf_ebelp.
    poschedulex-sched_line = 1.
    poschedulex-po_itemx = c_x.
    poschedulex-sched_linex = c_x.
    poschedulex-delivery_date = c_x.
    APPEND poschedulex.
    pt_item-vgpos = lf_ebelp.
    MODIFY pt_item.
  ENDLOOP.

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader         = poheader
      poheaderx        = poheaderx
      testrun          = pi_test
      no_authority     = no_authority
      no_price_from_po = no_price_from_po
    IMPORTING
      exppurchaseorder = exppurchaseorder
    TABLES
      return           = t_return
      poitem           = poitem
      poitemx          = poitemx
      poschedule       = poschedule
      poschedulex      = poschedulex.

  READ TABLE t_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    pe_type = 'E'.
    IF pi_test = ''.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
    LOOP AT t_return WHERE type = 'E'.
      IF t_return-id = 'BAPI' AND t_return-number = 1.
        CONTINUE.
      ENDIF.
      IF pe_message = ''.
        pe_message = t_return-message.
      ELSE.
        CONCATENATE pe_message t_return-message INTO pe_message
          SEPARATED BY ';'.
      ENDIF.
      APPEND t_return TO pt_return.
    ENDLOOP.
    LOOP AT pt_item.
      CLEAR pt_item-vgpos.
      MODIFY pt_item.
    ENDLOOP.
  ELSE.
    pe_type = 'S'.
    IF pi_test = ''.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      pe_ebeln = exppurchaseorder.
      LOOP AT pt_item.
        pt_item-vgbel = exppurchaseorder.
        MODIFY pt_item.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFUNCTION.
