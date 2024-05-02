FUNCTION z_bc_send_email_with_pdf_a .
*"----------------------------------------------------------------------
*"*"區域介面：
*"  IMPORTING
*"     VALUE(PI_SUBJECT) TYPE  SO_OBJ_DES
*"     VALUE(PI_SENDER) TYPE  AD_SMTPADR OPTIONAL
*"     VALUE(PT_SPOOLIDS) TYPE  TSFSPOOLID OPTIONAL
*"     VALUE(PI_PDF_XSTRING) TYPE  XSTRING OPTIONAL
*"     VALUE(PI_ATTACHMENT_SUBJECT) TYPE  SO_OBJ_DES
*"     VALUE(PI_SKIP_COMMIT) TYPE  FLAG OPTIONAL
*"  TABLES
*"      PT_RECEIVERS STRUCTURE  ZSEMAIL_RECEIVER_A
*"      PT_BODY TYPE  SOLI_TAB OPTIONAL
*"  EXCEPTIONS
*"      DOCUMENT_NOT_SENT
*"      FIND_NO_SPOOLID
*"----------------------------------------------------------------------

  DATA:
    l_document    TYPE REF TO cl_document_bcs,
    l_attsubj     TYPE sood-objdes,
    l_attach_size LIKE sood-objlen,
    bcs_exception TYPE REF TO cx_bcs,
    sent_to_all   TYPE os_boolean,
    mail_address  TYPE adr6-smtp_addr.

  DATA:
    l_xf         TYPE x,
    l_fpath(128) TYPE c,
    i1           TYPE i,
    i_fsize      TYPE i,
    wa           LIKE solix,
    filebindat   TYPE solix_tab,
    s_filebindat LIKE LINE OF filebindat,
    l_errtype    TYPE bcs_cxerr.

  DATA:
    d_binsize TYPE i,
    d_binfile TYPE xstring,
    d_subject TYPE so_obj_des.
  DATA: t_otf      TYPE TABLE OF itcoo,
        t_lines    TYPE TABLE OF tline,
        t_bintab   TYPE solix_tab,
        t_bodymail TYPE bcsy_text.

  DATA:
    l_attach    TYPE bcsy_text, " Attachment
    wa_text     TYPE soli,       "Work area for attach
    l_extension TYPE soodk-objtp VALUE 'OTF', " TXT format
    l_size      TYPE sood-objlen, " Size of Attachment
    l_lines     TYPE i.

  DATA: l_copy TYPE os_boolean.

  DATA: error_message TYPE string.

  DATA: BEGIN OF itab OCCURS 0,
          field(256),
        END   OF itab.

  DATA: big_string TYPE xstring,
        it_solix   TYPE solix_tab,
        gt_soli    TYPE soli_tab,
        gs_soli    TYPE soli.

  DATA: w_subject      TYPE so_obj_des,
        s_ret_msg      LIKE swr_messag,
        w_body         TYPE bcsy_text,
        l_send_request TYPE REF TO cl_bcs,
        l_sender       TYPE REF TO if_sender_bcs,    " SENDER ADDRESS
        l_sender2      TYPE REF TO cl_cam_address_bcs,
        l_recipient    TYPE REF TO if_recipient_bcs.  " RECIPIENT

  DATA rq       TYPE tsp01.
  DATA bin_size TYPE i.
  DATA dummy    TYPE TABLE OF rspoattr.

* for mail
  DATA send_request  TYPE REF TO cl_bcs.
  DATA document      TYPE REF TO cl_document_bcs.
  DATA recipient     TYPE REF TO if_recipient_bcs.
  DATA pdf_size      TYPE so_obj_len.
  DATA pdf_content   TYPE solix_tab.
  DATA pdf_xstring   TYPE xstring.


*&--------------------------------------------------------------------*
*& One spool = One mail to send
*&--------------------------------------------------------------------*
  LOOP AT pt_spoolids INTO DATA(ls_spool).

*&---------------------------------------------------------------------*
*& Create PDF
*&---------------------------------------------------------------------*
* Create PDF Content
* 1) get attributes of spool request
* 2) convert spool request to PDF dependent on document type
*----------------------------------------------------------------------*
*   ------------ get attributes of spool request ---------------------
    CALL FUNCTION 'RSPO_GET_ATTRIBUTES_SPOOLJOB'
      EXPORTING
        rqident     = ls_spool
      IMPORTING
        rq          = rq
      TABLES
        attributes  = dummy
      EXCEPTIONS
        no_such_job = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
*   error handling
      RAISE find_no_spoolid.
    ENDIF.
*   --- convert spool request into PDF, dependent on document type ---
    IF rq-rqdoctype = 'OTF' OR rq-rqdoctype = 'SMART'.
      CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid              = ls_spool
          no_dialog                = 'X'
          pdf_destination          = 'X'
          no_background            = ' '
        IMPORTING
          pdf_bytecount            = bin_size
          bin_file                 = pdf_xstring
        EXCEPTIONS
          err_no_otf_spooljob      = 1
          err_no_spooljob          = 2
          err_no_permission        = 3
          err_conv_not_possible    = 4
          err_bad_dstdevice        = 5
          user_cancelled           = 6
          err_spoolerror           = 7
          err_temseerror           = 8
          err_btcjob_open_failed   = 9
          err_btcjob_submit_failed = 10
          err_btcjob_close_failed  = 11
          OTHERS                   = 12.
      IF sy-subrc <> 0.
        RAISE document_not_sent.
      ELSE.
        pdf_size = bin_size.
      ENDIF.

    ELSEIF rq-rqdoctype = 'LIST'.
      CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid              = ls_spool
          no_dialog                = 'X'
          pdf_destination          = 'X'
          no_background            = ' '
        IMPORTING
          pdf_bytecount            = bin_size
          bin_file                 = pdf_xstring
        EXCEPTIONS
          err_no_abap_spooljob     = 1
          err_no_spooljob          = 2
          err_no_permission        = 3
          err_conv_not_possible    = 4
          err_bad_destdevice       = 5
          user_cancelled           = 6
          err_spoolerror           = 7
          err_temseerror           = 8
          err_btcjob_open_failed   = 9
          err_btcjob_submit_failed = 10
          err_btcjob_close_failed  = 11
          OTHERS                   = 12.
      IF sy-subrc <> 0.
*   error handling
        RAISE document_not_sent.
      ELSE.
        pdf_size = bin_size.
      ENDIF.

    ELSEIF rq-rqdoctype = 'ADSP'.

      pdf_xstring = pi_pdf_xstring.
      pdf_size = xstrlen( pdf_xstring ).

    ELSE.
*   error handling
      RAISE document_not_sent.
    ENDIF.

    w_body[] = CORRESPONDING #( pt_body[] ).

*&---------------------------------------------------------------------*
*& Send mail
*&---------------------------------------------------------------------*
    TRY.

*     -------- create persistent send request ------------------------
        send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document -------------------------------
        pdf_content = cl_document_bcs=>xstring_to_solix( pdf_xstring ).

        document = cl_document_bcs=>create_document(
          i_type       = 'HTM'
          i_text       = w_body
          i_length     = pdf_size
*          i_importance = '1'
          i_subject    = pi_subject ).                      "#EC NOTEXT

        document->add_attachment(
          i_attachment_type     = 'PDF'
          i_attachment_subject  = pi_attachment_subject
          i_attachment_language = sy-langu
          i_attachment_size     = pdf_size
          i_att_content_hex     = pdf_content ).


*     add document object to send request
        send_request->set_document( document ).

* SENDER ADDESS
        IF pi_sender IS INITIAL.
          l_sender = cl_sapuser_bcs=>create( sy-uname ).
          send_request->set_sender( l_sender ).
        ELSE.
          l_sender2 = cl_cam_address_bcs=>create_internet_address( pi_sender ).
          send_request->set_sender( l_sender2 ).
        ENDIF.

** RECIPIENT ADDRESS
        LOOP AT pt_receivers.
          mail_address = pt_receivers-receiver.
          l_recipient  = cl_cam_address_bcs=>create_internet_address( mail_address ).

          "1 : RECIVER 2. CC
          IF pt_receivers-receiver_type = '1'.
            l_copy  = space.
          ELSE.
            l_copy  = 'X'.
          ENDIF.

* ADD RECIPIENT ADDRESS TO SEND REQUEST
          CALL METHOD send_request->add_recipient
            EXPORTING
              i_recipient = l_recipient
              i_express   = 'X'
              i_copy      = l_copy.
        ENDLOOP.

        IF sy-subrc <> 0.
*   error handling
          RAISE document_not_sent.
        ELSE.

*--------------trigger E-Mail immediately
          send_request->set_send_immediately('X').

          send_request->send( ).

          IF pi_skip_commit IS INITIAL.
            COMMIT WORK.
          ENDIF.

        ENDIF.

*   ------------ exception handling ----------------------------------
*   replace this rudimentary exception handling with your own one !!!
      CATCH cx_bcs INTO bcs_exception.
        RAISE document_not_sent.
    ENDTRY.

  ENDLOOP.


ENDFUNCTION.
