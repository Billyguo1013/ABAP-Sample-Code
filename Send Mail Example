********************************************************
* For Mail Use                                         *
********************************************************
DATA: gt_body  LIKE solisti1 OCCURS 0 WITH HEADER LINE.

TYPES:
       BEGIN OF ty_alert,
         vbeln    TYPE vbeln,
         znumber  TYPE symsgno,
         message  TYPE bapi_msg,
         ernam    TYPE ernam,
         erdat    TYPE erdat,
       END OF ty_alert.
DATA: gt_alert    TYPE TABLE OF ty_alert,
      gs_alert    TYPE ty_alert.


DEFINE mailcontent_1. "表頭,有底色
  concatenate &1
     '<td nowrap bgcolor="#000080" height="35">'
     '<font color="#FFFFFF" size="2" face="Arial">'
     &2 '</font></td>' &3
  into gt_body-line.
  append gt_body. clear gt_body.
END-OF-DEFINITION.

DEFINE mailcontent_2. "內文
  concatenate &1
     '<td nowrap height="16"><pre><font size="2" face="Arial">'
     &2 '</pre></font></td>' &3
     into gt_body-line.
  append gt_body. clear gt_body.
END-OF-DEFINITION.

DEFINE alpha_out.
  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = &1
    importing
      output = &2.
END-OF-DEFINITION.

*--------------------------------------------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BUILD_ALERTMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_ZTSD0084  text
*----------------------------------------------------------------------*
FORM build_alertmail  USING  ps_ztsd0085 TYPE ztsd0085.

  CLEAR gs_alert.
  MOVE-CORRESPONDING ps_ztsd0085 TO gs_alert.
  APPEND gs_alert TO gt_alert.

ENDFORM.                    " BUILD_ALERTMAIL
*&---------------------------------------------------------------------*
*&      Form  SEND_ALERTMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_alertmail .

  DATA: lt_ztsd0040 LIKE ztsd0040 OCCURS 0 WITH HEADER LINE.
  DATA: w_to  LIKE zswf0005 OCCURS 0 WITH HEADER LINE.
  DATA: w_subject LIKE zswf0000-msubject.
  DATA: l_userid TYPE sy-uname,
        l_address LIKE pa0105-usrid_long,
        w_doc_type LIKE zswf0000-doc_type,
        w_mtype LIKE zswf0000-mtype,
        s_ret_msg LIKE swr_messag.

*<< Check mail exist
  CHECK gt_alert IS NOT INITIAL.

*<< Duplicate check
  DELETE ADJACENT DUPLICATES FROM gt_alert COMPARING ALL FIELDS.

*<< Mail Subject
  SELECT SINGLE zzprogdesc INTO w_subject FROM ztsd0043 WHERE cprog = 'ZBSDSA0130'.

*<< Mail body
  PERFORM getalertmailbody.

*<< Get config
  SELECT reporttype recivetype address reporttype FROM ztsd0040
    INTO CORRESPONDING FIELDS OF TABLE lt_ztsd0040
   WHERE cprog = 'ZBSDSA0130'
     AND reporttype = '2'.
  LOOP AT lt_ztsd0040.
    w_to-receivetype = lt_ztsd0040-recivetype.
    w_to-otype       = 'P'.
    w_to-address     = lt_ztsd0040-address.
    APPEND w_to.
  ENDLOOP.

*<< Send Mail out
  w_doc_type = 'HTM'.
  w_mtype = 'I'.

  CALL FUNCTION 'Z_FWF0043'
    EXPORTING
      mgroup      = 'SD'
      subject     = w_subject
      doc_type    = w_doc_type
      mtype       = 'I'
    IMPORTING
      ret_msg     = s_ret_msg
    TABLES
      doc_content = gt_body
      receivers   = w_to.

  CLEAR: gt_body, gt_body[].

ENDFORM.                    " SEND_ALERTMAIL
*&---------------------------------------------------------------------*
*&      Form  GETALERTMAILBODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM getalertmailbody .
  DATA: l_date(10) TYPE c,
        l_sono(10) TYPE c,
        l_soitem(6) TYPE c,
        l_timestamp TYPE string.

  CONCATENATE '<table border="1" cellpadding="0" '
     'cellspacing="0" width="100%" height="73">'
       INTO gt_body-line.
  APPEND gt_body. CLEAR gt_body.

  mailcontent_1 '<tr>' 'Order' ''.
  mailcontent_1 '' 'Message Number' ''.
  mailcontent_1 '' 'Message Text' ''.
  mailcontent_1 '' 'ERNAM' ''.
  mailcontent_1 '' 'ERDAT' '</tr>'.

  LOOP AT gt_alert INTO gs_alert.
    alpha_out gs_alert-vbeln l_sono.

    mailcontent_2 '<tr>' l_sono ''.
    mailcontent_2 '' gs_alert-znumber ''.
    mailcontent_2 '' gs_alert-message ''.
    mailcontent_2 '' gs_alert-ernam ''.
    mailcontent_2 '' gs_alert-erdat '</tr>'.
  ENDLOOP.

  CONCATENATE '</table>' '</br></br> ' INTO gt_body-line.
  APPEND gt_body. CLEAR gt_body.
ENDFORM.                    " GETALERTMAILBODY
