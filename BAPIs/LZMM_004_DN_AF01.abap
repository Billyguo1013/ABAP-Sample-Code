*----------------------------------------------------------------------*
***INCLUDE LZMM_004_DN_AF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form fill_bapireturn
*&---------------------------------------------------------------------*
FORM fill_bapireturn TABLES pt_return STRUCTURE bapiret2
                      USING pf_msgty
                            pf_msgid
                            pf_msgno
                            pf_msgv1
                            pf_msgv2
                            pf_msgv3
                            pf_msgv4.
  pt_return-type       = pf_msgty.
  pt_return-id         = pf_msgid.
  pt_return-number     = pf_msgno.
  pt_return-message_v1 = pf_msgv1.
  pt_return-message_v2 = pf_msgv1.
  pt_return-message_v3 = pf_msgv1.
  pt_return-message_v4 = pf_msgv1.
  MESSAGE ID pf_msgid TYPE pf_msgty NUMBER pf_msgno
          INTO pt_return-message
          WITH pf_msgv1 pf_msgv2 pf_msgv3 pf_msgv4.
  append pt_return.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fill_bapiret2
*&---------------------------------------------------------------------*
FORM fill_bapiret2 USING iv_msgty TYPE bapi_mtype
                          iv_msgid       TYPE symsgid
                          iv_msgno       TYPE any
                          iv_msgv1       TYPE any
                          iv_msgv2       TYPE any
                          iv_msgv3       TYPE any
                          iv_msgv4       TYPE any
                          iv_parameter   TYPE bapi_param
                          iv_row         TYPE bapi_line
                          iv_field       TYPE bapi_fld
                CHANGING  et_bapiret2    TYPE bapirettab.

  DATA ls_bapiret2 TYPE bapiret2.
  STATICS: lv_system   TYPE bapilogsys.
  STATICS: lv_system_read TYPE flag.

  IF lv_system_read IS INITIAL.
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = lv_system
      EXCEPTIONS
        own_logical_system_not_defined = 01.
    IF NOT sy-subrc IS INITIAL.
      CLEAR lv_system.
    ENDIF.
    lv_system_read = 'X'.
  ENDIF.

  MESSAGE ID     iv_msgid
          TYPE   iv_msgty
          NUMBER iv_msgno
          WITH   iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4
          INTO   ls_bapiret2-message.

  ls_bapiret2-type        = sy-msgty.
  ls_bapiret2-id          = sy-msgid.
  ls_bapiret2-number      = sy-msgno.
  ls_bapiret2-message_v1  = sy-msgv1.
  ls_bapiret2-message_v2  = sy-msgv2.
  ls_bapiret2-message_v3  = sy-msgv3.
  ls_bapiret2-message_v4  = sy-msgv4.
  ls_bapiret2-parameter   = iv_parameter.
  ls_bapiret2-row         = iv_row.
  ls_bapiret2-field       = iv_field.
  ls_bapiret2-system      = lv_system.
  APPEND ls_bapiret2 TO et_bapiret2.

ENDFORM. " FILL_BAPIRET2
*----------------------------------------------------------------------*
*        START NEW SCREEN                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR gt_bdcdata.
  gt_bdcdata-program  = program.
  gt_bdcdata-dynpro   = dynpro.
  gt_bdcdata-dynbegin = 'X'.
  APPEND gt_bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        INSERT FIELD                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR gt_bdcdata.
  gt_bdcdata-fnam = fnam.
  gt_bdcdata-fval = fval.
  APPEND gt_bdcdata.
ENDFORM.
*&---------
