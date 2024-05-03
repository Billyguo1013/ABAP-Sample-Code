
class ZCL_CRM_UTILITIES definition
  public
  final
  create public .

public section.

  class-data GC_SUCCESS type CHAR12 value 'SUCCESS' ##NO_TEXT.
  class-data GC_ERROR type CHAR12 value 'ERROR' ##NO_TEXT.

  class-methods GET_MAPPING
    returning
      value(RT_MAPPING) type ZVCRM0001_T .
  class-methods GET_SALES_MAPPING
    returning
      value(RT_SALES_MAPPING) type ZVCRM0003_T .
  class-methods GET_CURRENT_TIMESTAMP
    exporting
      value(E_TIMESTAMP) type CHAR20 .
  class-methods GET_PB_BY_RESALE
    importing
      value(IS_ZTCRM0003) type ZTCRM0003
    exporting
      value(ET_ZTCRM0004) type ZTCRM0004_T .
  class-methods GET_BP_NAME
    importing
      value(I_KUNNR) type KUNNR optional
      value(I_KUNNR_32) type ZKUNNR_32 optional
      value(I_PARTNER) type BU_PARTNER optional
    exporting
      value(E_NAME) type CHAR80 .
  class-methods CREATE_CONTRACT_NO
    exporting
      value(E_VBELN) type VBELN_VA .
  class-methods CREATE_SI_NO
    exporting
      value(E_SI_NO) type ZSI_NO .
  class-methods CREATE_PB_NO
    exporting
      value(E_PB_NO) type ZBUCKET_ID .
  class-methods CREATE_RESALE_NO
    exporting
      value(E_OBJID) type ZOBJID .
  class-methods CREATE_XMNO_DOC
    importing
      value(I_FULL_NAME) type RLGRAP-FILENAME
    exporting
      value(E_XMNO_DOC) type ZXMNO_DOC .
  class-methods CREATE_CM_NO
    exporting
      value(E_CMID) type ZCLAIMID .
  class-methods CREATE_CM_WF_DOC
    exporting
      value(E_DOC) type ZAPPROVE_GROUP .
  class-methods CREATE_IR_ID
    exporting
      value(E_IR_ID) type ZIRID .
  class-methods CREATE_PL_ID
    exporting
      value(E_PL_ID) type ZPPDOCID .
  class-methods CREATE_PP_ID
    exporting
      value(E_PP_ID) type ZPPOBJID .
  class-methods CREATE_PP_WF_DOC
    exporting
      value(E_DOC) type ZAPPROVE_GROUP .
  class-methods UPLOADFILE_SAVE_LOG
    importing
      value(I_LOG_TYPE) type ZLOG_TYPE
      value(I_XN_DESC) type ZXN_DESC
      value(I_FILENAME) type LOCALFILE
      value(I_MESSAGE) type CHAR220 .
  class-methods SIMPLE_POPUP_ALV
    importing
      value(I_START_COLUMN) type I default 25
      value(I_START_LINE) type I default 6
      value(I_END_COLUMN) type I default 140
      value(I_END_LINE) type I default 10
      value(I_TITLE) type STRING default 'ALV'
      value(I_POPUP) type FLAG default 'X'
      value(IT_ALV) type TABLE .
protected section.
private section.

  class-data MT_MAPPING type ZVCRM0001_T .
  class-data MT_SALES_MAPPING type ZVCRM0003_T .
ENDCLASS.



CLASS ZCL_CRM_UTILITIES IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_CM_NO
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_CMID                         TYPE        ZCLAIMID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_cm_no.

    DATA: number(10) TYPE c,
          object    TYPE nrobj VALUE 'ZCRMDOC',
          toyear    TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          toyear                  = toyear
          object                  = object
          subobject               = 'CM'
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        e_cmid = |{ number }| .  "ex.CM00000001
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_CM_WF_DOC
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_DOC                          TYPE        ZAPPROVE_GROUP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CREATE_CM_WF_DOC.

    DATA: number(10) TYPE c,
          object     TYPE nrobj VALUE 'ZWF_GROUP',
          toyear     TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = 'GC'
          object                  = object
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        e_doc =  |{ number }| .
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_CONTRACT_NO
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_VBELN                        TYPE        VBELN_VA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_contract_no.

    DATA: number(10) TYPE c,
          object    TYPE nrobj VALUE 'ZCRMDOC',
          toyear    TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          toyear                  = toyear
          object                  = object
          subobject               = 'CT'
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        e_vbeln =  |{ number }| .  "ex.CT00000001
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_IR_ID
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_IR_ID                        TYPE        ZIRID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_ir_id.

    DATA: number(10) TYPE c,
          object    TYPE nrobj VALUE 'ZCRMDOC',
          toyear    TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          toyear                  = toyear
          object                  = object
          subobject               = 'IR'
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        e_ir_id = |{ number }| .  "ex.IR00000001
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_PB_NO
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_PB_NO                        TYPE        ZBUCKET_ID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_pb_no.

    DATA: number(10) TYPE c,
          object    TYPE nrobj VALUE 'ZCRMDOC',
          toyear    TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          toyear                  = toyear
          object                  = object
          subobject               = 'PB'
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        e_pb_no = |{ number }| .  "ex.PB00000001

      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_PL_ID
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_PL_ID                        TYPE        ZPPDOCID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_pl_id.

    DATA: number(10) TYPE c,
          object     TYPE nrobj VALUE 'ZCRMDOC',
          toyear     TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          toyear                  = toyear
          object                  = object
          subobject               = 'PL'
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        e_pl_id = number.

      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_PP_ID
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_PP_ID                        TYPE        ZPPOBJID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_pp_id.

    DATA: number(10) TYPE c,
          object     TYPE nrobj VALUE 'ZCRMDOC',
          toyear     TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          toyear                  = toyear
          object                  = object
          subobject               = 'PP'
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        e_pp_id = number.

      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_PP_WF_DOC
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_DOC                          TYPE        ZAPPROVE_GROUP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_pp_wf_doc.

    DATA: number(10) TYPE c,
          object     TYPE nrobj VALUE 'ZWF_GROUP',
          toyear     TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = 'GP'
          object                  = object
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        e_doc =  |{ number }| .
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_RESALE_NO
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_OBJID                        TYPE        ZOBJID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_resale_no.

    DATA: number(10) TYPE c,
          object     TYPE nrobj VALUE 'ZRESALE',
          toyear     TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          toyear                  = toyear
          object                  = object
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        e_objid = |{ number }|.
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_SI_NO
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_SI_NO                        TYPE        ZSI_NO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_si_no.

    DATA: number(10) TYPE c,
          object    TYPE nrobj VALUE 'ZCRMDOC',
          toyear    TYPE nryear.

    toyear = sy-datum+0(4).

*<< LOCK
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          toyear                  = toyear
          object                  = object
          subobject               = 'SI'
        IMPORTING
          number                  = number
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

*<< UNLOCK
        CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
          EXPORTING
            object           = object
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        e_si_no =  |{ number }| .
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>CREATE_XMNO_DOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_FULL_NAME                    TYPE        RLGRAP-FILENAME
* | [<---] E_XMNO_DOC                     TYPE        ZXMNO_DOC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_xmno_doc.

    DATA:
      stripped_name TYPE string,
      file_path     TYPE string.


    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = i_full_name
      IMPORTING
        stripped_name = stripped_name
        file_path     = file_path
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*    TRY.
*        DATA(l_uuid) = cl_system_uuid=>if_system_uuid_static~create_uuid_c36( ).
*      CATCH cx_uuid_error INTO DATA(lo_root).
*        MESSAGE lo_root->get_text( ) TYPE 'E'.
*    ENDTRY.

    e_xmno_doc = stripped_name.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>GET_BP_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_KUNNR                        TYPE        KUNNR(optional)
* | [--->] I_KUNNR_32                     TYPE        ZKUNNR_32(optional)
* | [--->] I_PARTNER                      TYPE        BU_PARTNER(optional)
* | [<---] E_NAME                         TYPE        CHAR80
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_bp_name.

    DATA: l_partner TYPE but000-partner.

    DATA:
      centraldata             TYPE  bapibus1006_central,
      centraldataperson       TYPE  bapibus1006_central_person,
      centraldataorganization TYPE  bapibus1006_central_organ.

    IF ( i_kunnr IS INITIAL AND i_kunnr_32 IS INITIAL  AND i_partner IS INITIAL ).
      RETURN.
    ENDIF.

    IF i_kunnr IS SUPPLIED.
      l_partner = i_kunnr.
    ELSEIF i_kunnr_32 IS SUPPLIED.
      l_partner = i_kunnr_32.
    ELSEIF i_partner IS SUPPLIED.
      l_partner = i_partner.
    ENDIF.

    CONDENSE l_partner NO-GAPS.

    l_partner  = |{ l_partner  ALPHA = IN }|.


    SELECT SINGLE * INTO @DATA(ls_but000)
      FROM but000
      WHERE partner = @l_partner.
    IF sy-subrc <> 0.
    ELSE.
      CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
        EXPORTING
          businesspartner         = ls_but000-partner
          valid_date              = sy-datlo
          iv_req_mask             = 'X'
        IMPORTING
          centraldata             = centraldata
          centraldataperson       = centraldataperson
          centraldataorganization = centraldataorganization.

      IF centraldataperson IS NOT INITIAL.  "Person
        e_name = centraldataperson-fullname.
      ELSEIF centraldataorganization IS NOT INITIAL. "Org
        e_name = centraldataorganization-name1.
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>GET_CURRENT_TIMESTAMP
* +-------------------------------------------------------------------------------------------------+
* | [<---] E_TIMESTAMP                    TYPE        CHAR20
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_current_timestamp.

    DATA: l_timestamp(20) TYPE c.

    GET TIME STAMP FIELD DATA(ts).

    CONVERT TIME STAMP ts TIME ZONE sy-zonlo
                          INTO DATE DATA(date)
                          TIME DATA(time).

    l_timestamp = |{ date }| && |{ time }|.

    e_timestamp = l_timestamp.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>GET_MAPPING
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_MAPPING                     TYPE        ZVCRM0001_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mapping.

    IF mt_mapping[] IS INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_mapping "#EC CI_NOWHERE
        FROM zvcrm0001.
    ENDIF.

    rt_mapping[] = mt_mapping[].

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>GET_PB_BY_RESALE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ZTCRM0003                   TYPE        ZTCRM0003
* | [<---] ET_ZTCRM0004                   TYPE        ZTCRM0004_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_pb_by_resale.

    DATA: r_endcust TYPE RANGE OF ztcrm0004-endcust,
          r_zzshape TYPE RANGE OF ztcrm0004-zzshape.


    IF is_ztcrm0003-endcust IS NOT INITIAL.
      r_endcust = VALUE #( ( sign = 'I' option = 'EQ' low = is_ztcrm0003-endcust ) ).
    ENDIF.

    IF ( is_ztcrm0003-zzshape = 'S' OR is_ztcrm0003-zzshape = 'E' ).
      r_zzshape = VALUE #( sign = 'I' option = 'EQ' ( low = 'S' ) ( low = 'E' ) ).
    ELSE.
      r_zzshape = VALUE #( ( sign = 'I' option = 'EQ' low = is_ztcrm0003-zzshape ) ).
    ENDIF.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE @et_ztcrm0004
      FROM ztcrm0004
      WHERE quo_vbeln = @is_ztcrm0003-quo_vbeln
      AND   vkorg = @is_ztcrm0003-vkorg
      AND   kunnr = @is_ztcrm0003-kunnr
      AND   endcust IN @r_endcust
      AND   zzpart = @is_ztcrm0003-zzpart
      AND   zzshape IN @r_zzshape
      AND   avbl_qty > 0.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>GET_SALES_MAPPING
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_SALES_MAPPING               TYPE        ZVCRM0003_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_sales_mapping.

    IF mt_sales_mapping[] IS INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_sales_mapping
        FROM zvcrm0003
        WHERE vkorg = 'WCA1'.
    ENDIF.

    rt_sales_mapping[] = mt_sales_mapping[].

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>SIMPLE_POPUP_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_START_COLUMN                 TYPE        I (default =25)
* | [--->] I_START_LINE                   TYPE        I (default =6)
* | [--->] I_END_COLUMN                   TYPE        I (default =140)
* | [--->] I_END_LINE                     TYPE        I (default =10)
* | [--->] I_TITLE                        TYPE        STRING (default ='ALV')
* | [--->] I_POPUP                        TYPE        FLAG (default ='X')
* | [--->] IT_ALV                         TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD simple_popup_alv.

    DATA: lo_alv     TYPE REF TO cl_salv_table,
          lo_display TYPE REF TO cl_salv_display_settings.

    DATA: l_title TYPE lvc_title.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = it_alv[] ).

      CATCH cx_salv_msg.
    ENDTRY.

    DATA: lr_functions TYPE REF TO cl_salv_functions_list.

    lr_functions = lo_alv->get_functions( ).
    lr_functions->set_all( 'X' ).

    IF lo_alv IS BOUND.
      IF i_popup = 'X'.
        lo_display = lo_alv->get_display_settings( ).
        l_title = i_title.
        lo_display->set_list_header( l_title ).
        lo_alv->set_screen_popup(
          start_column = i_start_column
          end_column  = i_end_column
          start_line  = i_start_line
          end_line    = i_end_line ).
      ENDIF.

      lo_alv->display( ).

    ENDIF.

    FREE lo_alv.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_UTILITIES=>UPLOADFILE_SAVE_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LOG_TYPE                     TYPE        ZLOG_TYPE
* | [--->] I_XN_DESC                      TYPE        ZXN_DESC
* | [--->] I_FILENAME                     TYPE        LOCALFILE
* | [--->] I_MESSAGE                      TYPE        CHAR220
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD uploadfile_save_log.

    DATA: lt_log  TYPE TABLE OF ztcrm0010.

    get_current_timestamp( IMPORTING e_timestamp = DATA(l_timestamp) ).

    CLEAR lt_log.
    APPEND INITIAL LINE TO lt_log ASSIGNING FIELD-SYMBOL(<log>).
    <log>-timestamp = l_timestamp.
    <log>-log_type = i_log_type.
    <log>-xn_desc = i_xn_desc.
    <log>-filename = i_filename.
    <log>-message =  i_message.

    IF lt_log IS NOT INITIAL.
      MODIFY ztcrm0010 FROM TABLE lt_log.
      COMMIT WORK AND WAIT.

      CLEAR lt_log[].
    ENDIF.

  ENDMETHOD.
ENDCLASS.
