FORM send_spool_by_pdf  USING     pv_otfdata TYPE ssfcrescl-otfdata
                         CHANGING c_spoolid TYPE rspoid
                                  pdf_xtring TYPE xstring.

  DATA: ct_tline TYPE TABLE OF tline,
        cs_tline TYPE tline.
  DATA: spoolid  TYPE  rspoid.

  DATA: copynumber  LIKE  itcpo-tdcopies.

  CLEAR: ct_tline.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
        max_linewidth         = 134
*        copynumber            = copynumber
      TABLES
        otf                   = pv_otfdata
        lines                 = ct_tline
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.
    IF sy-subrc = 0.
    ENDIF.

* ->Convert PDF output to XSTRING
    DATA: lv_pdfsource TYPE xstring.
    FIELD-SYMBOLS:<p> TYPE x. " <p> type any.

    CLEAR lv_pdfsource.
    LOOP AT ct_tline INTO cs_tline.

      ASSIGN cs_tline TO <p> CASTING TYPE x.

      CONCATENATE lv_pdfsource <p> INTO lv_pdfsource IN BYTE MODE.
    ENDLOOP.

* ->Create spool request in PDF format
    CALL FUNCTION 'ADS_CREATE_PDF_SPOOLJOB'
      EXPORTING
        printer         = 'PDF1'            "Printer name supporting PDF device type
*       DEST            =
        pages           = 1
        pdf_data        = lv_pdfsource       "XSTRING internal table
*       NAME            =
*       SUFFIX1         =
*       SUFFIX2         =
*       copies          =
*       PRIO            =
        immediate_print = ''
*       AUTO_DELETE     =
*       TITLELINE       =
*       RECEIVER        =
*       DIVISION        =
*       AUTHORITY       =
*       LIFETIME        = ‘0’
      IMPORTING
        spoolid         = spoolid
* EXCEPTIONS
*       NO_DATA         = 1
*       NOT_PDF         = 2
*       WRONG_DEVTYPE   = 3
*       OPERATION_FAILED        = 4
*       CANNOT_WRITE_FILE      = 5
*       DEVICE_MISSING  = 6
*       NO_SUCH_DEVICE  = 7
*       OTHERS          = 8
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE.
      c_spoolid = spoolid.
      pdf_xtring = lv_pdfsource.
    ENDIF.

ENDFORM.
