REPORT yukm_commts.

DATA:
  et_ukm_totals        TYPE TABLE OF  ukm_totals,
  et_ukm_totals_source TYPE TABLE OF  ukm_s_totals_source.

CALL FUNCTION 'UKM_COMMTS_READ'
  EXPORTING
    i_partner            = 'HE0032'
    i_segment            = 'Z01'
*   I_DATE               =
*   I_INFERIOR           = 'X'
*   I_BUFFER             =
*   I_STAT               =
  TABLES
    et_ukm_totals        = et_ukm_totals
    et_ukm_totals_source = et_ukm_totals_source.

DATA: et_totals_display TYPE TABLE OF  ukm_s_totals_display.

CALL FUNCTION 'UKM_COMMTS_TOTALS_CALCULATE'
  TABLES
    it_totals_source          = et_ukm_totals_source
    et_totals_display         = et_totals_display
  EXCEPTIONS
    currency_conversion_error = 1
    OTHERS                    = 2.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

BREAK-POINT.

*zcl_crm_utilities=>simple_popup_alv( EXPORTING i_title = 'UKM_COMMTS_TOTALS_CALCULATE'
*                                               i_popup = abap_false
*                                               it_alv  = et_totals_display
*                                              ).
