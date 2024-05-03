FUNCTION zewm_convert_sysd_to_stmpl_a.
*"----------------------------------------------------------------------
*"*"區域介面：
*"  IMPORTING
*"     VALUE(P_SYST_DATE) TYPE  SY-DATUM
*"     VALUE(P_SYST_TIME) TYPE  SY-TIMLO DEFAULT '000000'
*"  EXPORTING
*"     VALUE(E_TIMESTAMPL) TYPE  TIMESTAMPL
*"  EXCEPTIONS
*"      CONVERT_HAPPEND_ERROR
*"----------------------------------------------------------------------

  TRY.
* Conversion: "Continuous System Time" in UTC Time
      cl_abap_tstmp=>systemtstmp_syst2utc( EXPORTING syst_date = p_syst_date
                                                     syst_time = p_syst_time
                                           IMPORTING utc_tstmp = DATA(l_tstmp) ).

* Converts time stamp from type TIMESTAMP(L) to type UTCLONG
      DATA(l_utclong) = cl_abap_tstmp=>tstmp2utclong( EXPORTING timestamp = l_tstmp ).

* Converts type UTCLONG to TIMESTAMPL (long time stamp)
      e_timestampl = cl_abap_tstmp=>utclong2tstmp( EXPORTING utclong = l_utclong ).

    CATCH cx_parameter_invalid_range cx_parameter_invalid_type cx_sy_conversion_no_date_time.
      RAISE convert_happend_error.
  ENDTRY.

ENDFUNCTION.
