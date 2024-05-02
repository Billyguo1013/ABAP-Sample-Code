FUNCTION z_mm_eta_cal_a.
*"----------------------------------------------------------------------
*"*"區域介面：
*"  IMPORTING
*"     VALUE(P_ERDAT) TYPE  ERDAT
*"     VALUE(P_ERZET) TYPE  ERZET
*"     VALUE(P_DURAT) TYPE  /SAPAPO/TR_DURAT
*"  EXPORTING
*"     VALUE(ETA_DATE_C) TYPE  CHAR10
*"     VALUE(ETA_TIME_C) TYPE  CHAR05
*"----------------------------------------------------------------------


  DATA: hhhmm TYPE string,
        days  TYPE i.

  DATA: calc_date LIKE  p0001-begda.

  CALL FUNCTION 'CONVERSION_EXIT_TSTRN_OUTPUT'
    EXPORTING
      input  = p_durat
    IMPORTING
      output = hhhmm.

  SPLIT hhhmm AT ':' INTO DATA(hhh) DATA(mm).
  days = hhh DIV 24.
  hhh = hhh - ( days * 24 ).
  CONDENSE hhh.
  SHIFT hhh RIGHT DELETING TRAILING '0'.
  SHIFT hhh RIGHT DELETING TRAILING '.'.
  CONDENSE hhh.
  hhhmm = hhh && ':' && mm.

* day
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = p_erdat
      days      = CONV dlydy( days )
      months    = 0
      years     = 0
    IMPORTING
      calc_date = calc_date.

* time
  DATA: hhmmss(8) TYPE c.
  WRITE p_erzet TO hhmmss USING EDIT MASK '__:__:__'.
  SPLIT hhmmss AT ':' INTO DATA(hh2) DATA(mm2) DATA(ss2).
  hhh = hhh + hh2.
  mm = mm + mm2.
  IF ss2 <> 0.  "秒數無條件進位
    mm = mm + 1.
  ENDIF.

  IF mm >= 60.
    DATA add_h TYPE i.

    add_h = mm DIV 60.
    mm = mm MOD 60.
    hhh = hhh + add_h.
  ENDIF.

  IF hhh >= 24.
    DATA add_d TYPE i.

    add_d = hhh DIV 24.
    hhh = hhh MOD 24.
  ENDIF.

*--------------------------------------------------------------------*
* Final
*--------------------------------------------------------------------*
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = calc_date
      days      = CONV dlydy( add_d )
      months    = 0
      years     = 0
    IMPORTING
      calc_date = calc_date.

  CONDENSE hhh.
  DATA: hours(2) TYPE c.
  hours = hhh.
  hours = |{ hours ALPHA = IN }|.

* Output
  WRITE calc_date TO eta_date_c DD/MM/YYYY.
  eta_time_c = |{ hours }:{ mm }|.

ENDFUNCTION.
