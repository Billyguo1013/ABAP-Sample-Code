*&---------------------------------------------------------------------*
*& INCLUDE          ZRPUR032_ACL1
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      BEGIN OF gcs_toolbar,
        expall_name     TYPE salv_de_function VALUE 'EXPALL',
        expall_icon     TYPE iconname VALUE icon_expand_all,
        colall_name     TYPE salv_de_function VALUE 'COLALL',
        colall_icon     TYPE iconname VALUE icon_collapse_all,
        expall_tooltip  TYPE string,
        colall_tooltip  TYPE string,
        expall_position TYPE salv_de_function_pos VALUE if_salv_c_function_position=>right_of_salv_functions,
        colall_position TYPE salv_de_function_pos VALUE if_salv_c_function_position=>right_of_salv_functions,
      END OF gcs_toolbar.

    CLASS-METHODS:
      class_constructor,

      get_icon
        IMPORTING
          iv_type        TYPE char1
        RETURNING
          VALUE(rv_icon) TYPE text40.

    METHODS:
      handle_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
          e_salv_function
          sender,
      handle_link_click     FOR EVENT link_click  OF cl_salv_events_table
        IMPORTING
          row
          column
          sender,
      handle_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
          row
          column
          sender.

ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.

  METHOD class_constructor.
    gcs_toolbar-expall_tooltip = 'Expand all Details'.
    gcs_toolbar-colall_tooltip = 'Collapse all Details'.
  ENDMETHOD.


  METHOD handle_added_function.
    DATA:
      lv_add_subrows_index TYPE i,
      lv_refresh_alv       TYPE abap_bool.

    DATA:
     lr_log TYPE REF TO zspur032_alv_testrun_a.

    DATA:
      lo_filters TYPE REF TO cl_salv_filters.
*
    DATA: l_z_err_fields    TYPE string,
          l_z_error_message TYPE string.
    DATA: t_err_fields    TYPE STANDARD TABLE OF z_err_fields_a,
          t_error_message TYPE STANDARD TABLE OF z_error_message_a.

    FIELD-SYMBOLS:
      <ls_alv_new> TYPE zspur032_alv_testrun_a.

    lv_refresh_alv = abap_false.

    TRY.
        lo_filters = go_salv_table->get_filters( ).
        CASE e_salv_function.
          WHEN gcs_toolbar-expall_name.
            LOOP AT gt_alv ASSIGNING <alv> WHERE expand(3) EQ icon_expand(3).

              DATA(l_row) = sy-tabix.
              <alv>-expand = lcl_handle_events=>get_icon( iv_type = 'C' ).

              READ TABLE gt_testrun INTO gs_testrun WITH KEY filename = <alv>-filename
                                                              dataseq = <alv>-dataseq.
              IF sy-subrc = 0.
                CHECK gs_testrun-trerror IS NOT INITIAL.
                l_z_error_message =  gs_testrun-trerror.
                SPLIT l_z_error_message AT `/` INTO TABLE t_error_message.

                DATA(l_lines) = lines( t_error_message ).

                LOOP AT t_error_message INTO DATA(s_error_message) FROM 1 TO l_lines.
                  lv_add_subrows_index = l_row + sy-tabix.
                  INSERT INITIAL LINE INTO gt_alv INDEX lv_add_subrows_index ASSIGNING <ls_alv_new>.
                  <ls_alv_new> = CORRESPONDING #( gs_testrun ).
                  CLEAR <ls_alv_new>-expand.
                  <ls_alv_new>-trerror = s_error_message.
                ENDLOOP.

              ENDIF.

            ENDLOOP.
            IF sy-subrc EQ 0.
              lv_refresh_alv = abap_true.
            ENDIF.
          WHEN gcs_toolbar-colall_name.
            LOOP AT gt_alv ASSIGNING <alv> WHERE expand(3) EQ icon_collapse(3).
              <alv>-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).
              DELETE gt_alv  WHERE filename EQ <alv>-filename
                               AND dataseq EQ <alv>-dataseq
                               AND expand IS INITIAL.
            ENDLOOP.
            IF sy-subrc EQ 0.
              lv_refresh_alv = abap_true.
            ENDIF.
        ENDCASE.
      CATCH cx_salv_not_found.
      CATCH cx_salv_data_error.
      CATCH cx_salv_existing.
    ENDTRY.

    IF lv_refresh_alv EQ abap_true.
      DATA(lr_columns) = go_salv_table->get_columns( ).
      lr_columns->set_optimize( abap_false ).
      lr_columns->set_optimize( abap_true ).
      go_salv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.

  METHOD get_icon.
    DATA:
      lv_name TYPE iconname,
      lv_info TYPE text40.

    CASE iv_type.
      WHEN 'E'.
        lv_name = icon_expand.
        lv_info = 'Expand Details'.
      WHEN 'C'.
        lv_name = icon_collapse.
        lv_info = 'Collapse Details'.
    ENDCASE.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = lv_name
        info                  = lv_info
        add_stdinf            = ' '
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 0
        outputfield_too_short = 0
        OTHERS                = 0.
  ENDMETHOD.

  METHOD handle_link_click.
    DATA:
      lv_add_subrows_index TYPE i,
      lv_refresh_alv       TYPE abap_bool.

    DATA:
      lr_log TYPE REF TO zspur032_alv_testrun_a.

    DATA: l_z_err_fields    TYPE string,
          l_z_error_message TYPE string.
    DATA: t_err_fields    TYPE STANDARD TABLE OF z_err_fields_a,
          t_error_message TYPE STANDARD TABLE OF z_error_message_a.

    FIELD-SYMBOLS:
      <ls_alv_new> TYPE zspur032_alv_testrun_a.

    lv_refresh_alv = abap_false.

    CASE column.
      WHEN 'EXPAND'.
        READ TABLE gt_alv ASSIGNING <alv> INDEX row.
        IF <alv>-expand(3) EQ icon_expand(3).

          <alv>-expand = lcl_handle_events=>get_icon( iv_type = 'C' ).

          READ TABLE gt_testrun INTO gs_testrun WITH KEY filename = <alv>-filename
                                                           dataseq = <alv>-dataseq.
          IF sy-subrc = 0.
            CHECK gs_testrun-trerror IS NOT INITIAL.
            l_z_error_message =  gs_testrun-trerror.
            SPLIT l_z_error_message AT `/` INTO TABLE t_error_message.

            DATA(l_lines) = lines( t_error_message ).

            LOOP AT t_error_message INTO DATA(s_error_message) FROM 1 TO l_lines.
              lv_add_subrows_index = row.
              lv_add_subrows_index = lv_add_subrows_index + sy-tabix.
              INSERT INITIAL LINE INTO gt_alv INDEX lv_add_subrows_index ASSIGNING <ls_alv_new>.
              <ls_alv_new> = CORRESPONDING #( gs_testrun ).
              CLEAR <ls_alv_new>-expand.
              <ls_alv_new>-trerror = s_error_message.
            ENDLOOP.

          ENDIF.

        ELSE.
          <alv>-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).
          DELETE gt_alv  WHERE filename  EQ <alv>-filename
                           AND dataseq EQ <alv>-dataseq
                           AND expand IS INITIAL.
        ENDIF.

        IF lv_refresh_alv = abap_true.
          DATA(lr_columns) = go_salv_table->get_columns( ).
          lr_columns->set_optimize( abap_false ).
          lr_columns->set_optimize( abap_true ).
          go_salv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
        ENDIF.

      WHEN 'DATASEQ'.

        PERFORM refresh_alv_2 USING row.

    ENDCASE.

  ENDMETHOD.

  METHOD handle_double_click.

    PERFORM refresh_alv_2 USING row.

  ENDMETHOD.

ENDCLASS.
