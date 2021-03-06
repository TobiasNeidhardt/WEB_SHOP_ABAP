  CLASS zcl_order_overview_model DEFINITION
    PUBLIC
    FINAL
    CREATE PUBLIC.

    PUBLIC SECTION.

      TYPES: t_table  TYPE STANDARD TABLE OF zweb_order WITH DEFAULT KEY,
             tty_view TYPE STANDARD TABLE OF ZWEB_S_ORDER WITH DEFAULT KEY.
      DATA:  mt_order_view TYPE tty_view .
      CONSTANTS: lc_no_filter       TYPE string VALUE 'Alle',
                 lc_customer_number TYPE string VALUE 'Kundnnummer',
                 lc_order_number    TYPE string VALUE 'Bestellnummer',
                 lc_status          TYPE string VALUE 'Bestellstatus'.

      METHODS: constructor IMPORTING io_log TYPE REF TO zcl_webshop_log,
        delete_position IMPORTING is_position TYPE zweb_order
                        RAISING   zcx_webshop_exception_new,
        edit_postition  IMPORTING iv_wert     TYPE any
                                  is_position TYPE zweb_order
                        RAISING   zcx_webshop_exception_new,
        delete_order    IMPORTING iv_order_number TYPE zweb_order_number
                        RAISING   zcx_webshop_exception_new,
        get_orders      RETURNING VALUE(rt_orders) TYPE t_table,
        get_information IMPORTING VALUE(iv_filter)          TYPE i OPTIONAL
                                  VALUE(iv_customer_number) TYPE zweb_kd_numr_de OPTIONAL
                                  VALUE(iv_order_number)    TYPE zweb_order_number OPTIONAL
                                  VALUE(iv_status)          TYPE zweb_status OPTIONAL
                        RAISING   zcx_webshop_exception_new,
        select_with_condition  IMPORTING iv_condition             TYPE any OPTIONAL
                                         iv_condition_description TYPE string
                               RAISING   zcx_webshop_exception_new,
        get_positions   IMPORTING iv_order_number TYPE zweb_order_number,
        get_positions_output RETURNING VALUE(rt_positions) TYPE t_table,
        get_order_overview   RETURNING VALUE(rt_orders) TYPE tty_view
                             RAISING   zcx_webshop_exception_new,
        get_view RETURNING VALUE(rt_view) TYPE tty_view.

    PROTECTED SECTION.
    PRIVATE SECTION.
      DATA: mt_orders    TYPE t_table,
            mt_positions TYPE t_table,
            mo_log       TYPE REF TO zcl_webshop_log.
      METHODS: change_structur_from_value IMPORTING iv_wert     TYPE any
                             CHANGING  cs_position TYPE zweb_order
                             RAISING   zcx_webshop_exception_new,
        add_to_view   IMPORTING is_order       TYPE zweb_order
                                iv_total_price TYPE p
                      CHANGING  cs_view        TYPE ZWEB_S_ORDER,
        get_article_price_table RETURNING VALUE(rt_artikel_preis) TYPE ltt_artikel_price
                                RAISING   zcx_webshop_exception_new.
ENDCLASS.



CLASS ZCL_ORDER_OVERVIEW_MODEL IMPLEMENTATION.


    METHOD add_to_view.

    ENDMETHOD.


    METHOD change_structur_from_value.

      TRY.
          DATA(lv_column_type) = cl_abap_typedescr=>describe_by_data( iv_wert )->absolute_name.
          DATA(lv_rest_strlen) = strlen( lv_column_type ) - 6.
          DATA(lt_ddic) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( cs_position ) )->get_ddic_object( ).

          "Type richtig -> Komponente finden Typ: lv_nam+6(lv_strlen) -> Komponenten Feld-Symbol zuweisen / Komponente mithilfe des Feld-Symbols m??glich zu bearbeiten
          ASSIGN COMPONENT lt_ddic[ rollname = lv_column_type+6(lv_rest_strlen) ]-fieldname OF STRUCTURE cs_position TO FIELD-SYMBOL(<ls_position>).

          <ls_position> = iv_wert.

        CATCH cx_sy_itab_line_not_found.
          MESSAGE e035(z_web_shop) INTO DATA(lv_msg).
          me->mo_log->add_msg_from_sys( ).
          me->mo_log->safe_log( ).
          RAISE EXCEPTION TYPE zcx_webshop_exception_new USING MESSAGE.
      ENDTRY.

    ENDMETHOD.


    METHOD constructor.

      me->mo_log = io_log.

    ENDMETHOD.


    METHOD delete_order.

    ENDMETHOD.


    METHOD delete_position.

    ENDMETHOD.


    METHOD edit_postition.


    ENDMETHOD.


    METHOD get_article_price_table.


    ENDMETHOD.


    METHOD get_information.

      CLEAR mt_orders.
      TRY.
          IF iv_filter EQ 0.
            select_with_condition( iv_condition_description = lc_no_filter ).
          ELSEIF iv_filter EQ 1 AND iv_customer_number IS NOT INITIAL.
            select_with_condition( iv_condition_description = lc_customer_number iv_condition = iv_customer_number ).
          ELSEIF iv_filter EQ 2 AND iv_order_number IS NOT INITIAL.
            select_with_condition( iv_condition_description = lc_order_number iv_condition = iv_order_number ).
          ELSEIF iv_filter EQ 3 AND iv_status IS NOT INITIAL.
            select_with_condition( iv_condition_description = lc_status iv_condition = iv_status ).
          ELSE.

          ENDIF.
        CATCH zcx_webshop_exception_new.
          RAISE EXCEPTION TYPE zcx_webshop_exception_new.
      ENDTRY.

    ENDMETHOD.


    METHOD get_orders.

      rt_orders = me->mt_orders.

    ENDMETHOD.


    METHOD get_order_overview.

      DATA: lt_orders        TYPE TABLE OF zweb_order,
            ls_orders        TYPE          zweb_order,
            ls_zwischen      TYPE          zweb_order,
            lv_price_article TYPE          zweb_price,
            lv_total_price   TYPE          p,
            lt_view          TYPE TABLE OF ZWEB_S_ORDER,
            ls_view          TYPE          ZWEB_S_ORDER,
            lv_counter       TYPE          i.

      lt_orders = me->mt_orders.

      DATA(lt_article_price) = me->get_article_price_table(  ).

      LOOP AT lt_orders INTO ls_zwischen.
        lv_price_article = lt_article_price[ artikel_number = ls_zwischen-article ]-artikel_price.
        IF ls_orders-order_number = ls_zwischen-order_number.
          lv_total_price = lv_price_article * ls_zwischen-order_amount + lv_total_price.
          "Sobald eine neue Bestellung beginnt bisherige Bestellung in Tabelle
        ELSEIF ls_orders IS  NOT INITIAL AND ls_orders-order_number <> ls_zwischen-order_number.
          me->add_to_view( EXPORTING is_order       = ls_orders
                                     iv_total_price = lv_total_price
                           CHANGING  cs_view        = ls_view ).
          APPEND ls_view TO lt_view.
          CLEAR ls_view.
          lv_total_price = lv_price_article * ls_zwischen-order_amount.
        ELSE.
          CLEAR lv_total_price.
          lv_total_price = lv_price_article * ls_zwischen-order_amount.
        ENDIF.

        "Daten??berschreiben f??r Vergleich bei mehreren Loops ??ber Bestellung
        ls_orders = ls_zwischen.
        lv_counter = lv_counter + 1.
      ENDLOOP.

      me->add_to_view( EXPORTING is_order       = ls_orders
                                 iv_total_price = lv_total_price
                       CHANGING cs_view         = ls_view ).

      APPEND ls_view TO lt_view.
      CLEAR ls_view.
      mt_order_view = lt_view.

    ENDMETHOD.


    METHOD get_positions.

    ENDMETHOD.


    METHOD get_positions_output.

      rt_positions = me->mt_positions.

    ENDMETHOD.


    METHOD get_view.

      rt_view = me->mt_order_view.

    ENDMETHOD.


    METHOD select_with_condition.

    ENDMETHOD.
ENDCLASS.
