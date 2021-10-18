CLASS zcl_homescreen_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      mt_order                 TYPE zweb_tt_order,
      mt_articles              TYPE TABLE OF zweb_article WITH KEY mandt article_number,
      mt_articles_string_table TYPE TABLE OF string,
      mt_searched_articles     TYPE match_result_tab,
      mt_cart_order            TYPE TABLE OF zweb_order,
      mt_articles_out          TYPE TABLE OF zweb_article,
      mt_cart                  TYPE TABLE OF zweb_s_cart,
      mo_log                   TYPE REF TO zcl_webshop_log.

    METHODS: search_entries_to_new_table
      IMPORTING
        !iv_search_string TYPE string ,
      order_cart
        RAISING zcx_webshop_exception_new,
      remove_item_from_cart
        IMPORTING
          !iv_article_number TYPE zweb_article_number ,
      constructor
        IMPORTING
          !io_home_screen_controller TYPE REF TO zcl_homescreen_controller
          !iv_customer_number        TYPE zweb_customer_number
          !io_log                    TYPE REF TO zcl_webshop_log,
      add_to_cart
        IMPORTING
                  !iv_number_of_articles TYPE zweb_order_amount
                  !is_article            TYPE zweb_article
        RAISING   zcx_webshop_exception_new,
      get_address_of_customer
        RAISING zcx_webshop_exception_new,
      set_order_address
        IMPORTING
          !is_order_address TYPE zweb_s_adress ,
      return_address
        RETURNING
          VALUE(rv_address) TYPE zweb_s_adress ,
      get_email_address_of_customer
        RETURNING
          VALUE(rv_email) TYPE zweb_email ,
      set_customer_email
        IMPORTING
          !iv_email TYPE zweb_email ,
      get_all_orders_from_customer
        RAISING zcx_webshop_exception_new ,
      get_open_order_from_mt_order
        RETURNING
          VALUE(rt_openorder) TYPE zweb_tt_order ,
      get_done_order_from_mt_order
        RETURNING
          VALUE(rt_done_order) TYPE zweb_tt_order ,
      get_order
        IMPORTING
          !iv_order_number TYPE tv_nodekey
        RETURNING
          VALUE(rt_order)  TYPE zweb_tt_order ,
      delete_position
        IMPORTING
                  !is_position TYPE zweb_order
        RAISING   zcx_webshop_exception_new ,
      edit_quantity_of_position
        IMPORTING
                  !is_position TYPE zweb_order
                  !iv_quantity TYPE int4
        RAISING   zcx_webshop_exception_new,
      add_up_item
        IMPORTING
          !iv_article_number TYPE zweb_article_number
          !iv_quantity       TYPE zweb_order_amount
        RETURNING
          VALUE(rv_quantity) TYPE zweb_order_amount,
      check_if_order_exist
        IMPORTING is_selected_row TYPE zweb_order
        RETURNING VALUE(rv_exist) TYPE boolean.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS:
      mc_status_inactive    TYPE zweb_status VALUE 'IN' ##NO_TEXT,  "Bestellung Inaktive
      mc_status_completed   TYPE zweb_status VALUE 'AB' ##NO_TEXT,   "Bestellung Abgeschlossen
      mc_status_in_progress TYPE zweb_status VALUE 'IB' ##NO_TEXT,   "Bestellung in Bearbeitung
      mc_status_ordered     TYPE zweb_status VALUE 'BE' ##NO_TEXT,   "Bestellung Bestellt
      mc_range_nr           TYPE nrnr VALUE '01' ##NO_TEXT.

    DATA:
      mo_home_screen_controller TYPE REF TO zcl_homescreen_controller,
      mv_customernumber         TYPE zweb_customer_number,
      ms_order_address          TYPE zweb_s_adress,
      mv_customer_email         TYPE zweb_email.

    METHODS: select_articles ,
      get_ordernumber
        RETURNING
          VALUE(rv_order_number) TYPE numc10 ,
      insert_address_of_order
        IMPORTING
          !iv_order_number TYPE numc10 ,
      select_articles_as_char_fields
        RAISING zcx_webshop_exception_new,
      find_occurences_of_regex
        IMPORTING
          !iv_search_string TYPE string ,
      add_to_cart_order_table
        IMPORTING
          !iv_order_number TYPE numc10.
ENDCLASS.



CLASS ZCL_HOMESCREEN_MODEL IMPLEMENTATION.


  METHOD add_to_cart.

  ENDMETHOD.


  METHOD add_to_cart_order_table.
    CLEAR mt_cart_order.
    DATA: ls_order TYPE zweb_order.
    LOOP AT me->mt_cart ASSIGNING FIELD-SYMBOL(<ls_cart>).

      ls_order = VALUE #( article         = <ls_cart>-article_number
                          order_amount    = <ls_cart>-number_of_articles
                          order_number   = iv_order_number
                          unit   = <ls_cart>-unit
                          position_number = sy-tabix
                          status          = me->mc_status_ordered
                          customer_number           = me->mv_customernumber ).

      APPEND ls_order TO mt_cart_order.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_up_item.

    rv_quantity = iv_quantity.
    "sum new quantity, delete current entry and add item with new quantity
    LOOP AT me->mt_cart ASSIGNING FIELD-SYMBOL(<ls_item>) WHERE article_number = iv_article_number.
      rv_quantity = rv_quantity + <ls_item>-number_of_articles.
      me->remove_item_from_cart( iv_article_number = iv_article_number ).
    ENDLOOP.

  ENDMETHOD.


  METHOD check_if_order_exist.

  ENDMETHOD.


  METHOD constructor.

    me->mo_home_screen_controller = io_home_screen_controller.
    me->mv_customernumber         = iv_customer_number.
    me->mo_log = io_log.
    me->select_articles( ).

  ENDMETHOD.


  METHOD delete_position.

  ENDMETHOD.


  METHOD edit_quantity_of_position.

  ENDMETHOD.


  METHOD find_occurences_of_regex.
    CLEAR mt_searched_articles.
    "search entries
    FIND ALL OCCURRENCES OF REGEX iv_search_string
    IN TABLE me->mt_articles_string_table
    IGNORING CASE
    RESULTS mt_searched_articles.
  ENDMETHOD.


  METHOD get_address_of_customer.


  ENDMETHOD.


  METHOD get_all_orders_from_customer.

  ENDMETHOD.


  METHOD get_done_order_from_mt_order.

  ENDMETHOD.


  METHOD get_email_address_of_customer.

    rv_email = me->mv_customer_email.

  ENDMETHOD.


  METHOD get_open_order_from_mt_order.

  ENDMETHOD.


  METHOD get_order.

    LOOP AT me->mt_order ASSIGNING FIELD-SYMBOL(<ls_position>) WHERE order_number = iv_order_number.

      APPEND <ls_position> TO rt_order.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_ordernumber.

    DATA: lv_ordernumber TYPE n LENGTH 10.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = me->mc_range_nr
        object                  = 'ZSBT_BEST'
      IMPORTING
        number                  = lv_ordernumber
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
      MESSAGE i046(z_web_shop) WITH sy-subrc.
    ENDIF.

    rv_order_number = lv_ordernumber.

  ENDMETHOD.


  METHOD insert_address_of_order.

  ENDMETHOD.


  METHOD order_cart.

    DATA(lv_order_number) = me->get_ordernumber( ).

    me->add_to_cart_order_table( iv_order_number = lv_order_number ).

    me->insert_address_of_order( iv_order_number = lv_order_number ).
    INSERT zweb_order FROM TABLE mt_cart_order.

    IF sy-subrc <> 0.
      MESSAGE i045(z_web_shop) INTO DATA(ls_msg).
      me->mo_log->add_msg_from_sys( ).
      me->mo_log->safe_log( ).
      RAISE EXCEPTION TYPE zcx_webshop_exception_new USING MESSAGE.
    ENDIF.

    me->mo_home_screen_controller->send_order_confirmation( iv_email = me->get_email_address_of_customer( )
                                                            iv_order_number = lv_order_number ).

    CLEAR me->mt_cart.

  ENDMETHOD.


  METHOD remove_item_from_cart.

  ENDMETHOD.


  METHOD return_address.

    rv_address = me->ms_order_address.

  ENDMETHOD.


  METHOD search_entries_to_new_table.

    TRY.
        IF iv_search_string IS NOT INITIAL.
          CLEAR me->mt_articles_out.
          me->select_articles_as_char_fields(  ).
          me->find_occurences_of_regex( iv_search_string = iv_search_string  ).
          "build new output table of searched entries
          LOOP AT mt_searched_articles ASSIGNING FIELD-SYMBOL(<ls_searched_entries>).

            APPEND me->mt_articles[ <ls_searched_entries>-line ] TO me->mt_articles_out.

          ENDLOOP.
        ELSE.
          me->mt_articles_out = me->mt_articles.
        ENDIF.

      CATCH zcx_webshop_exception_new INTO DATA(lo_exc).
        MESSAGE lo_exc.
        me->mt_articles_out = me->mt_articles.
    ENDTRY.

  ENDMETHOD.


  METHOD select_articles.

  ENDMETHOD.


  METHOD select_articles_as_char_fields.
    SELECT designation
           FROM @me->mt_articles AS articles
           INTO TABLE @mt_articles_string_table.
    IF sy-subrc <> 0.
      MESSAGE i058(z_web_shop) INTO DATA(ls_msg).
      me->mo_log->add_msg_from_sys( ).
      me->mo_log->safe_log( ).
      RAISE EXCEPTION TYPE zcx_webshop_exception_new USING MESSAGE.
    ENDIF.
  ENDMETHOD.


  METHOD set_customer_email.

    me->mv_customer_email = iv_email.

  ENDMETHOD.


  METHOD set_order_address.

    CLEAR me->ms_order_address.

    me->ms_order_address = is_order_address.

  ENDMETHOD.
ENDCLASS.
