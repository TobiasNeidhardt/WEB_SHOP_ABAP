*"* use this source file for your ABAP unit test classes
CLASS ltc_homescreen_model_orders DEFINITION DEFERRED.
CLASS zcl_homescreen_model DEFINITION LOCAL FRIENDS ltc_homescreen_model_orders.
CLASS ltc_homescreen_model_orders DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_cut                 TYPE REF TO zcl_homescreen_model,
          lt_order_data         TYPE TABLE OF zweb_order,
          l_exception_occured   TYPE abap_bool,
          homescreen_controller TYPE REF TO zcl_homescreen_controller,
          login_controller      TYPE REF TO zcl_customer_login_controller.

    CONSTANTS: lc_customer_number    TYPE zweb_customer_number VALUE '1',
               lc_order_number_1     TYPE zweb_order_number VALUE  '1',
               lc_order_number_2     TYPE zweb_order_number VALUE  '2',
               lc_position_number_1  TYPE zweb_position_number VALUE '1',
               lc_position_number_2  TYPE zweb_position_number VALUE '2',
               lc_order_amount_1     TYPE zweb_order_amount VALUE 01,
               lc_order_amount_2     TYPE zweb_order_amount VALUE 02,
               lc_order_status       TYPE zweb_status VALUE 'BE',
               lc_order_value        TYPE zweb_order_amount VALUE '100',
               lc_status_in_progress TYPE zweb_status VALUE 'IB',
               lc_status_completet   TYPE zweb_status VALUE 'AB'.


    CLASS-DATA: m_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    METHODS setup.

    METHODS:
      "!Test method for add_to_cart_method
      check_add_to_cart FOR TESTING.

    METHODS:
      "!Test methods for delete_position_method
      check_delete_position FOR TESTING,
      delete_position_exc_exist FOR TESTING,
      delete_pos_status_exc_exist FOR TESTING.

    METHODS:
      "!Test methods for edit_quantity_of_position method
      check_edit_quantity_of_pos FOR TESTING,
      edit_quantity_of_pos_exc_exist FOR TESTING.

    METHODS:
      "! Test methods for get_all_orders_from_customer method
      check_all_orders_from_customer FOR TESTING,
      all_orders_customer_exc_exist FOR TESTING.

    METHODS:
      "!Test method for get_all_orders_from_customer method
      check_get_done_order FOR TESTING.

    METHODS:
      "!Test method for get_all_orders_from_customer method
      check_get_open_order FOR TESTING.

    METHODS:
      "! Test methods for check_if_order_exist method
      check_if_order_exist_true FOR TESTING,
      check_if_order_exist_false FOR TESTING.


ENDCLASS.

CLASS ltc_homescreen_model_orders IMPLEMENTATION.

  METHOD class_setup.
    m_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'zweb_order' ) ) ).
  ENDMETHOD.

  METHOD setup.

    "given
    lt_order_data = VALUE #( ( order_number = lc_order_number_1 position_number = lc_position_number_1 order_amount = lc_order_amount_1 status = lc_order_status customer_number = lc_customer_number )
                             ( order_number = lc_order_number_1 position_number = lc_position_number_2 order_amount = lc_order_amount_1 status = lc_order_status customer_number = lc_customer_number )
                             ( order_number = lc_order_number_2 position_number = lc_position_number_1 order_amount = lc_order_amount_1 status = lc_status_completet customer_number = lc_customer_number ) ).
    login_controller = NEW zcl_customer_login_controller(  ).
    homescreen_controller = NEW zcl_homescreen_controller( io_login_controller = login_controller iv_customer_number =  lc_customer_number iv_email =  'test@web.de').

    m_environment->clear_doubles( ).

    m_cut = NEW zcl_homescreen_model( io_home_screen_controller = homescreen_controller iv_customer_number = lc_customer_number ).

    m_environment->insert_test_data( EXPORTING i_data = lt_order_data ).

    l_exception_occured = abap_false.

  ENDMETHOD.

  METHOD check_add_to_cart.
    "given
    DATA: lv_article            TYPE zweb_article,
          lv_number_of_articles TYPE zweb_order_amount.

    lv_article = VALUE #( article_number = 1 designation = 'Bildschirm' ).
    lv_number_of_articles = 2.

    "when
    TRY.
        m_cut->add_to_cart( is_article = lv_article iv_number_of_articles = lv_number_of_articles ).

        "then
        cl_abap_unit_assert=>assert_equals( EXPORTING act =  m_cut->mt_cart[ 1 ]-article_designation
                                                     exp =  lv_article-designation ).
      CATCH zcx_webshop_exception_new INTO DATA(e_text).
        MESSAGE e_text->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD check_delete_position.
    "when
    TRY.
        m_cut->delete_position( is_position = VALUE #( order_number = lc_order_number_1
                                                       position_number = lc_position_number_1
                                                       status = lc_order_status ) ).
      CATCH zcx_webshop_exception_new .
        cl_abap_unit_assert=>fail( EXPORTING msg = 'Beim löschen ist ein Fehler aufgetreten' ).
    ENDTRY.
    "then
    SELECT *
    FROM zweb_order
    INTO TABLE @DATA(lt_data)
    WHERE order_number = @lc_order_number_1.


    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( EXPORTING msg = 'Es wurden zu viele Datensätze gelöscht' ).
    ELSE.

      IF line_exists( lt_data[ position_number = 1 ] ).
        cl_abap_unit_assert=>fail( EXPORTING msg = 'Position wurde nicht gelöscht'  ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD delete_position_exc_exist.
    TRY.
        "given
        m_environment->clear_doubles( ).

        m_cut->delete_position( is_position = VALUE #( order_number = lc_order_number_1
                                                   position_number = lc_position_number_1
                                                   status = lc_order_status ) ).
      CATCH zcx_webshop_exception_new.
        l_exception_occured = abap_true.
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_true(
      act = l_exception_occured
      msg = 'In der Methode delete_position wird keine Exception geworfen, falls ein Fehler auftritt!'
    ).
  ENDMETHOD.

  METHOD delete_pos_status_exc_exist.
    TRY.
        "when
        m_cut->delete_position( is_position = VALUE #( order_number = lc_order_number_1
                                                   position_number = lc_position_number_1
                                                   status = lc_status_in_progress ) ).

      CATCH zcx_webshop_exception_new.
        l_exception_occured = abap_true.
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_true(
      act = l_exception_occured
      msg = 'In der Methode delete_position wird keine Exception geworfen, der Status AB oder IB ist!'
    ).
  ENDMETHOD.

  METHOD check_edit_quantity_of_pos.
    TRY.
        "when
        m_cut->edit_quantity_of_position( is_position = VALUE #( order_number = lc_order_number_1
                                                       position_number = lc_position_number_1
                                                       status = lc_order_status ) iv_quantity = 5 ).
      CATCH zcx_webshop_exception_new .
        cl_abap_unit_assert=>fail( EXPORTING msg = 'Beim anpassen der Menge ist ein Fehler aufgetreten' ).
    ENDTRY.
    "then
    SELECT *
    FROM zweb_order
    INTO TABLE @DATA(lt_data)
    WHERE order_amount = 5 .

    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( EXPORTING msg = 'Bestellmenge wurde nicht geändert!'  ).
    ENDIF.

  ENDMETHOD.

  METHOD edit_quantity_of_pos_exc_exist.

    TRY.
        "given
        m_environment->clear_doubles( ).
        "when
        m_cut->edit_quantity_of_position( is_position = VALUE #( order_number = lc_order_number_1
                                                       position_number = lc_position_number_1
                                                       status = lc_order_status ) iv_quantity = 5 ).
      CATCH zcx_webshop_exception_new.
        l_exception_occured = abap_true.
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_true(
      act = l_exception_occured
      msg = 'In der Methode edit_quantity_of_position wird keine Exception geworfen, falls ein Fehler auftritt!'
    ).
  ENDMETHOD.



  METHOD check_all_orders_from_customer.
    DATA: mt_orders TYPE zweb_tt_order.
    TRY.
        "when
        m_cut->get_all_orders_from_customer(  ).
        mt_orders = m_cut->mt_order.
      CATCH zcx_webshop_exception_new .
        cl_abap_unit_assert=>fail( EXPORTING msg = 'Beim Aufruf der Methode get_all_orders_from_customer ist ein Fehler aufgetreten' ).
    ENDTRY.
    "then
    IF m_cut->mt_order IS INITIAL.
      cl_abap_unit_assert=>fail( EXPORTING msg = 'In der Methode get_all_orders_from_customer wurden keine Daten gelesen!').
    ELSE.
      "In Tabelle zweb_order werden einzelne Bestellpositionen auch als einzelne  Bestellung gespeichert
      cl_abap_unit_assert=>assert_equals( EXPORTING act =  mt_orders[ 1 ]-order_number
                                                    exp =  lc_order_number_1 ).
      cl_abap_unit_assert=>assert_equals( EXPORTING act =  mt_orders[ 2 ]-order_number
                                                    exp =  lc_order_number_1 ).
      cl_abap_unit_assert=>assert_equals( EXPORTING act =  mt_orders[ 3 ]-order_number
                                                    exp =  lc_order_number_2 ).
    ENDIF.
  ENDMETHOD.

  METHOD all_orders_customer_exc_exist.
    DATA: mt_orders TYPE zweb_tt_order.

    TRY.
        "given
        m_environment->clear_doubles( ).
        "when
        m_cut->get_all_orders_from_customer( ).
        mt_orders = m_cut->mt_order.
        "then
      CATCH zcx_webshop_exception_new.
        l_exception_occured = abap_true.
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_true(
      act = l_exception_occured
      msg = 'In der Methode get_all_orders_from_customer wird keine Exception geworfen, falls ein Fehler auftritt!'
    ).
  ENDMETHOD.

  METHOD check_get_done_order.
    DATA: lv_done_orders TYPE zweb_tt_order.
    TRY.
        "when
        m_cut->get_all_orders_from_customer(  ).
      CATCH zcx_webshop_exception_new .
        cl_abap_unit_assert=>fail( EXPORTING msg = 'Beim Aufruf der Methode get_all_orders_from_customer ist ein Fehler aufgetreten' ).
    ENDTRY.
    IF m_cut->mt_order IS INITIAL.
      cl_abap_unit_assert=>fail( EXPORTING msg = 'In der Methode get_all_orders_from_customer wurden keine Daten gelesen. Erst wenn die Methode get_all_orders_from_customer funktioniert, kann dieser Test positiv werden.').
    ELSE.
      lv_done_orders = m_cut->get_done_order_from_mt_order(  ).

      "then
      cl_abap_unit_assert=>assert_equals( EXPORTING act =  lv_done_orders[ 1 ]-order_number
                                                    exp =  lt_order_data[ 3 ]-order_number ).
    ENDIF.
  ENDMETHOD.

  METHOD check_get_open_order.
    DATA: lv_done_orders TYPE zweb_tt_order.
    TRY.
        "when
        m_cut->get_all_orders_from_customer(  ).
      CATCH zcx_webshop_exception_new .
        cl_abap_unit_assert=>fail( EXPORTING msg = 'In der Methode get_all_orders_from_customer wurden keine Daten gelesen. Erst wenn die Methode get_all_orders_from_customer funktioniert, kann dieser Test positiv werden.' ).
    ENDTRY.
    IF m_cut->mt_order IS INITIAL.
      cl_abap_unit_assert=>fail( EXPORTING msg = 'In der Methode get_all_orders_from_customer wurden keine Daten gelesen. Erst wenn die Methode get_all_orders_from_customer funktioniert, kann dieser Test positiv werden.').
    ELSE.
      lv_done_orders = m_cut->get_open_order_from_mt_order(  ).

      "then
      cl_abap_unit_assert=>assert_equals( EXPORTING act =  lv_done_orders[ 1 ]-order_number
                                                      exp =  lt_order_data[ 1 ]-order_number ).
    ENDIF.
  ENDMETHOD.

  METHOD check_if_order_exist_true.
    DATA: lv_exist          TYPE boolean,
          ls_order_to_check TYPE zweb_order.
    "given
    m_cut->mo_home_screen_controller->mt_order_to_show = VALUE #( ( order_number = 1 article = 'Bildschirm' )
                                                                  ( order_number = 2 article = 'Bildschirm' ) ).

    ls_order_to_check = VALUE #( order_number = 1 article = 'Bildschirm'  ).

    "when
    lv_exist = m_cut->check_if_order_exist( is_selected_row = ls_order_to_check ).

    "then
    cl_abap_unit_assert=>assert_true(
     act = lv_exist
     msg = 'Die Methode check_if_order_exist funktioniert nicht richitg. Bestellnummer 1 existiert!'
   ).

  ENDMETHOD.

  METHOD check_if_order_exist_false.
    DATA: lv_exist          TYPE boolean,
          ls_order_to_check TYPE zweb_order.
    "given
    m_cut->mo_home_screen_controller->mt_order_to_show = VALUE #( ( order_number = 1 article = 'Bildschirm' )
                                                                  ( order_number = 2 article = 'Bildschirm' ) ).

    ls_order_to_check = VALUE #( order_number = 3 article = 'Bildschirm' ).

    "when
    lv_exist = m_cut->check_if_order_exist( is_selected_row = ls_order_to_check ).

    "then
    cl_abap_unit_assert=>assert_false(
     act = lv_exist
     msg = 'Die Methode check_if_order_exist funktioniert nicht richitg. Bestellnummer 3 existiert nicht!'
   ).

  ENDMETHOD.

ENDCLASS.





CLASS ltc_homescreen_model_artikel DEFINITION DEFERRED.
CLASS zcl_homescreen_model DEFINITION LOCAL FRIENDS ltc_homescreen_model_artikel.
CLASS ltc_homescreen_model_artikel DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_cut                 TYPE REF TO zcl_homescreen_model,
          lt_article_data       TYPE TABLE OF zweb_article,
          l_exception_occured   TYPE abap_bool,
          homescreen_controller TYPE REF TO zcl_homescreen_controller,
          login_controller      TYPE REF TO zcl_customer_login_controller.


    CLASS-DATA: m_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    METHODS setup.
    METHODS check_select_articles FOR TESTING.
    METHODS check_remove_item_from_card FOR TESTING.

    CONSTANTS: lc_customer         TYPE zweb_customer_number VALUE '1',
               lc_article_number_1 TYPE zweb_article_number VALUE '1',
               lc_article_number_2 TYPE zweb_article_number VALUE '2',
               lc_article_number_3 TYPE zweb_article_number VALUE '3',
               lc_description_1    TYPE zweb_description VALUE 'Bildschirm',
               lc_description_2    TYPE zweb_description VALUE 'Tastatur',
               lc_description_3    TYPE zweb_description VALUE 'Maus',
               lc_currency         TYPE zweb_currency VALUE 'Euro'.



ENDCLASS.

CLASS ltc_homescreen_model_artikel IMPLEMENTATION.

  METHOD setup.
    "given
    lt_article_data = VALUE #( ( article_number = lc_article_number_1 designation = lc_description_1 price = 200 currency = lc_currency )
                               ( article_number = lc_article_number_2 designation = lc_description_2 price = 70 currency = lc_currency )
                               ( article_number = lc_article_number_3 designation = lc_description_3 price = 30 currency = lc_currency ) ).

    login_controller = NEW zcl_customer_login_controller(  ).
    homescreen_controller = NEW zcl_homescreen_controller( io_login_controller = login_controller iv_customer_number =  lc_customer iv_email =  'test@web.de').

    m_environment->clear_doubles( ).

    m_cut = NEW zcl_homescreen_model( io_home_screen_controller = homescreen_controller iv_customer_number = lc_customer ).
    m_environment->insert_test_data( EXPORTING i_data = lt_article_data ).
  ENDMETHOD.

  METHOD class_setup.
    m_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'zweb_article' ) ) ).
  ENDMETHOD.

  METHOD check_select_articles.
    DATA: lt_articles TYPE TABLE OF zweb_article.

    "when
    m_cut->select_articles(  ).
    .
    lt_articles = m_cut->mt_articles.

    IF lt_articles IS INITIAL.
      cl_abap_unit_assert=>fail( EXPORTING msg = 'In der Methode select_articles wurde nichts ausgelesen!').
    ELSE.

      "then
      cl_abap_unit_assert=>assert_equals( EXPORTING act =  lt_articles[ 1 ]-article_number
                                                    exp =  lt_article_data[ 1 ]-article_number ).
      cl_abap_unit_assert=>assert_equals( EXPORTING act =  lt_articles[ 2 ]-article_number
                                                    exp =  lt_article_data[ 2 ]-article_number ).
      cl_abap_unit_assert=>assert_equals( EXPORTING act =  lt_articles[ 3 ]-article_number
                                                exp =  lt_article_data[ 3 ]-article_number ).
    ENDIF.
  ENDMETHOD.

  METHOD check_remove_item_from_card.
    "given
    m_cut->mt_cart  = VALUE #( ( article_number = lc_article_number_1 article_description = lc_description_1  price = 200 currency = lc_currency )
                            ( article_number = lc_article_number_2 article_description = lc_description_2  price = 70 currency = lc_currency )
                            ( article_number = lc_article_number_3 article_description = lc_description_3  price = 30 currency = lc_currency )    ).

    "when
    m_cut->remove_item_from_cart( iv_article_number = lc_article_number_2 ).

    "then
    cl_abap_unit_assert=>assert_equals( EXPORTING act =  m_cut->mt_cart[ 1 ]-article_number
                                                exp = lc_article_number_1 ).
    cl_abap_unit_assert=>assert_equals( EXPORTING act =  m_cut->mt_cart[ 2 ]-article_number
                                                  exp =  lc_article_number_3 ).

  ENDMETHOD.
ENDCLASS.







CLASS ltc_homescreen_model_adress DEFINITION DEFERRED.
CLASS zcl_homescreen_model DEFINITION LOCAL FRIENDS ltc_homescreen_model_adress.
CLASS ltc_homescreen_model_adress DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_cut                 TYPE REF TO zcl_homescreen_model,
          lt_adress_data        TYPE TABLE OF zweb_order_ad,
          l_exception_occured   TYPE abap_bool,
          homescreen_controller TYPE REF TO zcl_homescreen_controller,
          login_controller      TYPE REF TO zcl_customer_login_controller.

    CONSTANTS: lc_customer_number TYPE zweb_customer_number VALUE '1',
               lc_order_number_1  TYPE numc10 VALUE 1,
               lc_order_number_2  TYPE numc10 VALUE 2,
               lc_street_1        TYPE zweb_street VALUE 'Schlossallee',
               lc_street_2        TYPE zweb_street VALUE 'Parkstrasse',
               lc_zip_code        TYPE zweb_postalcode VALUE 97070,
               lc_city            TYPE zweb_city VALUE 'Würzburg'.



    CLASS-DATA: m_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    METHODS setup.
    METHODS check_insert_address_of_order FOR TESTING.





ENDCLASS.

CLASS ltc_homescreen_model_adress IMPLEMENTATION.

  METHOD setup.
    "given
    lt_adress_data = VALUE #( ( order_number = lc_order_number_1 street = lc_street_1 house_number = 1 zip_code = lc_zip_code city = lc_city ) ).

    login_controller = NEW zcl_customer_login_controller(  ).
    homescreen_controller = NEW zcl_homescreen_controller( io_login_controller = login_controller iv_customer_number =  lc_customer_number iv_email =  'test@web.de').

    m_environment->clear_doubles( ).

    m_cut = NEW zcl_homescreen_model( io_home_screen_controller = homescreen_controller iv_customer_number = lc_customer_number ).
    m_environment->insert_test_data( EXPORTING i_data = lt_adress_data ).
  ENDMETHOD.

  METHOD class_setup.
    m_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'zweb_order_ad' ) ) ).
  ENDMETHOD.

  METHOD check_insert_address_of_order.
    DATA: lt_adress TYPE TABLE OF zweb_order_ad.

    "given
    m_cut->ms_order_address = VALUE #( street = lc_street_2 houese_number = 2 zip_code = lc_zip_code city = lc_city ).

    "when
    m_cut->insert_address_of_order( iv_order_number = lc_order_number_2 ).

    "then
    SELECT SINGLE street
    FROM zweb_order_ad INTO @DATA(ls_adress)
    WHERE street = @lc_street_2.

    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( EXPORTING msg = 'Adresse wurde nicht eingefügt').
    ENDIF.



  ENDMETHOD.


ENDCLASS.





CLASS ltc_homescreen_model_customer DEFINITION DEFERRED.
CLASS zcl_homescreen_model DEFINITION LOCAL FRIENDS ltc_homescreen_model_customer.
CLASS ltc_homescreen_model_customer DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_cut                 TYPE REF TO zcl_homescreen_model,
          lt_adress_data        TYPE TABLE OF zweb_customer,
          l_exception_occured   TYPE abap_bool,
          homescreen_controller TYPE REF TO zcl_homescreen_controller,
          login_controller      TYPE REF TO zcl_customer_login_controller.

    CONSTANTS: lc_customer_number_1 TYPE zweb_customer_number VALUE '1',
               lc_customer_number_2 TYPE zweb_customer_number VALUE '2',
               lc_name_1            TYPE zweb_name VALUE 'Meyer',
               lc_name_2            TYPE zweb_name VALUE 'Müller',
               lc_street_1          TYPE zweb_street VALUE 'Schlossallee',
               lc_street_2          TYPE zweb_street VALUE 'Parkstrasse',
               lc_house_number      TYPE zweb_house_nr VALUE 1,
               lc_zip_code          TYPE zweb_postalcode VALUE 97070,
               lc_city              TYPE zweb_city VALUE 'Würzburg'.



    CLASS-DATA: m_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    METHODS setup.
    METHODS check_get_adress_of_customer FOR TESTING.
    METHODS get_adress_of_custom_exc_exist FOR TESTING.





ENDCLASS.

CLASS ltc_homescreen_model_customer IMPLEMENTATION.

  METHOD setup.
    "given
    lt_adress_data = VALUE #( ( customer_number = lc_customer_number_1 name = lc_name_1 street = lc_street_1 house_number = lc_house_number zip_code = lc_zip_code city = lc_city )
                              ( customer_number = lc_customer_number_2 name = lc_name_2 street = lc_street_2 house_number = lc_house_number zip_code = lc_zip_code city = lc_city ) ).

    login_controller = NEW zcl_customer_login_controller(  ).
    homescreen_controller = NEW zcl_homescreen_controller( io_login_controller = login_controller iv_customer_number =  lc_customer_number_2 iv_email =  'test@web.de').

    m_environment->clear_doubles( ).

    m_cut = NEW zcl_homescreen_model( io_home_screen_controller = homescreen_controller iv_customer_number = lc_customer_number_2 ).
    m_environment->insert_test_data( EXPORTING i_data = lt_adress_data ).
  ENDMETHOD.

  METHOD class_setup.
    m_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'zweb_customer' ) ) ).
  ENDMETHOD.

  METHOD check_get_adress_of_customer.

    DATA: ls_order_adress TYPE zweb_s_adress.
    TRY.
        "when
        m_cut->get_address_of_customer(  ).
      CATCH zcx_webshop_exception_new .
        cl_abap_unit_assert=>fail( EXPORTING msg = 'Beim Aufruf der Methode get_address_of_customer ist ein Fehler aufgetreten' ).
    ENDTRY.
    ls_order_adress = m_cut->ms_order_address.

    "then
    cl_abap_unit_assert=>assert_equals( EXPORTING act =  ls_order_adress-street
                                                  exp =  lt_adress_data[ 2 ]-street ).

  ENDMETHOD.

  METHOD get_adress_of_custom_exc_exist.
    DATA: ls_order_adress TYPE zweb_s_adress.

    TRY.
        "given
        m_environment->clear_doubles( ).
        "when
        m_cut->get_address_of_customer(  ).
        ls_order_adress = m_cut->ms_order_address.

      CATCH zcx_webshop_exception_new.
        l_exception_occured = abap_true.
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_true(
      act = l_exception_occured
      msg = 'In der Methode get_address_of_customer wird keine Exception geworfen wenn ein Fehler auftritt.'
    ).

  ENDMETHOD.

ENDCLASS.