*"* use this source file for your ABAP unit test classes

CLASS ltc_customer_model DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_cut               TYPE REF TO zcl_customer_login_model,
          lt_adress_data      TYPE TABLE OF zweb_customer,
          l_exception_occured TYPE abap_bool,
          login_controller    TYPE REF TO zcl_customer_login_controller.

    CONSTANTS: lc_customer_number_1 TYPE zweb_customer_number VALUE '1',
               lc_customer_number_2 TYPE zweb_customer_number VALUE '2',
               lc_name_1            TYPE zweb_name VALUE 'Meyer',
               lc_name_2            TYPE zweb_name VALUE 'Müller',
               lc_mail_1            TYPE zweb_email VALUE 'test@test.de',
               lc_mail_2            TYPE zweb_email VALUE 'unit-test@test.de',
               lc_password_1        TYPE zweb_password VALUE '1234',
               lc_password_2        TYPE zweb_password VALUE 'ABCD'.



    CLASS-DATA: m_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    METHODS setup.
    METHODS: check_email_is_available_true FOR TESTING,
      check_email_is_available_false FOR TESTING RAISING cx_static_check,
      check_passwort_no_account_exc FOR TESTING,
      check_passwort_false FOR TESTING,
      check_get_customer_number FOR TESTING,
      get_customer_number_exc_exist FOR TESTING.
ENDCLASS.

CLASS ltc_customer_model IMPLEMENTATION.
  METHOD setup.
    "given
    lt_adress_data = VALUE #( ( customer_number = lc_customer_number_1 name = lc_name_1 email = lc_mail_1 password = lc_password_1 )
                              ( customer_number = lc_customer_number_2 name = lc_name_2 email = lc_mail_2 password = lc_password_2 ) ).

    login_controller = NEW zcl_customer_login_controller(  ).

    m_environment->clear_doubles( ).

    m_cut = NEW zcl_customer_login_model( login_controller ).
    m_environment->insert_test_data( EXPORTING i_data = lt_adress_data ).
    l_exception_occured = abap_false.

  ENDMETHOD.

  METHOD class_setup.
    m_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'zweb_customer' ) ) ).
  ENDMETHOD.

  METHOD check_email_is_available_true.

    "given
    DATA: lv_mail TYPE zweb_email VALUE 'new@test.de'.
    TRY.
        "when
        m_cut->check_if_email_is_available( iv_email = lv_mail ).

      CATCH zcx_webshop_exception_new.
        l_exception_occured = abap_true.
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_false(
      act = l_exception_occured
      msg = 'Die Methode check_if_email_is_avaible funktioniert noch nicht richtig. Email noch nicht vorhanden deswegen darf sie verwendet werden!'
    ).
  ENDMETHOD.

  METHOD check_email_is_available_false.
  "given
    DATA: lv_mail TYPE zweb_email VALUE 'test@test.de'.
    TRY.
        "when
        m_cut->check_if_email_is_available( iv_email = lv_mail ).

      CATCH zcx_webshop_exception_new.
        l_exception_occured = abap_true.
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_true(
      act = l_exception_occured
      msg = 'Die Methode check_if_email_is_avaible funktioniert noch nicht richtig. Die Email ist schon vorhanden und darf nicht noch einmal verwendet werden!'
    ).

  ENDMETHOD.

  METHOD check_passwort_no_account_exc.
    "given
    DATA: lv_password TYPE zweb_password VALUE '1234',
          lv_mail     TYPE zweb_email VALUE 'Test2@test.de'.

    TRY.
        m_cut->check_if_password_is_correct( iv_email = lv_mail iv_password = lv_password ).
      CATCH zcx_webshop_exception_new.
        l_exception_occured = abap_true.
    ENDTRY.

    "then
    cl_abap_unit_assert=>assert_true(
      act = l_exception_occured
      msg = 'Die Methode check_if_password_is_correct wirft keine Exception, falls sie keinen Account findet!'
    ).
  ENDMETHOD.


  METHOD check_passwort_false.
    "given
    DATA: lv_password TYPE zweb_password VALUE '4321',
          lv_mail     TYPE zweb_email VALUE 'Test@test.de'.

    TRY.
        m_cut->check_if_password_is_correct( iv_email = lv_mail iv_password = lv_password ).
      CATCH zcx_webshop_exception_new INTO DATA(e_text).
        l_exception_occured = abap_true.
    ENDTRY.

    "then
    cl_abap_unit_assert=>assert_true(
      act = l_exception_occured
      msg = 'Die Methode check_if_password_is_correct wirft keine Exception, falls das Passwort faslsch ist!'
    ).
  ENDMETHOD.

  METHOD check_get_customer_number.
    "given
    DATA: lv_customer_number TYPE zweb_customer_number,
          lv_mail            TYPE zweb_email VALUE 'test@test.de'.

    TRY.
        "when
        lv_customer_number = m_cut->get_customer_number( iv_email = lv_mail ).
        "then
        cl_abap_unit_assert=>assert_equals( EXPORTING act =  lv_customer_number
                                                       exp =  lc_customer_number_1 ).
      CATCH zcx_webshop_exception_new.
        cl_abap_unit_assert=>fail( EXPORTING msg = 'Die Methode get_customer_number funktioniert nicht richtig!' ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_customer_number_exc_exist.
    "given
    DATA: lv_customer_number TYPE zweb_customer_number,
          lv_mail            TYPE zweb_email VALUE 'nicht-vorhanden@test.de'.

    TRY.
        "when
        lv_customer_number = m_cut->get_customer_number( iv_email = lv_mail ).

      CATCH zcx_webshop_exception_new.
        l_exception_occured = abap_true.

    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_true(
      act = l_exception_occured
      msg = 'Die Methode get_customer_number_exc_exist wirft keine Exception, falls ein Fehler aufgetreten ist!'
    ).
  ENDMETHOD.

ENDCLASS.
