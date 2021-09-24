CLASS zcl_customer_login_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      check_if_password_is_correct
        IMPORTING
                  !iv_password TYPE zweb_password
                  !iv_email    TYPE zweb_email
        RAISING   zcx_webshop_exception_new,
      save_registration_customer
        IMPORTING
                  !is_register_data TYPE zweb_s_register
        RAISING   zcx_webshop_exception_new,
      check_if_email_is_available
        IMPORTING
                  !iv_email TYPE zweb_email
        RAISING   zcx_webshop_exception_new,
      constructor
        IMPORTING
          !io_controller TYPE REF TO zcl_customer_login_controller
          !io_log        TYPE REF TO zcl_webshop_log,
      get_customer_number
        IMPORTING
                  !iv_email                TYPE zweb_email
        RETURNING
                  VALUE(rv_customernumber) TYPE zweb_customer_number
        RAISING   zcx_webshop_exception_new.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS mc_range_nr TYPE nrnr VALUE '01' ##NO_TEXT.
    DATA: mo_controller TYPE REF TO zcl_customer_login_controller,
          mo_log        TYPE REF TO zcl_webshop_log.

    METHODS get_new_customer_number
      RETURNING
                VALUE(rv_customer_number) TYPE numc10
      RAISING   zcx_webshop_exception_new.
ENDCLASS.



CLASS zcl_customer_login_model IMPLEMENTATION.


  METHOD check_if_email_is_available.

    SELECT SINGLE *
      FROM zweb_customer
      INTO @DATA(ls_customer)
      WHERE email = @iv_email.

    IF sy-subrc <> 4 OR ls_customer IS NOT INITIAL.
      MESSAGE i088(z_web_shop) INTO DATA(ls_msg).
      me->mo_log->add_msg_from_sys( ).
      me->mo_log->safe_log( ).
      RAISE EXCEPTION TYPE zcx_webshop_exception_new USING MESSAGE.
    ENDIF.

  ENDMETHOD.


  METHOD check_if_password_is_correct.

    SELECT SINGLE password
      FROM zweb_customer
    INTO @DATA(lv_password)
      WHERE email = @iv_email.

    IF sy-subrc <> 0.
      "no account could be found => Error Message
      MESSAGE i039(z_web_shop) INTO DATA(ls_msg).
      me->mo_log->add_msg_from_sys( ).
      me->mo_log->safe_log( ).
      RAISE EXCEPTION TYPE zcx_webshop_exception_new USING MESSAGE.
    ENDIF.

    "Password is not correct => Error Message
    IF lv_password NE iv_password.
      MESSAGE i038(z_web_shop) INTO ls_msg.
      me->mo_log->add_msg_from_sys( ).
      me->mo_log->safe_log( ).
      RAISE EXCEPTION TYPE zcx_webshop_exception_new USING MESSAGE.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    me->mo_controller = io_controller.
    me->mo_log = io_log.

  ENDMETHOD.


  METHOD get_customer_number.

    SELECT SINGLE customer_number
      FROM zweb_customer
      INTO @DATA(lv_customernumber)
      WHERE email = @iv_email.

    IF sy-subrc <> 0.
      MESSAGE i044(z_web_shop) INTO DATA(ls_msg).
      me->mo_log->add_msg_from_sys( ).
      me->mo_log->safe_log( ).
      RAISE EXCEPTION TYPE zcx_webshop_exception_new USING MESSAGE.
    ENDIF.

    rv_customernumber = lv_customernumber.

  ENDMETHOD.


  METHOD get_new_customer_number.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = mc_range_nr
        object      = 'ZWEB_CUSTO'
      IMPORTING
        number      = rv_customer_number
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc <> 0.
      MESSAGE i041(z_web_shop) INTO DATA(ls_msg).
      me->mo_log->add_msg_from_sys( ).
      me->mo_log->safe_log( ).
      RAISE EXCEPTION TYPE zcx_webshop_exception_new USING MESSAGE.
    ENDIF.

  ENDMETHOD.


  METHOD save_registration_customer.

    DATA: ls_register_data_for_insert TYPE zweb_customer.

    ls_register_data_for_insert = VALUE #( customer_number = me->get_new_customer_number( )
                                           city            = is_register_data-city
                                           email           = is_register_data-email
                                           first_name      = is_register_data-firstname
                                           name            = is_register_data-name
                                           house_number    = is_register_data-house_number
                                           salutation      = is_register_data-salutation
                                           street          = is_register_data-street
                                           zip_code        = is_register_data-zip_code
                                           telephone_number  = is_register_data-telephone_number
                                           password        = me->mo_controller->encrypt_password( iv_password = is_register_data-password ) ).

    INSERT zweb_customer FROM ls_register_data_for_insert.

    IF sy-subrc <> 0.
      MESSAGE i042(z_web_shop) INTO DATA(ls_msg).
      me->mo_log->add_msg_from_sys( ).
      me->mo_log->safe_log( ).
      RAISE EXCEPTION TYPE zcx_webshop_exception_new USING MESSAGE.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
