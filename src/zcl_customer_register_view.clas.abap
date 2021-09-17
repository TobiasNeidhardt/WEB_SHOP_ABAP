CLASS zcl_customer_register_view DEFINITION
PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: call_register_screen
      RAISING
        zcx_webshop_exception_new ,
      register_screen_pai
        IMPORTING
          !is_register_data TYPE zweb_s_register ,
      constructor
        IMPORTING
          !io_login_controller TYPE REF TO zcl_customer_login_controller
        EXCEPTIONS
          io_controller .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_login_controller TYPE REF TO zcl_customer_login_controller .
ENDCLASS.



CLASS zcl_customer_register_view IMPLEMENTATION.


  METHOD call_register_screen.
    TRY.
        CALL FUNCTION 'Z_CUSTOMER_REGISTER'
          EXPORTING
            io_customer_register_view = me.
      CATCH cx_root .
        RAISE EXCEPTION TYPE zcx_webshop_exception_new.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.

    mo_login_controller = io_login_controller.

  ENDMETHOD.


  METHOD register_screen_pai.

    CASE sy-ucomm.

      WHEN 'BACK'.
        me->mo_login_controller->on_back( ).

      WHEN 'LEAVE'.
        me->mo_login_controller->on_leave( ).

      WHEN 'CONFIRM'.
        me->mo_login_controller->on_confirm_registration( is_register_data = is_register_data ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
