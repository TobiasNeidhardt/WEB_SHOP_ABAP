CLASS zcl_alternativ_adress DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: call_dynpro_for_address
      RAISING
        zcx_webshop_exception_new ,
      constructor
        IMPORTING
          !io_home_screen_controller TYPE REF TO zcl_homescreen_controller ,
      alternative_address_pai
        IMPORTING
          !is_order_address TYPE zweb_s_adress .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_home_screen_controller TYPE REF TO zcl_homescreen_controller .
ENDCLASS.



CLASS zcl_alternativ_adress IMPLEMENTATION.


  METHOD alternative_address_pai.

    CASE sy-ucomm.

      WHEN 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN 'LEAVE'.
        LEAVE PROGRAM.
      WHEN 'CONFIRM'.
        me->mo_home_screen_controller->on_confirm_address( is_order_address = is_order_address ).
    ENDCASE.

  ENDMETHOD.


  METHOD call_dynpro_for_address.
    TRY.
        CALL FUNCTION 'Z_SHIPPING_ADRESS'
          EXPORTING
            io_address_view = me.
      CATCH cx_root .
        RAISE EXCEPTION TYPE zcx_webshop_exception_new.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.

    me->mo_home_screen_controller = io_home_screen_controller.

  ENDMETHOD.
ENDCLASS.
