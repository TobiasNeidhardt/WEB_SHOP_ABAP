class ZCL_INBOUND_DELIVERY_CNTRL definition
  public
  final
  create public .

public section.

  data MO_VIEW type ref to ZCL_INBOUND_DELIVERY_VIEW .

  methods START .
  methods CONSTRUCTOR
    importing
      !IO_LOG type ref to ZCL_WEBSHOP_LOG .
  PROTECTED SECTION.
private section.

  data MO_LOG type ref to ZCL_WEBSHOP_LOG .
  data MO_MODEL type ref to ZCL_INBOUND_DELIVERY_MODEL .
ENDCLASS.



CLASS ZCL_INBOUND_DELIVERY_CNTRL IMPLEMENTATION.


  METHOD constructor.

    me->mo_log = io_log.

    IF me->mo_view IS NOT BOUND.
      me->mo_view = NEW zcl_inbound_delivery_view( io_controller = me
                                                   io_log = me->mo_log ).
    ENDIF.

    IF me->mo_model IS NOT BOUND.
      me->mo_model = NEW zcl_inbound_delivery_model( io_log         = me->mo_log
                                                     io_controller  = me ).
    ENDIF.

  ENDMETHOD.


  METHOD start.

    "kleiner Tipp: Controller gibt der View die Anweisung das Login Dynpro zu starten
*    me->mo_view->call_dynpro_login( ).

  ENDMETHOD.
ENDCLASS.
