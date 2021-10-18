class ZCL_INBOUND_DELIVERY_MODEL definition
  public
  final
  create public .

public section.

  data MO_LOG type ref to ZCL_WEBSHOP_LOG .

  methods CONSTRUCTOR
    importing
      !IO_LOG type ref to ZCL_WEBSHOP_LOG
      !IO_CONTROLLER type ref to ZCL_INBOUND_DELIVERY_CNTRL .
  PROTECTED SECTION.
private section.

  data MO_CONTROLLER type ref to ZCL_INBOUND_DELIVERY_CNTRL .
ENDCLASS.



CLASS ZCL_INBOUND_DELIVERY_MODEL IMPLEMENTATION.


  METHOD constructor.

    me->mo_log = io_log.
    me->mo_controller = io_controller.

  ENDMETHOD.
ENDCLASS.
