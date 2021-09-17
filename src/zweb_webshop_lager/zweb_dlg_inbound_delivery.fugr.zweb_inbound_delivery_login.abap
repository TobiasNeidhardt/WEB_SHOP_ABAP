FUNCTION ZWEB_INBOUND_DELIVERY_LOGIN.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(IO_VIEW) TYPE REF TO  ZCL_INBOUND_DELIVERY_VIEW
*"----------------------------------------------------------------------


  go_login_view = io_view.

  CALL SCREEN 9000.


ENDFUNCTION.
