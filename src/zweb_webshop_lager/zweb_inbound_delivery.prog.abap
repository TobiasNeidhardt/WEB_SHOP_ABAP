*&---------------------------------------------------------------------*
*& Report ZSBT_INBOUND_DELIVERY
*&---------------------------------------------------------------------*
*& Report zur Lagerverwaltung:
*&---------------------------------------------------------------------*
REPORT zweb_inbound_delivery.

CONSTANTS: lc_logobject TYPE bal_s_log-object    VALUE 'ZWEB',
           lc_subobjec  TYPE bal_s_log-subobject VALUE 'ZWEB'.

"start the application
TRY.
    DATA(lo_log) = NEW zcl_webshop_log( iv_object = lc_logobject
                                         iv_suobj = lc_subobjec ).

    NEW zcl_inbound_delivery_cntrl( io_log = lo_log )->start( ).

  CATCH zcx_webshop_exception_new INTO DATA(lo_exc).

    "logg messages and display in a popup
    lo_log->safe_log( ).
    lo_log->display_log_as_popup( ).

    "restart the application
    SUBMIT zweb_inbound_delivery.
ENDTRY.
