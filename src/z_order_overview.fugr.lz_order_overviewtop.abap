FUNCTION-POOL Z_ORDER_OVERVIEW.             "MESSAGE-ID ..

* INCLUDE LZ_ORDER_OVERVIEWD...              " Local class definition

DATA: go_web_shop TYPE REF TO zcl_order_overview_view.
DATA: p_ein       TYPE        zweb_order_amount.
DATA: p_status    TYPE        zweb_status.
