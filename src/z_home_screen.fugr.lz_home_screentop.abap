FUNCTION-POOL Z_HOME_SCREEN.                "MESSAGE-ID ..

* INCLUDE LZ_HOME_SCREEND...                 " Local class definition
DATA go_home_screen_view TYPE REF TO zcl_homescreen_view.
DATA go_cart_view TYPE REF TO zcl_cart_view.
DATA p_quantity TYPE zweb_order_amount.
DATA p_search TYPE string.
DATA go_address_view TYPE REF TO zcl_alternativ_adress.
DATA p_street TYPE zweb_street.
DATA p_house_number TYPE zweb_house_nr.
DATA p_zip_code TYPE zweb_postalcode.
DATA p_address_city TYPE zweb_city.
DATA go_order_overview_view TYPE REF TO zcl_homescreen_order_view.
