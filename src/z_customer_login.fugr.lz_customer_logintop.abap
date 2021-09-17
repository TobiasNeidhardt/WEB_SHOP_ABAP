FUNCTION-POOL Z_CUSTOMER_LOGIN.             "MESSAGE-ID ..

* INCLUDE LZ_CUSTOMER_LOGIND...              " Local class definition
DATA go_login_view TYPE REF TO zcl_customer_login_view.
DATA go_customer_register_view TYPE REF TO zcl_customer_register_view.
DATA p_email    TYPE zweb_email.
DATA p_password TYPE zweb_password.
DATA p_password_repeat TYPE zweb_password.
DATA p_street TYPE zweb_street.
DATA p_house_number TYPE zweb_house_nr.
DATA p_zipcode TYPE zweb_postalcode.
DATA p_city TYPE zweb_city.
DATA p_telephone_number TYPE zweb_phone_number.
DATA gs_register_data TYPE zweb_s_register.
DATA p_salutation type zweb_salutation.
DATA p_firstname type zweb_firstname.
DATA p_name type zweb_name.
