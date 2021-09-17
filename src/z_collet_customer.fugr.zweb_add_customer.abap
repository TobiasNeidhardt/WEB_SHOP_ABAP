FUNCTION ZWEB_ADD_CUSTOMER.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(IS_CUSTOMER) TYPE  ZWEB_CUSTOMER
*"----------------------------------------------------------------------



INSERT zweb_customer FROM is_customer.

IF sy-subrc EQ 0.
  COMMIT WORK.
  " Neuer Kunde &1 &2 mit Kundennummer &3 angelegt
  MESSAGE i002(z_web_shop) WITH is_customer-first_name is_customer-name is_customer-customer_number.
ELSE.
  ROLLBACK WORK.
  " fehler ....
  MESSAGE e003(z_web_shop).
ENDIF.

ENDFUNCTION.
