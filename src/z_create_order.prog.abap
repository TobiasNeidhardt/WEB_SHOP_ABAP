*&---------------------------------------------------------------------*
*& Report Z_CREATE_ORDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_create_order.

PARAMETERS: p_artnum TYPE zweb_article_number MATCHCODE OBJECT zweb_sh_article OBLIGATORY,
            p_kundid TYPE zweb_kd_numr_de MATCHCODE OBJECT zweb_sh_customer OBLIGATORY,
            p_amount TYPE zweb_order_amount  OBLIGATORY,
            p_bmeins TYPE zweb_unit  OBLIGATORY.

CONSTANTS: lc_range_nr       TYPE  nrnr  VALUE '01',
           lc_status_ordered TYPE string VALUE 'BESTELLT',
           lc_status_cart    TYPE string VALUE 'Im Warenkorb'.

DATA: lo_alv          TYPE REF TO   cl_salv_table
      ,ls_order       TYPE          zweb_order
      ,lv_bnumber_int TYPE          i
      ,lv_bnumber_chr TYPE          zweb_order_number
      ,lt_cart   TYPE TABLE OF zweb_order
      ,lo_columns     TYPE REF TO cl_salv_columns_table
      ,lo_column      TYPE REF TO cl_salv_column.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (33) FOR FIELD p_amount.

SELECTION-SCREEN END OF LINE.

FIELD-SYMBOLS <fs_cart> TYPE zweb_order.

SELECTION-SCREEN:
PUSHBUTTON /2(20) button1 USER-COMMAND add_to_cart , "Zum Warenkorb hinzufügen"
PUSHBUTTON /2(20) button2 USER-COMMAND order, "Bestellung aufgeben"
PUSHBUTTON /2(20) button3 USER-COMMAND show. "Bestellung anzeigen"


INITIALIZATION.
  button1 = TEXT-b01.
  button2 = TEXT-b02.
  button3 = TEXT-b03.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'add_to_cart'.

    WHEN 'order'.

    WHEN 'show'.

  ENDCASE.
