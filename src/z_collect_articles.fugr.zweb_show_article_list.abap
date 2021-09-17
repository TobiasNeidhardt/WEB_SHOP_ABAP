FUNCTION ZWEB_SHOW_ARTICLE_LIST.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"----------------------------------------------------------------------

DATA: lt_article         TYPE TABLE OF  zweb_article,
      lo_alv            TYPE REF TO    cl_salv_table.


SELECT *
  FROM zweb_article
  INTO TABLE lt_article.

IF sy-subrc EQ 0.
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = lo_alv
        CHANGING
          t_table        = lt_article
      ).

      lo_alv->display( ).
    CATCH cx_salv_msg.
  ENDTRY.

ELSEIF sy-subrc <> 0.
  " nachrichtnetext
  MESSAGE e004(z_web_shop).
ENDIF.




ENDFUNCTION.
