*&---------------------------------------------------------------------*
*& Report Z_COLLECT_ARTICLES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_COLLECT_ARTICLES.

PARAMETERS: p_desig   TYPE zweb_designation  OBLIGATORY,
            p_descr TYPE zweb_description OBLIGATORY,
           p_curr TYPE zweb_currency     OBLIGATORY,
           p_unit  TYPE zweb_unit      OBLIGATORY,
            p_price TYPE zweb_price        OBLIGATORY.

DATA: lv_art_nr      TYPE                          i,
      ls_article        TYPE           zweb_article,
      lt_article         TYPE TABLE OF  zweb_article,
      lo_alv            TYPE REF TO    cl_salv_table.

CONSTANTS: lc_range_nr TYPE nrnr VALUE '01'.

"ZSBT_CL_WEBSHOP_CONTANTEN=>GC_range_nr.

CALL FUNCTION 'NUMBER_GET_NEXT'
  EXPORTING
    nr_range_nr = lc_range_nr
    object      = 'ZWEB_ART' "todo
  IMPORTING
    number      = lv_art_nr
  EXCEPTIONS
    OTHERS      = 1.
IF sy-subrc <> 0.
  MESSAGE e001(z_web_shop).
ENDIF.

ls_article-article_number = lv_art_nr.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input  = ls_article-article_number
  IMPORTING
    output = ls_article-article_number.

ls_article-designation   = p_desig.
ls_article-description  = p_descr.
ls_article-currency      = p_curr.
ls_article-price         = p_price.
ls_article-unit       = p_unit.

CALL FUNCTION 'ZWEB_ADD_ARTICLE'
  EXPORTING is_article = ls_article.

CALL FUNCTION 'ZWEB_SHOW_ARTICLE_LIST'.