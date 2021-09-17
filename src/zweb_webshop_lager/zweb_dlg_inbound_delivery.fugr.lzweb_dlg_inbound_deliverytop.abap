FUNCTION-POOL ZWEB_DLG_INBOUND_DELIVERY.    "MESSAGE-ID ..

* INCLUDE LZWEB_DLG_INBOUND_DELIVERYD...     " Local class definition
DATA: go_login_view           TYPE REF TO zcl_inbound_delivery_view,
      gs_login_data           TYPE zweb_db_wh_ma,
      gv_article_number       TYPE zweb_article_number,
      go_putaway_article_view TYPE REF TO zcl_inbound_delivery_view,
      gv_lagernummer          TYPE zweb_warehouse_number,
      gv_lagerbereich         TYPE zweb_storage_area,
      gv_lagerplatz           TYPE zweb_storage_place,
      gv_storage_place_in     TYPE zweb_storage_place,
      gv_quantity             TYPE zweb_amount,
      gv_meins                TYPE zweb_unit.
