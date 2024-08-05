FUNCTION z_rf_pick_zpicar_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
********************************************************************
*& Description  : Start of the pick by cart transaction. Scanned
*&    Queue and cart are validated. Main data for the transaction is
*&    collected and build
********************************************************************

  NEW lcl_start_cart_pai( )->execute(
    CHANGING
      cs_selection = selection
      cs_resource  = resource ).

ENDFUNCTION.
