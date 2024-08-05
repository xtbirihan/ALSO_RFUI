FUNCTION z_rf_pick_zpicar_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-06.07.2023 15:28:57
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
********************************************************************
*& Description  : RF picking by cart process. Prepare the output data
*&    for the initial screen
********************************************************************

  NEW lcl_start_cart_pbo( )->execute(
    CHANGING
      cs_selection = selection
      cs_resource  = resource ).

ENDFUNCTION.
