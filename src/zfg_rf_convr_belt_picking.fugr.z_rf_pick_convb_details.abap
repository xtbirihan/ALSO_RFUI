FUNCTION z_rf_pick_convb_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_PICK_CART_DETAILS) TYPE  ZSTR_RF_PICK_CART_DETAILS
*"----------------------------------------------------------------------

********************************************************************
*& Key          : <BSUGAREV>-Nov 22, 2023
*& Request No.  : GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  : Collect data for picking information
*&
*&
********************************************************************

  NEW lcl_pick_details( )->execute(
    CHANGING
      cs_ordim_confirm = ordim_confirm
      cs_pick_cart_details = zcs_pick_cart_details ).

ENDFUNCTION.
