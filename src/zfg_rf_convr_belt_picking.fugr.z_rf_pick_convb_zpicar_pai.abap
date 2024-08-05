FUNCTION z_rf_pick_convb_zpicar_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description
*&
*&
********************************************************************

  NEW lcl_start_cart_pai( )->execute(
    CHANGING
      cs_selection = selection
      cs_resource  = resource ).

ENDFUNCTION.
