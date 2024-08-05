FUNCTION z_rf_pick_buffer_bin_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
********************************************************************
*& Description  : Move cart to parking bin
*&
*&
********************************************************************

  NEW lcl_buffer_bin( )->execute_pai(
    CHANGING
      cs_resource  = resource
      cs_ptwy      = cs_ptwy
      cs_selection = selection ).

ENDFUNCTION.
