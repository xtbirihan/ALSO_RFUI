FUNCTION z_rf_pick_putwall_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
********************************************************************
*& Description  : PUTWALL area, leave or pick HUs
*&
*&
********************************************************************

  NEW lcl_putwall_pai( )->execute(
    CHANGING
      cs_selection = selection
      cs_resource  = resource
      cs_ptwy   = cs_ptwy ).

ENDFUNCTION.
