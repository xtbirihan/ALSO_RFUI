FUNCTION z_rf_pick_convb_zpichu_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"     REFERENCE(RF_PICK_HUS) TYPE  /SCWM/S_RF_PICK_HUS
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description
*&
*&
********************************************************************

  NEW lcl_zpichu_pai( )->execute(
    CHANGING
      cs_selection = selection
      cs_resource  = resource
      cs_ptwy      = cs_ptwy
      cs_rf_pick_hus  = rf_pick_hus ).

ENDFUNCTION.
