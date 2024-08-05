FUNCTION z_rf_pick_putwall_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************

  NEW lcl_putwall_pbo( )->execute(
    CHANGING
      cs_selection = selection
      cs_resource  = resource
      cs_ptwy      = cs_ptwy ).

ENDFUNCTION.
