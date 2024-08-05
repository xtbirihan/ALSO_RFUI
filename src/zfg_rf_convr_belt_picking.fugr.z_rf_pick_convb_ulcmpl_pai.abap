FUNCTION z_rf_pick_convb_ulcmpl_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
*&
*&
********************************************************************

  NEW lcl_unload_pai( )->execute(
    CHANGING
      cs_selection     = selection
      cs_resource      = resource
      cs_ordim_confirm = ordim_confirm ).

ENDFUNCTION.
