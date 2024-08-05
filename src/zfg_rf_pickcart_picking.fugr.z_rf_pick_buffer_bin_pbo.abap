FUNCTION z_rf_pick_buffer_bin_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
********************************************************************
*& Description
*&
*&
********************************************************************

  NEW lcl_buffer_bin( )->execute_pbo(
    CHANGING
      cs_ptwy = cs_ptwy ).

ENDFUNCTION.
