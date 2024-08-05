FUNCTION z_rf_pick_zpicpe_fullhu_exc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(CT_SERNR) TYPE  /SCWM/TT_RF_SERNR
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
********************************************************************
*& Description  : Handling exception for HU Full
*&
*&
********************************************************************
  NEW lcl_hufull( )->execute(
    CHANGING
      cs_ordim_confirm = ordim_confirm
      cs_ptwy          = cs_ptwy ).

ENDFUNCTION.
