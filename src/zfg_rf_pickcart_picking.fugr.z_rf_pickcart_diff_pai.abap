FUNCTION z_rf_pickcart_diff_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(CT_SERNR_DIFF) TYPE  /SCWM/TT_RF_SERNR
*"     REFERENCE(CS_SN) TYPE  /SCWM/S_RF_SN
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  : Update tasks in case of quantity difference
*&
*&
********************************************************************

  NEW lcl_diff_pai( )->execute(
    CHANGING
      ct_ordim_confirm = tt_ordim_confirm
      cs_ordim_confirm = ordim_confirm
      ct_sernr_diff    = ct_sernr_diff
      cs_sn            = cs_sn ).

ENDFUNCTION.
