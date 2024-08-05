FUNCTION z_rf_pick_zpicpe_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(RF_PICK_HUS) TYPE  /SCWM/S_RF_PICK_HUS
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(CS_SN) TYPE  /SCWM/S_RF_SN
*"     REFERENCE(CT_SERNR) TYPE  /SCWM/TT_RF_SERNR
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
********************************************************************
*& Description
*&
*&
********************************************************************

  NEW lcl_zpicpe_pai( )->execute(
    CHANGING
      cs_ordim_confirm = ordim_confirm
      ct_ordim_confirm = tt_ordim_confirm
      cs_sn  = cs_sn
      ct_sernr = ct_sernr ).

ENDFUNCTION.
