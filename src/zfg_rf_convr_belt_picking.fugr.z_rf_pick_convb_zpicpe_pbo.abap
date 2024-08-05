FUNCTION z_rf_pick_convb_zpicpe_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(CT_SERNR) TYPE  /SCWM/TT_RF_SERNR
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  : GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description
*&
*&
********************************************************************

  NEW lcl_zpicpe_pbo( )->execute(
    CHANGING
      cs_selection   = selection
      cs_resource    = resource
      cs_wt_pick_sreen_sourc = zcs_wt_pick_sreen_sourc
      cs_ordim_confirm = ordim_confirm
      ct_ordim_confirm = tt_ordim_confirm
      ct_sernr = ct_sernr ).

ENDFUNCTION.
