FUNCTION z_rf_pick_kep_next_prv_wt_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCT_WT_KEP_PICK_CART_GRP) TYPE
*"        ZEWM_TT_RF_PICK_CART_WT_CONF
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-28.08.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM get next line from the open pick wt. Screen control
********************************************************************

  NEW lcl_out_kep_mc_picking( )->next_wt(
    CHANGING
      ct_wt_kep_pick_cart_grp = zct_wt_kep_pick_cart_grp
      cs_wt_pick_screen_sourc = zcs_wt_pick_sreen_sourc
      ct_ordim_confirm        = tt_ordim_confirm ).

ENDFUNCTION.
