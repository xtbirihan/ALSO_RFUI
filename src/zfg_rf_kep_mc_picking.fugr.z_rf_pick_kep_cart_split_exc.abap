FUNCTION z_rf_pick_kep_cart_split_exc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(ZCT_WT_KEP_PICK_CART_GRP) TYPE
*"        ZEWM_TT_RF_PICK_CART_WT_CONF
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-28.09.2023
*& Request No.  :
********************************************************************
*& Description
*& RF exception code split cart. Set remaining WT to new WHO
*& SPED
********************************************************************

  NEW lcl_out_kep_mc_picking( )->split_who(
    CHANGING
      ct_ordim_confirm        = tt_ordim_confirm
      cs_wt_pick_screen_sourc = zcs_wt_pick_sreen_sourc
      cs_sel_queue_pickcart   = zcs_sel_queue_pickcart ).

ENDFUNCTION.
