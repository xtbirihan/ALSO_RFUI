FUNCTION z_rf_pick_info_who_pbo.
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
*"     REFERENCE(ZCS_PICK_MC_WHO_INFO) TYPE  ZSTR_RF_PICK_MC_WHO_INFO
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-26.11.2023
*& Request No.  :
********************************************************************
*& Description
*& RF PICK Details remaining pick WHo
********************************************************************

  lcl_out_kep_mc_picking=>info_screen_who(
    EXPORTING
      it_ordim_confirm        = tt_ordim_confirm
      is_wt_pick_sreen_sourc  = zcs_wt_pick_sreen_sourc
      is_sel_queue_pickcart   = zcs_sel_queue_pickcart
      it_wt_kep_pick_cart_grp = zct_wt_kep_pick_cart_grp
    CHANGING
      cs_pick_mc_who_info     = zcs_pick_mc_who_info ).

ENDFUNCTION.
