FUNCTION z_rf_pick_kep_part_conf_exc.
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
*"     REFERENCE(ZCT_RF_SPED_SN_ALL) TYPE  ZTT_RF_SPED_SN_ALL
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-28.09.2023
*& Request No.  :
********************************************************************
*& Description
*& RF exception code execution partial MC confirmation
********************************************************************

  NEW lcl_out_kep_mc_picking( )->src_wt_confirm_mc(
    EXPORTING
      iv_exc_part_master_carton = CONV #( zcs_wt_pick_sreen_sourc-num_mc_varif )
    CHANGING
      ct_ordim_confirm        = tt_ordim_confirm
      cs_wt_pick_screen_sourc = zcs_wt_pick_sreen_sourc
      cs_sel_queue_pickcart   = zcs_sel_queue_pickcart
      ct_wt_kep_pick_cart_grp = zct_wt_kep_pick_cart_grp
      ct_sn                   = zct_rf_sped_sn_all  ).

ENDFUNCTION.
