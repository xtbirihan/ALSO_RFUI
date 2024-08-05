FUNCTION z_rf_pick_kep_diff_conf_pai .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(ZCT_WT_KEP_PICK_CART_GRP) TYPE
*"        ZEWM_TT_RF_PICK_CART_WT_CONF
*"     REFERENCE(ZCT_RF_SPED_SN_ALL) TYPE  ZTT_RF_SPED_SN_ALL
*"     REFERENCE(ZCS_RF_SPED_PICKING_PALL_CR) TYPE
*"        ZSTR_RF_SPED_PICKING_PALL_CR
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-01.09.2023
*& Request No.  :
********************************************************************
*& Description
*& RF confirmation of diff qty of MC
*& PAI screen
********************************************************************


  NEW lcl_out_kep_mc_picking( )->conf_diff_pai(
    CHANGING
      cs_ordim_confirm_diff    = ordim_confirm
      ct_wt_kep_pick_cart_grp  = zct_wt_kep_pick_cart_grp
      ct_ordim_confirm         = tt_ordim_confirm
      cs_wt_pick_screen_sourc  = zcs_wt_pick_sreen_sourc
      cs_sel_queue_pickcart    = zcs_sel_queue_pickcart
      ct_sn                    = zct_rf_sped_sn_all
      cs_sped_picking_pall_cr  = zcs_rf_sped_picking_pall_cr ).

ENDFUNCTION.
