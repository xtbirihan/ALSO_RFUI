FUNCTION z_rf_pick_kep_validate_src_wt.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(ZCT_RF_SPED_SN_ALL) TYPE  ZTT_RF_SPED_SN_ALL
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-29.08.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM validate the source WT
********************************************************************

  NEW lcl_out_kep_mc_picking( )->validate_src_data(
    EXPORTING
       it_sn_scanned          = zct_rf_sped_sn_all
    CHANGING
      cs_wt_pick_screen_sourc = zcs_wt_pick_sreen_sourc
      cs_sel_queue_pickcart   = zcs_sel_queue_pickcart ).

ENDFUNCTION.
