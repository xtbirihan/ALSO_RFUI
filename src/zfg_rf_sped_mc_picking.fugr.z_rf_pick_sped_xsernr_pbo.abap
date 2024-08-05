FUNCTION z_rf_pick_sped_xsernr_pbo .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCT_RF_SPED_SN_ALL) TYPE  ZTT_RF_SPED_SN_ALL
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(CS_SN) TYPE  /SCWM/S_RF_SN
*"     REFERENCE(CT_SERNR_LIST) TYPE  /SCWM/TT_RF_SERNR
*"----------------------------------------------------------------------
*******************************************************************
*& Key          : <AYORDANOV>-07.11.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM preparation of Scan SN screen
********************************************************************

  NEW lcl_out_sped_mc_picking( )->pbo_screen_sn(
    CHANGING
      cs_sel_queue_pickcart   = zcs_sel_queue_pickcart
      cs_sn                   = cs_sn
      ct_sernr                = zct_rf_sped_sn_all
      ct_sernr_list           = ct_sernr_list
      cs_wt_pick_screen_sourc = zcs_wt_pick_sreen_sourc ).

ENDFUNCTION.
