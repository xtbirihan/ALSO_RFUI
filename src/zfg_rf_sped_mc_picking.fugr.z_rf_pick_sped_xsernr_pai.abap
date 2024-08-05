FUNCTION z_rf_pick_sped_xsernr_pai .
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
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCT_WT_KEP_PICK_CART_GRP) TYPE
*"        ZEWM_TT_RF_PICK_CART_WT_CONF
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-07.11.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM Scan SN handler
********************************************************************

  NEW lcl_out_sped_mc_picking( )->pai_screen_sn(
    EXPORTING
      it_ordim_conf           = tt_ordim_confirm
    CHANGING
      ct_wt_kep_pick_cart_grp = zct_wt_kep_pick_cart_grp
      cs_sel_queue_pickcart   = zcs_sel_queue_pickcart
      cs_sn                   = cs_sn
      ct_sernr                = zct_rf_sped_sn_all
      ct_sernr_list           = ct_sernr_list
      cs_wt_pick_screen_sourc = zcs_wt_pick_sreen_sourc ).

ENDFUNCTION.
