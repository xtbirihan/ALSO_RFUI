FUNCTION z_rf_pick_kep_ent_sel_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-15.08.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM scan of picking process -  screen KEP/SPED pickign.
*& user can scan Queue or directly pick cart
********************************************************************

  NEW lcl_out_kep_mc_picking( )->queue_pick_cart_sel(
    EXPORTING is_wt_pick_sreen_sourc = zcs_wt_pick_sreen_sourc
    IMPORTING et_ordim_conf          = tt_ordim_confirm
    CHANGING cs_sel_queue_pickcart   = zcs_sel_queue_pickcart ).

ENDFUNCTION.
