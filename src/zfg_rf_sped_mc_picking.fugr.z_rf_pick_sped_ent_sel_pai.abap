FUNCTION z_rf_pick_sped_ent_sel_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_RF_SPED_PICKING_PALL_CR) TYPE
*"        ZSTR_RF_SPED_PICKING_PALL_CR
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-15.08.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM scan of picking process -  screen KEP/SPED pickign.
*& user can scan Queue or directly pick cart
********************************************************************

  NEW lcl_out_sped_mc_picking( )->queue_pick_cart_sel(
    IMPORTING et_ordim_conf         = tt_ordim_confirm
    CHANGING cs_sel_queue_pickcart  = zcs_sel_queue_pickcart
             cs_sped_pall_top_hu_cr = zcs_rf_sped_picking_pall_cr ).

ENDFUNCTION.
