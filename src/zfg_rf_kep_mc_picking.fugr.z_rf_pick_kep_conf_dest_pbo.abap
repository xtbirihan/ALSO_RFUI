FUNCTION z_rf_pick_kep_conf_dest_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-31.08.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM preparation final Conveyor confirmation.
*& PBO screen
********************************************************************

  NEW lcl_out_kep_mc_picking( )->pbo_screen_wt_conv(
    EXPORTING
      is_sel_queue_pickcart  = zcs_sel_queue_pickcart
    CHANGING
      cs_wt_pick_sreen_sourc = zcs_wt_pick_sreen_sourc
      ct_ordim_to_conveyor   = tt_ordim_confirm ).

ENDFUNCTION.
