FUNCTION z_rf_pick_kep_conf_dest_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_RF_SPED_PICKING_PALL_CR) TYPE
*"        ZSTR_RF_SPED_PICKING_PALL_CR
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-01.09.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM Final confirmation of all MC to Conveyor
*& PAI screen
********************************************************************

  NEW lcl_out_kep_mc_picking( )->pai_screen_wt_conf(
    CHANGING
      cs_sel_queue_pickcart   = zcs_sel_queue_pickcart
      cs_wt_pick_sreen_sourc  = zcs_wt_pick_sreen_sourc
      ct_ordim_to_conveyor    = tt_ordim_confirm
      cs_sped_picking_pall_cr = zcs_rf_sped_picking_pall_cr ). " this param. is relevant only for SPED/PALL

ENDFUNCTION.
