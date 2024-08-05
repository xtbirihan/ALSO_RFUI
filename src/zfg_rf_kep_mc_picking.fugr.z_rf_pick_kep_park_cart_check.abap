FUNCTION z_rf_pick_kep_park_cart_check.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-01.09.2023
*& Request No.  :
********************************************************************
*& Description
*& RF Check if we should part cart. If no tasks was confirmed we should
*& do nothing just jump back
********************************************************************

  NEW lcl_out_kep_mc_picking( )->unassign_who(
    CHANGING
      cs_sel_queue_pickcart = zcs_sel_queue_pickcart
      ct_ordim_to_conveyor  = tt_ordim_confirm ).

ENDFUNCTION.
