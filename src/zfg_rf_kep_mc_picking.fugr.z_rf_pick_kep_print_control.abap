FUNCTION z_rf_pick_kep_print_control.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-11.01.2024
*& Request No.  :
********************************************************************
********************************************************************
*& Description
*& RF change mode of Printing determination screen. Switch on/off
*& of the logic
********************************************************************

  NEW lcl_out_kep_mc_picking( )->switch_print_logic( cs_sel_queue_pickcart = zcs_sel_queue_pickcart ).

ENDFUNCTION.
