FUNCTION z_rf_pick_kep_ent_create_cart.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-15.08.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM create picking cart
********************************************************************

  NEW lcl_out_kep_mc_picking( )->create_cart(
    CHANGING
      cs_sel_queue_pickcart = zcs_sel_queue_pickcart ).

ENDFUNCTION.
