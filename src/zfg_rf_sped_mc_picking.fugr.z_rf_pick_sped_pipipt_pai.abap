FUNCTION z_rf_pick_sped_pipipt_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-30.10.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM SPED/PALL Set Printer PAI
********************************************************************

  NEW lcl_out_sped_mc_picking( )->printer_validate( is_wt_pick_sreen_sourc = zcs_wt_pick_sreen_sourc
                                                    is_sel_queue_pickcart  = zcs_sel_queue_pickcart ).

ENDFUNCTION.
