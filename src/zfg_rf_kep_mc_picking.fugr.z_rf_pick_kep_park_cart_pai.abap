FUNCTION z_rf_pick_kep_park_cart_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(ZCT_RF_SPED_SN_ALL) TYPE  ZTT_RF_SPED_SN_ALL
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-01.09.2023
*& Request No.  :
********************************************************************
*& Description
*& RF  parking screen PAI
********************************************************************

  NEW lcl_out_kep_mc_picking( )->park_screen_pai( is_sel_queue_pickcart = zcs_sel_queue_pickcart
                                                  it_sn                 = zct_rf_sped_sn_all ).

ENDFUNCTION.
