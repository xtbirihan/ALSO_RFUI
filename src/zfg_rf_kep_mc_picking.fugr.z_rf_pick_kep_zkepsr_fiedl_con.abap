FUNCTION z_rf_pick_kep_zkepsr_fiedl_con.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCT_WT_KEP_PICK_CART_GRP) TYPE
*"        ZEWM_TT_RF_PICK_CART_WT_CONF
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-20.09.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM Hide Empty Fields + Display final destination like msg
********************************************************************

  lcl_out_kep_mc_picking=>fields_control_scr( it_wt_kep_pick_cart_grp = zct_wt_kep_pick_cart_grp
                                              is_wt_pick_screen_src   = zcs_wt_pick_sreen_sourc
                                              it_ordim_confirm        = tt_ordim_confirm ).

ENDFUNCTION.
