FUNCTION z_rf_pick_kep_diff_pbo .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_WT_PICK_SREEN_SOURC) TYPE
*"        ZSTR_RF_PICK_CART_WT_CONF
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-01.09.2023
*& Request No.  :
********************************************************************
*& Description
*& RF screen preparation confirmation of diff qty of MC
*& PAI screen
********************************************************************


  lcl_out_kep_mc_picking=>diff_scr_pbo(
    EXPORTING
      is_wt_pick_sreen_sourc      = zcs_wt_pick_sreen_sourc
    CHANGING
      cs_ordim_confirm_diff = ordim_confirm ).

ENDFUNCTION.
