FUNCTION z_rf_pick_sped_pall_spad_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZCS_SEL_QUEUE_PICKCART) TYPE
*"        ZSTR_RF_KEP_MC_SELSCR_PICK
*"     REFERENCE(ZCS_RF_SPED_PICKING_PALL_CR) TYPE
*"        ZSTR_RF_SPED_PICKING_PALL_CR
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-25.10.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM scan of picking process -  screen KEP/SPED pickign.
*& Control modul for process determiantion SPED or PALL
********************************************************************

  IF zcs_sel_queue_pickcart-pall_logic IS NOT INITIAL.
    /scwm/cl_rf_bll_srvc=>set_prmod( iv_prmod = /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).
  ELSEIF zcs_sel_queue_pickcart-sped_logic IS NOT INITIAL.
    /scwm/cl_rf_bll_srvc=>set_prmod( iv_prmod = /scwm/cl_rf_bll_srvc=>c_prmod_background ).
  ELSE.
    MESSAGE e069(zmc_rfui).
  ENDIF.

  CLEAR zcs_rf_sped_picking_pall_cr-huident.

  /scwm/cl_rf_bll_srvc=>set_screen_param( iv_param_name = lcl_out_sped_mc_picking=>c_create_pall_scr ).

ENDFUNCTION.
