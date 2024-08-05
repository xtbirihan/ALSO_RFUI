FUNCTION z_rf_pick_kep_init_sel_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AYORDANOV>-15.08.2023
*& Request No.  :
********************************************************************
*& Description
*& RF FM initial step of loading of the screen KEP/SPED pickign
********************************************************************

  /scwm/cl_rf_bll_srvc=>init_screen_param( ).
  /scwm/cl_rf_bll_srvc=>set_screen_param( iv_param_name = lcl_out_kep_mc_picking=>c_sel_queue_pickcart ).
  /scwm/cl_rf_bll_srvc=>set_screen_param( iv_param_name = lcl_out_kep_mc_picking=>c_wt_pick_group_load ).

ENDFUNCTION.
