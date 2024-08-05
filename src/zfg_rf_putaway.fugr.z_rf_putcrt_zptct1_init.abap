FUNCTION z_rf_putcrt_zptct1_init.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_PTCRT) TYPE  ZSTR_RF_PROCESSES_PUTAWAY_CART
*"----------------------------------------------------------------------


  BREAK-POINT ID zcg_rfputcrt.

  CLEAR: zs_ptcrt.

  zs_ptcrt-header = 'Putaway with Cart'(001).

  /scwm/cl_rf_bll_srvc=>init_screen_param( ).
  /scwm/cl_rf_bll_srvc=>set_screen_param( 'ZS_PTCRT' ).


  DATA(lo_putaway) = zcl_rf_putaway_with_cart=>get_instance(  ).

  IF lo_putaway IS NOT BOUND.
    MESSAGE e010(/scwm/rf_de).
* Initialization for warehouse number &1 failed
  ENDIF.

  zs_ptcrt-lgnum = lo_putaway->ss_resource-lgnum.
ENDFUNCTION.
