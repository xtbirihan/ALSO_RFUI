FUNCTION z_rf_putcrt_zptct3_init.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_PTCRT) TYPE  ZSTR_RF_PROCESSES_PUTAWAY_CART
*"----------------------------------------------------------------------
  DATA ls_message TYPE bapiret2.

  BREAK-POINT ID zcg_rfputcrt.

  DATA(lo_putaway) = zcl_rf_putaway_with_cart=>get_instance(  ).

  /scwm/cl_rf_bll_srvc=>set_prmod( /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).

  IF lo_putaway->cart_is_empty(  ).
    /scwm/cl_rf_bll_srvc=>set_prmod( /scwm/cl_rf_bll_srvc=>c_prmod_background ).
    /scwm/cl_rf_bll_srvc=>set_fcode( 'BACKF' ).
  ENDIF.
ENDFUNCTION.
