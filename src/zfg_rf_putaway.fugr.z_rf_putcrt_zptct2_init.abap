FUNCTION z_rf_putcrt_zptct2_init.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_PTCRT) TYPE  ZSTR_RF_PROCESSES_PUTAWAY_CART
*"----------------------------------------------------------------------
  BREAK-POINT ID zcg_rfputcrt.

  DATA(lo_putaway) = zcl_rf_putaway_with_cart=>get_instance(  ).
  lo_putaway->show_next_cart_position( CHANGING cs_cart_postion = zs_ptcrt ).
  lo_putaway->change_screen_layout( zs_ptcrt ).
ENDFUNCTION.
