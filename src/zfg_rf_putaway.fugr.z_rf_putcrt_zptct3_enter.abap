FUNCTION z_rf_putcrt_zptct3_enter.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_PTCRT) TYPE  ZSTR_RF_PROCESSES_PUTAWAY_CART
*"----------------------------------------------------------------------
  DATA ls_message TYPE bapiret2.

  BREAK-POINT ID zcg_rfputcrt.

  DATA(lo_putaway) = zcl_rf_putaway_with_cart=>get_instance(  ).

  /scwm/cl_rf_bll_srvc=>set_prmod( /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).

  IF zs_ptcrt-cart_parking_lot IS INITIAL.
    RETURN.
  ENDIF.

  ls_message = lo_putaway->validate_parking_lot( iv_parking_lot = zs_ptcrt-cart_parking_lot ).

  IF ls_message IS NOT INITIAL.
    MESSAGE ID ls_message-id TYPE ls_message-type NUMBER ls_message-number
       WITH  ls_message-message_v1 ls_message-message_v2 ls_message-message_v3 ls_message-message_v4.
    EXIT.
  ENDIF.

  ls_message = lo_putaway->assign_cart_to_parking_lot( iv_cart_hu = zs_ptcrt-cart_id
                                                       iv_parking_lot = zs_ptcrt-cart_parking_lot ).
  IF ls_message IS NOT INITIAL.
    MESSAGE ID ls_message-id TYPE ls_message-type NUMBER ls_message-number
       WITH  ls_message-message_v1 ls_message-message_v2 ls_message-message_v3 ls_message-message_v4.
    EXIT.
  ENDIF.

  /scwm/cl_rf_bll_srvc=>set_prmod( /scwm/cl_rf_bll_srvc=>c_prmod_background ).
  /scwm/cl_rf_bll_srvc=>set_fcode( 'BACKF' ).
ENDFUNCTION.
