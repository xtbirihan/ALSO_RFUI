FUNCTION z_rf_putcrt_zptct1_enter.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_PTCRT) TYPE  ZSTR_RF_PROCESSES_PUTAWAY_CART
*"----------------------------------------------------------------------
  DATA ls_message TYPE bapiret2.

  BREAK-POINT ID zcg_rfputcrt.

  DATA(lo_putaway) = zcl_rf_putaway_with_cart=>get_instance(  ).

  " Validate the entered Cart HU
  ls_message = lo_putaway->validate_cart_hu( iv_cart_hu =  zs_ptcrt-cart_id ).
  IF ls_message IS NOT INITIAL.
    MESSAGE ID ls_message-id TYPE ls_message-type NUMBER ls_message-number
       WITH ls_message-message_v1 ls_message-message_v2 ls_message-message_v3 ls_message-message_v4.
    EXIT.
  ENDIF.

  " Move Cart HU to Ressource
  ls_message =  lo_putaway->assign_cart_to_resource( iv_cart_hu = zs_ptcrt-cart_id ).
  IF ls_message IS NOT INITIAL.
    MESSAGE ID ls_message-id TYPE ls_message-type NUMBER ls_message-number
       WITH  ls_message-message_v1 ls_message-message_v2 ls_message-message_v3 ls_message-message_v4.
    EXIT.
  ENDIF.

ENDFUNCTION.
