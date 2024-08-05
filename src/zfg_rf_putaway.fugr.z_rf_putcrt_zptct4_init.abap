FUNCTION z_rf_putcrt_zptct4_init.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_PTCRT) TYPE  ZSTR_RF_PROCESSES_PUTAWAY_CART
*"----------------------------------------------------------------------
  DATA ls_message TYPE bapiret2.

  BREAK-POINT ID zcg_rfputcrt.

  DATA(lo_putaway) = zcl_rf_putaway_with_cart=>get_instance(  ).

  IF zs_ptcrt-org_dest_storage_bin IS INITIAL.
    zs_ptcrt-org_dest_storage_bin = zs_ptcrt-dest_storage_bin.
  ENDIF.

  CLEAR zs_ptcrt-dest_storage_bin.

ENDFUNCTION.
