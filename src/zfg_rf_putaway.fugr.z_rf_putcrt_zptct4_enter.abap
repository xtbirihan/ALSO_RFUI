FUNCTION z_rf_putcrt_zptct4_enter.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_PTCRT) TYPE  ZSTR_RF_PROCESSES_PUTAWAY_CART
*"----------------------------------------------------------------------
  DATA ls_message TYPE bapiret2.

  BREAK-POINT ID zcg_rfputcrt.

  DATA(lo_putaway) = zcl_rf_putaway_with_cart=>get_instance(  ).

  /scwm/cl_rf_bll_srvc=>set_prmod( /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).

  CASE /scwm/cl_rf_bll_srvc=>get_fcode(  ).
    WHEN 'PTMAPB'.
      ls_message = lo_putaway->propose_storage_bin( CHANGING cs_cart_postion = zs_ptcrt ).
      EXIT.
  ENDCASE.

  IF zs_ptcrt-dest_storage_bin IS INITIAL.
    EXIT.
  ENDIF.

  " If the user decided not to change the destiantion bin after all.
  IF zs_ptcrt-org_dest_storage_bin = zs_ptcrt-dest_storage_bin.
    CLEAR zs_ptcrt-org_dest_storage_bin.
    CLEAR zs_ptcrt-execption.
  ELSE.
    zs_ptcrt-execption = 'CHBD'.
  ENDIF.

  /scwm/cl_rf_bll_srvc=>set_prmod( /scwm/cl_rf_bll_srvc=>c_prmod_background ).
  /scwm/cl_rf_bll_srvc=>set_fcode( 'BACKF' ).

ENDFUNCTION.
