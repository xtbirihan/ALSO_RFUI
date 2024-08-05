FUNCTION z_rf_putcrt_zptct2_enter.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_PTCRT) TYPE  ZSTR_RF_PROCESSES_PUTAWAY_CART
*"----------------------------------------------------------------------

  DATA ls_message TYPE bapiret2.
  DATA lv_shortcut TYPE /scwm/de_shortcut.

  BREAK-POINT ID zcg_rfputcrt.

  DATA(lo_putaway) = zcl_rf_putaway_with_cart=>get_instance(  ).

  /scwm/cl_rf_bll_srvc=>set_prmod( /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).


  CASE /scwm/cl_rf_bll_srvc=>get_fcode(  ).
    WHEN 'SKIP'.
      lo_putaway->show_next_cart_position( CHANGING cs_cart_postion = zs_ptcrt ).
      lo_putaway->change_screen_layout( zs_ptcrt ).

      EXIT.
  ENDCASE.

  " For some reason, the Function Module is triggerd by the content provider, in between every validation field.
  " So we have to make sure, that all varification fields are scanned before processing the the data.

  " HU Verification can't be epmty
  IF zs_ptcrt-source_hu_verify IS INITIAL.
    EXIT.
  ENDIF.

  " The Destination bin can't be empty.
  IF zs_ptcrt-dest_storage_bin IS INITIAL.
    EXIT.
  ENDIF.

  IF zs_ptcrt-dest_bin_verify IS INITIAL.
    EXIT.
  ENDIF.

  " Only count the tote, if there are actually muiltiple totes to count.
  IF zs_ptcrt-number_of_totes > 0.
    zs_ptcrt-scanned_totes = zs_ptcrt-scanned_totes + 1.
  ENDIF.

  IF zs_ptcrt-scanned_totes < zs_ptcrt-number_of_totes.
    " If not all totes have been scanned yet, skip the WT condfirmation and
    " return to the screen.
    CLEAR zs_ptcrt-dest_bin_verify.
    CLEAR zs_ptcrt-source_hu_verify.
    EXIT.
  ENDIF.

  ls_message = lo_putaway->finish_cart_positon( zs_ptcrt ).
  IF ls_message IS INITIAL.
    CLEAR zs_ptcrt-dest_bin_verify.
    CLEAR zs_ptcrt-source_hu_verify.

    IF lo_putaway->cart_is_empty(  ).
      /scwm/cl_rf_bll_srvc=>set_prmod( /scwm/cl_rf_bll_srvc=>c_prmod_background ).
      /scwm/cl_rf_bll_srvc=>set_fcode( 'BACKF' ).
    ELSE.
      lo_putaway->show_next_cart_position( CHANGING cs_cart_postion = zs_ptcrt ).
      lo_putaway->change_screen_layout( zs_ptcrt ).
    ENDIF.
  ELSE.
    MESSAGE ID ls_message-id TYPE ls_message-type NUMBER ls_message-number
    WITH ls_message-message_v1 ls_message-message_v2 ls_message-message_v3 ls_message-message_v4.
  ENDIF.
ENDFUNCTION.
