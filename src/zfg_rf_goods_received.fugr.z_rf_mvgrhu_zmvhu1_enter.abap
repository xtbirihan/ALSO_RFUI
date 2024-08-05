FUNCTION z_rf_mvgrhu_zmvhu1_enter.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_MOVHU) TYPE  ZSTR_RF_MOVE_DUMMY_HU
*"----------------------------------------------------------------------
  BREAK-POINT ID zcg_rfmovehu.

  /scwm/cl_rf_bll_srvc=>set_prmod( '2' ).

  IF zs_movhu-huident IS INITIAL.
    RETURN.
  ENDIF.

  DATA(lo_move_hu_helper) = zcl_rf_move_gr_dummy_hu=>get_instance( ).

  IF lo_move_hu_helper->is_handlung_unit_valid( zs_movhu-huident ) = abap_false.
    DATA(lv_huident) = zs_movhu-huident.
    CLEAR zs_movhu-huident.
    MESSAGE e113(zmc_rfui) WITH lv_huident.
* Handling unit &1 is invalid
    RETURN.
  ENDIF.

  /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZSTR_RF_MOVE_DUMMY_HU-HUIDENT' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on( 'ZSTR_RF_MOVE_DUMMY_HU-DESTBIN' ).
  /scwm/cl_rf_bll_srvc=>set_field( 'ZSTR_RF_MOVE_DUMMY_HU-DESTBIN' ).

  IF zs_movhu-destbin IS INITIAL.
    RETURN.
  ENDIF.

  IF lo_move_hu_helper->is_storage_bin_valid( iv_warehousenumber = zs_movhu-lgnum
                                              iv_dest_storage_bin = zs_movhu-destbin ) = abap_false.
    DATA(lv_destbin) = zs_movhu-destbin.
    CLEAR zs_movhu-destbin.
    MESSAGE e210(/scwm/rf_de) WITH lv_destbin.
* Destination storage bin &1 is invalid
    RETURN.
  ENDIF.

  lo_move_hu_helper->move_gr_dummy_hu(
      EXPORTING
        iv_warehousenumber = zs_movhu-lgnum
      CHANGING
        cv_handling_unit = zs_movhu-huident
        cv_dest_storage_bin = zs_movhu-destbin ).

  IF lo_move_hu_helper->error_orccurred( ) = abap_true.
    DATA(ls_message) = lo_move_hu_helper->get_last_error_message(  ).
    MESSAGE ls_message-message TYPE ls_message-type.
  ENDIF.

  /scwm/cl_rf_bll_srvc=>set_prmod( '1' ).
  /scwm/cl_rf_bll_srvc=>set_fcode( 'INIT' ).

ENDFUNCTION.
