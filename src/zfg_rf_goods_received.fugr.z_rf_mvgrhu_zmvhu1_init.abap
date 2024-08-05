FUNCTION z_rf_mvgrhu_zmvhu1_init.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_MOVHU) TYPE  ZSTR_RF_MOVE_DUMMY_HU
*"----------------------------------------------------------------------


  DATA: ls_resource_values TYPE /scwm/rsrc.

  BREAK-POINT ID zcg_rfmovehu.

  CLEAR: zs_movhu.

  zs_movhu-header = 'Move Dummy HU'(002).

  /scwm/cl_rf_bll_srvc=>init_screen_param( ).
  /scwm/cl_rf_bll_srvc=>set_screen_param( 'ZS_MOVHU' ).

  " Get warehouse number for user(ressource)
  CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
    EXPORTING
      iv_uname = sy-uname
    CHANGING
      cs_rsrc  = ls_resource_values.

  IF ls_resource_values-lgnum IS INITIAL.
    MESSAGE e010(/scwm/rf_de).
* Initialization for warehouse number &1 failed
  ENDIF.

  DATA(lo_move_hu_helper) = zcl_rf_move_gr_dummy_hu=>get_instance( ).
  IF lo_move_hu_helper->is_warehousenumber_valid( ls_resource_values-lgnum ) = abap_false.
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZSTR_RF_MOVE_DUMMY_HU-HUIDENT' ).
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZSTR_RF_MOVE_DUMMY_HU-DESTBIN' ).
    MESSAGE e041(/scwm/rf_de) WITH ls_resource_values-lgnum.
* Initialization for warehouse number &1 failed
    RETURN.
  ENDIF.

  IF lo_move_hu_helper->is_warehouse_processtype_valid( ls_resource_values-lgnum ) = abap_false.
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZSTR_RF_MOVE_DUMMY_HU-HUIDENT' ).
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZSTR_RF_MOVE_DUMMY_HU-DESTBIN' ).
    MESSAGE e142(/scwm/rf_de).
* Enter a warehouse process type
    RETURN.
  ENDIF.

  zs_movhu-lgnum = ls_resource_values-lgnum.
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on( 'ZSTR_RF_MOVE_DUMMY_HU-HUIDENT' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZSTR_RF_MOVE_DUMMY_HU-DESTBIN' ).
  /scwm/cl_rf_bll_srvc=>set_field( 'ZSTR_RF_MOVE_DUMMY_HU-HUIDENT' ).
ENDFUNCTION.
