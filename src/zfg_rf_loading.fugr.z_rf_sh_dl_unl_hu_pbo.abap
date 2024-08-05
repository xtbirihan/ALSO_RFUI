FUNCTION Z_RF_SH_DL_UNL_HU_PBO ##needed.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_LOAD) TYPE  /SCWM/S_RF_LOAD
*"     REFERENCE(CS_ADMIN_LOAD) TYPE  /SCWM/S_RF_ADMIN_LOAD
*"     REFERENCE(CT_LOAD) TYPE  /SCWM/TT_RF_LOAD
*"     REFERENCE(ZCS_LOADING) TYPE  ZSTR_RF_LOADING
*"--------------------------------------------------------------------
**********************************************************************
*& Key           : LH-151223
*& Request No.   : GAP-091 – RF Loading
**********************************************************************
*& Description (short)
*&  RF UI - Unload HU Step PBO
**********************************************************************

  zstr_rf_loading = zcs_loading.
  CLEAR zstr_rf_loading-huident_inp.

  data(lt_param) = /scwm/cl_rf_bll_srvc=>get_screen_param( ).

  READ TABLE lt_param WITH KEY table_line = 'ZCS_LOADING'
    TRANSPORTING NO FIELDS.

  IF sy-subrc <> 0.
    CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
      EXPORTING
        iv_param_name = 'ZCS_LOADING'.
  ENDIF.

ENDFUNCTION.
