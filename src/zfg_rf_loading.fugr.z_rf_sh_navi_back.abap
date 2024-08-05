FUNCTION z_rf_sh_navi_back ##needed.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_LOAD) TYPE  /SCWM/S_RF_LOAD
*"     REFERENCE(CS_ADMIN_LOAD) TYPE  /SCWM/S_RF_ADMIN_LOAD
*"     REFERENCE(CT_LOAD) TYPE  /SCWM/TT_RF_LOAD
*"     REFERENCE(ZCS_LOADING) TYPE  ZSTR_RF_LOADING
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-151223
*& Request No.   : GAP-091 – RF Loading
**********************************************************************
*& Description (short)
*&  RF UI - Navigation
**********************************************************************
  DATA(lv_step)    = /scwm/cl_rf_bll_srvc=>get_step( ).
  IF zcs_loading-tsp_curr is NOT INITIAL.
    IF lv_step EQ 'SHTUSL'.
      "     Set process mode to background
      /scwm/cl_rf_bll_srvc=>set_prmod(
                         /scwm/cl_rf_bll_srvc=>c_prmod_background ).
      "     Set fcode
      /scwm/cl_rf_bll_srvc=>set_fcode(
                   /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans ).

      "     dequeue all
      /scwm/cl_rf_bll_srvc=>set_flg_dequeue_all( ).
    ELSE.
      /scwm/cl_rf_bll_srvc=>set_prmod(
                           /scwm/cl_rf_bll_srvc=>c_prmod_background ).
      /scwm/cl_rf_bll_srvc=>set_fcode( 'ZPREST' ).
    ENDIF.
    RETURN.
  ENDIF.

  CALL FUNCTION '/SCWM/RF_SH_NAVI_BACK'
    CHANGING
      cs_load       = cs_load                       " Dynpro-Struktur für Ladeprozess
      cs_admin_load = cs_admin_load                 " Administrative Struktur, um globale Variablen zu vermeiden
      ct_load       = ct_load.                       " Tabellentyp für RF-Dynpros beim Ladeprozess
ENDFUNCTION.
