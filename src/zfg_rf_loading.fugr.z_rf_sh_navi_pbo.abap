FUNCTION z_rf_sh_navi_pbo ##needed.
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
  CONSTANTS c_fcode_goon        TYPE /scwm/de_fcode VALUE 'SYGOON'.


  IF zcs_loading-tsp_curr IS NOT INITIAL.
    DATA(lv_fcode_n) = c_fcode_goon. " SYGOON
    CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
      EXPORTING
        iv_fcode = lv_fcode_n.
    CALL METHOD /scwm/cl_rf_bll_srvc=>set_call_stack_optimizer
      EXPORTING
        iv_step = 'SHDLHU'.
    RETURN.
  ENDIF.


  CALL FUNCTION '/SCWM/RF_SH_NAVI_PBO'
    CHANGING
      cs_load       = cs_load                 " Dynpro-Struktur für Entladeprozess
      cs_admin_load = cs_admin_load                 " Administrative Struktur, um globale Variablen zu vermeiden
      ct_load       = ct_load.                 " Tabellentyp für RF-Dynpros beim Entladeprozess

ENDFUNCTION.
