FUNCTION z_rf_sh_sel_pbo ##needed.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_LOAD) TYPE  /SCWM/S_RF_LOAD
*"     REFERENCE(CS_ADMIN_LOAD) TYPE  /SCWM/S_RF_ADMIN_LOAD
*"     REFERENCE(CT_LOAD) TYPE  /SCWM/TT_RF_LOAD
*"     REFERENCE(CT_SHTUUS) TYPE  /SCWM/TT_RF_SHTUUS
*"     REFERENCE(ZCS_LOADING) TYPE  ZSTR_RF_LOADING
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-151223
*& Request No.   : GAP-091 – RF Loading
**********************************************************************
*& Description (short)
*&  RF UI - Fill screen fields
**********************************************************************
  zstr_rf_loading = zcs_loading.

  CALL FUNCTION '/SCWM/RF_SH_SEL_PBO'
    CHANGING
      cs_load       = cs_load                       " Dynpro-Struktur für Entladeprozess
      cs_admin_load = cs_admin_load                 " Materialnummer
      ct_load       = ct_load                       " Tabellentyp für RF-Dynpros beim Ladeprozess
      ct_shtuus     = ct_shtuus.                     " Tabellentyp für Entsiegelung einer TE
ENDFUNCTION.
