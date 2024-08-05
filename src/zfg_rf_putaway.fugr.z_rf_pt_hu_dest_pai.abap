FUNCTION z_rf_pt_hu_dest_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"     REFERENCE(CS_ADMIN) TYPE  /SCWM/S_RF_ADMIN
*"     REFERENCE(CT_PTWY) TYPE  /SCWM/TT_RF_PTWY
*"     REFERENCE(CT_LGPLA) TYPE  /SCWM/TT_LGPLA
*"     REFERENCE(CT_SERNR) TYPE  /SCWM/TT_RF_SERNR
*"     REFERENCE(CT_SERNR_DIFF) TYPE  /SCWM/TT_RF_SERNR
*"     REFERENCE(CS_SN) TYPE  /SCWM/S_RF_SN
*"     REFERENCE(TT_NESTED_HU) TYPE  /SCWM/TT_RF_NESTED_HU
*"     REFERENCE(ZCS_PARTIAL_REPLENISHMENT) TYPE
*"        ZSTR_RF_PARTIAL_REPLENISHMENT
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-190423
*& Request No.   : GAP-004 – Display Partial Replenisment
**********************************************************************
*& Description (short)
*& Continue with replenishment if replinshment is in process
*&
**********************************************************************


  CALL FUNCTION '/SCWM/RF_PT_HU_DEST_PAI'
    CHANGING
      cs_ptwy       = cs_ptwy                       " Dynpro-Struktur für Entladeprozess
      cs_admin      = cs_admin
      ct_ptwy       = ct_ptwy                       " Tabellentyp für RF-Dynpros beim Einlagerprozess
      ct_lgpla      = ct_lgpla                      " Tabelle von Lagerplätzen
      ct_sernr      = ct_sernr                      " Tabelle zur RF Serialnummern-Erfassung
      ct_sernr_diff = ct_sernr_diff                 " Tabelle zur RF Serialnummern-Erfassung
      cs_sn         = cs_sn                         " Struktur zur RF Serialnummern-Erfassung
      tt_nested_hu  = tt_nested_hu.                  " RF Nested HU

  "all the lines processes
  READ TABLE ct_ptwy WITH KEY processed = abap_false TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    IF zcs_partial_replenishment-ptwosl_call EQ abap_true.
      IF zcs_partial_replenishment-ptwosl_nof_calls EQ 0.
        zcs_partial_replenishment-ptwosl_call = abap_false.

        /scwm/cl_rf_bll_srvc=>set_fcode( /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans ).
        /scwm/cl_rf_bll_srvc=>set_ltrans( gc_ltrans_pysisg ).
      ENDIF.
      SUBTRACT 1 FROM zcs_partial_replenishment-ptwosl_nof_calls.
    ENDIF.
  ENDIF.

ENDFUNCTION.
