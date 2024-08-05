FUNCTION z_rf_pt_leave_tx.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"     REFERENCE(CS_ADMIN) TYPE  /SCWM/S_RF_ADMIN
*"     REFERENCE(CT_PTWY) TYPE  /SCWM/TT_RF_PTWY
*"     REFERENCE(CT_CREA_HU) TYPE  /SCWM/TT_RF_PTWY
*"----------------------------------------------------------------------

  BREAK-POINT ID /scwm/rf_putaway.

*--------------------------------------------------------------
* Data Definition
*--------------------------------------------------------------
  DATA: lv_fcode TYPE /scwm/de_fcode,
        lv_sev   TYPE bapi_mtype.                           "#EC NEEDED
  DATA: lt_hu_ident TYPE  /scwm/tt_hu_ident.

  CONSTANTS: lc_idart_i TYPE /scwm/de_huidart VALUE 'I'.
  FIELD-SYMBOLS <ptwy> TYPE /scwm/s_rf_ptwy_att.

*--------------------------------------------------------------
* Program Logic
*--------------------------------------------------------------

  CLEAR: cs_ptwy.

* collect all open WTs which are in HU
  LOOP AT ct_ptwy ASSIGNING <ptwy> WHERE processed = ' '.
    SELECT SINGLE FROM /scwm/ordim_o
           FIELDS sguid_hu
           WHERE lgnum EQ @/scwm/cl_tm=>sv_lgnum
             AND tanum EQ @<ptwy>-tanum
           INTO @DATA(lv_sguid_hu).
    IF <ptwy>-huident IS INITIAL.
      " Read alternative HU Identifier
      CALL FUNCTION '/SCWM/HU_SELECT_IDENT'
        EXPORTING
          iv_lgnum    = /scwm/cl_tm=>sv_lgnum
          it_guid_hu  = VALUE /scwm/tt_guid_hu( ( guid_hu = lv_sguid_hu ) )
        IMPORTING
          et_hu_ident = lt_hu_ident.
      READ TABLE lt_hu_ident INTO DATA(ls_ident_i)
           WITH KEY idart = lc_idart_i.
      IF sy-subrc EQ 0.
        <ptwy>-huident = ls_ident_i-huident.
      ELSE.
        <ptwy>-huident = <ptwy>-vlenr.
      ENDIF.
    ENDIF.
    APPEND <ptwy> TO ct_crea_hu.
  ENDLOOP.

  SORT ct_crea_hu BY huident.
  DELETE ADJACENT DUPLICATES FROM ct_crea_hu COMPARING huident.


*1) HU TOs for HUs?
*---------------------------------------------------
  IF ct_crea_hu IS NOT INITIAL.

*     call special function code to create squit HU-WTs
    lv_fcode = zif_rfui_c=>gs_fcode-huput.

  ENDIF.

*2) WHO split?
*---------------------------------------------------
  IF lv_fcode IS INITIAL.

    lv_fcode = zif_rfui_c=>gs_fcode-wosplt.

  ENDIF.

* set fcode
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
    EXPORTING
      iv_fcode = lv_fcode.

ENDFUNCTION.
