FUNCTION z_rf_sh_dl_unl_hu_pai ##needed.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_LOAD) TYPE  /SCWM/S_RF_LOAD
*"     REFERENCE(CS_ADMIN_LOAD) TYPE  /SCWM/S_RF_ADMIN_LOAD
*"     REFERENCE(CT_LOAD) TYPE  /SCWM/TT_RF_LOAD
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF
*"     REFERENCE(ZCS_LOADING) TYPE  ZSTR_RF_LOADING
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-151223
*& Request No.   : GAP-091 – RF Loading
**********************************************************************
*& Description (short)
*&  RF UI - Unload HU Step PAI
**********************************************************************

  IF zcs_loading-huident_unload IS NOT INITIAL.
    DATA(lv_unload) = zcs_loading-huident_unload.
    CLEAR: zcs_loading-huident_unload, zcs_loading-huident_inp.

    DATA(lo_rf_loading) = NEW zcl_rf_loading( cs_admin_load-lgnum ).
    lo_rf_loading->get_hu_hdr(
      EXPORTING
        iv_lgnum   = cs_admin_load-lgnum                 " Warehouse Number/Warehouse Complex
        iv_huident = lv_unload                 " Handling Unit Identification
      IMPORTING
        es_huhdr    = DATA(ls_hu_hdr)                 " Handling unit header
        ev_unnested = DATA(lv_unnested)
        et_huhdr    = DATA(lt_huhdr)
    ).
    IF ls_hu_hdr IS INITIAL AND lt_huhdr IS INITIAL.
      MESSAGE e092(zmc_rfui) WITH lv_unload.
    ENDIF.

    IF lv_unnested EQ abap_false.
      IF ls_hu_hdr-loc_type NE wmegc_tu.
        MESSAGE e098(zmc_rfui) WITH lv_unload.
      ENDIF.
    ELSEIF lt_huhdr IS INITIAL OR lt_huhdr[ 1 ]-loc_type NE wmegc_tu.
      MESSAGE e098(zmc_rfui) WITH lv_unload.
    ENDIF.

    IF lv_unnested EQ abap_true.
      READ TABLE lt_huhdr INTO ls_hu_hdr INDEX 1.
    ENDIF.

    lo_rf_loading->select_hu_deliveries(
      EXPORTING
        iv_lgnum    = cs_admin_load-lgnum
        it_guid_hu  = VALUE #( ( guid_hu = ls_hu_hdr-guid_hu ) )
      IMPORTING
        et_prd_item = DATA(lt_prd_item)
    ).

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = cs_admin_load-lgnum                  " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zout_0001                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_procty_kep                " Parameter ID for process
      IMPORTING
        et_range  = DATA(lt_rng_procty_kep)                 " Parameter-Framework Low
    ).

    lo_rf_loading->reverse_loading_hu(
      EXPORTING
        is_tu_act             =  VALUE #( tu_num = cs_load-tu_num tu_sr_act_num = cs_load-tu_sr_act_num )                " Key for Aspect: TU
        iv_huident            =  COND #( WHEN lv_unnested = abap_false THEN ls_hu_hdr-huident
                                                                       ELSE |{ CONV /scwm/huident( lv_unload ) ALPHA = IN }| )             " Handling Unit Identification
        iv_unassign_hu        =  zcs_loading-mix_sped_kep
        iv_guid_hu            =  ls_hu_hdr-guid_hu
      IMPORTING
        ev_not_loaded         = DATA(lv_not_loaded)
        ev_wrong_tu           = DATA(lv_wrong_tu)
        ev_cannot_be_unloaded = DATA(lv_cannot_be_unloaded)
        ev_cannot_unassign    = DATA(lv_cannot_unassign)
    ).
    IF lv_not_loaded EQ abap_true.
      MESSAGE e093(zmc_rfui) WITH lv_unload.
    ENDIF.
    IF lv_wrong_tu EQ abap_true.
      MESSAGE e094(zmc_rfui) WITH lv_unload.
    ENDIF.
    IF lv_cannot_be_unloaded EQ abap_true.
      MESSAGE e095(zmc_rfui) WITH lv_unload.
    ENDIF.
    IF lv_cannot_unassign EQ abap_true.
      MESSAGE e095(zmc_rfui) WITH lv_unload.
    ENDIF.
    IF cs_admin_load-sumhu_proc GT 0.
      SUBTRACT 1 FROM cs_admin_load-sumhu_proc.
    ENDIF.
    IF zcs_loading-mix_sped_kep EQ abap_true AND zcs_loading-mix_sped_kep EQ abap_true AND cs_admin_load-sumhu GT 0.
      SUBTRACT 1 FROM cs_admin_load-sumhu.
    ENDIF.
  ENDIF.
  IF cs_load-sumhu_proc EQ 0.
    /scwm/cl_rf_bll_srvc=>set_fcode( /scwm/cl_rf_bll_srvc=>c_fcode_backf ).
  ELSE.
    /scwm/cl_rf_bll_srvc=>set_fcode( 'PBO1' ).
  ENDIF.
ENDFUNCTION.
