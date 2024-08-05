FUNCTION z_rf_sh_dl_hu_sel_pbo ##needed.
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
*&  RF UI - Load HU Step PBO
**********************************************************************
  zstr_rf_loading = zcs_loading.

  DATA(lo_loading) = NEW zcl_rf_loading( cs_admin_load-lgnum ).

  DATA(lt_param) = /scwm/cl_rf_bll_srvc=>get_screen_param( ).

  READ TABLE lt_param WITH KEY table_line = 'ZCS_LOADING'
    TRANSPORTING NO FIELDS.

  IF sy-subrc <> 0.
    CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
      EXPORTING
        iv_param_name = 'ZCS_LOADING'.
    CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
      EXPORTING
        iv_param_name = 'CS_LOAD'.
  ENDIF.

  IF zcs_loading-mix_sped_kep EQ abap_false.
    CALL FUNCTION '/SCWM/RF_SH_DL_HU_SEL_PBO'
      CHANGING
        cs_load       = cs_load                       " Dynpro-Struktur für Entladeprozess
        cs_admin_load = cs_admin_load                 " Administrative Struktur, um globale Variablen zu vermeiden
        ct_load       = ct_load.                       " Tabellentyp für RF-Dynpros beim Entladeprozess

    lo_loading->select_tu_deliveries(
      EXPORTING
        iv_lgnum         = cs_admin_load-lgnum
        iv_tu_num        = cs_load-tu_num
        iv_tu_sr_act_num = cs_load-tu_sr_act_num
        iv_huident_async = zcs_loading-huident_async
      IMPORTING
        ev_nof_loaded_hus = DATA(lv_nof_loaded_hus)
        ev_all_hus        = DATA(lv_all_hus)
    ).
    cs_admin_load-sumhu_proc = lv_nof_loaded_hus.
    cs_admin_load-sumhu = lv_all_hus.
  ELSE.
    CALL METHOD /scwm/cl_rf_bll_srvc=>get_screen_param
      RECEIVING
        rt_param = lt_param.

    READ TABLE lt_param WITH KEY table_line = 'CS_ADMIN_LOAD'
      TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.
      CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
        EXPORTING
          iv_param_name = 'CS_ADMIN_LOAD'.
    ENDIF.

    lo_loading->select_tu_deliveries(
      EXPORTING
        iv_lgnum         = cs_admin_load-lgnum
        iv_tu_num        = cs_load-tu_num
        iv_tu_sr_act_num = cs_load-tu_sr_act_num
        iv_huident_async = zcs_loading-huident_async
      IMPORTING
        ev_nof_loaded_hus = lv_nof_loaded_hus
        ev_all_hus        = lv_all_hus
    ).
    cs_admin_load-sumhu_proc = lv_nof_loaded_hus.
    IF zcs_loading-mix_sped_kep EQ abap_true.
      cs_admin_load-sumhu = lv_all_hus.
    ENDIF.

    IF cs_admin_load-sumhu_proc NE 0 AND cs_admin_load-sumhu_proc EQ cs_admin_load-sumhu
       AND zcs_loading-mix_sped_kep EQ abap_false.

      IF lo_loading->set_tu_load_complete( is_tu_sr_act_num = VALUE #( tu_num = cs_load-tu_num tu_sr_act_num = cs_load-tu_sr_act_num )
                                           iv_check_completness = abap_true ).
*   Set process mode to background
        /scwm/cl_rf_bll_srvc=>set_prmod(
                           /scwm/cl_rf_bll_srvc=>c_prmod_background ).
*   Set fcode
        /scwm/cl_rf_bll_srvc=>set_fcode(
                     /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans ).

*   dequeue all
        /scwm/cl_rf_bll_srvc=>set_flg_dequeue_all( ).
      ENDIF.
      RETURN.
    ENDIF.
  ENDIF.

  IF zcs_loading-mix_sped_kep EQ abap_true.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( '/SCWM/S_RF_ADMIN_LOAD-SUMHU' ).
  ELSE.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( '/SCWM/S_RF_ADMIN_LOAD-SUMHU' ).
  ENDIF.

  IF zcs_loading-mix_sped_kep EQ abap_false.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( 'ZSTR_RF_LOADING-MIX_SPED_KEP' ).
  ELSE.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( 'ZSTR_RF_LOADING-MIX_SPED_KEP' ).
  ENDIF.

ENDFUNCTION.
