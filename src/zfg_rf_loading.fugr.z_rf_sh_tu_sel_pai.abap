FUNCTION z_rf_sh_tu_sel_pai.
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
*&  RF UI - Enter Door/TU
**********************************************************************

  FIELD-SYMBOLS <lv_tu_num> TYPE /scwm/de_tu_num.
  CONSTANTS: c_msgid_no_hus_for_tu TYPE sy-msgid VALUE '/SCWM/RF_DE',
             c_msgno_no_hus_for_tu TYPE sy-msgid VALUE '227'.

  cs_load-doccat          = wmegc_doccat_pdo.

  DATA(lo_rf_loading) = NEW zcl_rf_loading( cs_admin_load-lgnum ).
  lo_rf_loading->reset_buffer( ).
  TRY.
      /scwm/cl_sr_config=>read_door_single(
        EXPORTING
          iv_lgnum    = cs_admin_load-lgnum
          iv_door     = CONV #( cs_load-id_tu_door )
        IMPORTING
          es_tdoor    = DATA(ls_tdoor) ).
    CATCH /scwm/cx_sr_error ##NO_HANDLER.
  ENDTRY.

  IF ls_tdoor IS INITIAL.
    MESSAGE e084(zmc_rfui).
  ENDIF.

  cs_load-door = ls_tdoor-door.

  "Read active TU -----------------------------------------------------
  /scwm/cl_sr_db_tu=>read_active_activity_for_rf(
    EXPORTING
      iv_lgnum       = cs_admin_load-lgnum
      iv_doccat      = cs_load-doccat
      iv_tu_or_door  = cs_load-id_tu_door
      iv_scac_or_tsp = cs_load-tsp_scac
    IMPORTING
      es_tu_act_key  = DATA(ls_tu_act_key) ).

  IF ls_tu_act_key IS INITIAL.
    MESSAGE e331(/scwm/rf_de) WITH cs_load-id_tu_door.
  ENDIF.

  "Get carrier
  SELECT SINGLE FROM /scwm/tu_sr_act
         FIELDS tsp_curr
         WHERE tu_num        EQ @ls_tu_act_key-tu_num
           AND tu_sr_act_num EQ @ls_tu_act_key-tu_sr_act_num
         INTO @zcs_loading-tsp_curr.

  "Get MIX flag
  SELECT SINGLE FROM /scwm/tunit
         FIELDS zz_mix_sped_kep
         WHERE tu_num        EQ @ls_tu_act_key-tu_num
         INTO @zcs_loading-mix_sped_kep.

  lo_rf_loading->select_tu_deliveries(
    EXPORTING
      iv_lgnum         = cs_admin_load-lgnum
      iv_tu_num        = ls_tu_act_key-tu_num
      iv_tu_sr_act_num = ls_tu_act_key-tu_sr_act_num
      iv_huident_async = zcs_loading-huident_async
    IMPORTING
      et_prd_hdr       = DATA(lt_prd_hdr)
      et_prd_item      = DATA(lt_prd_item) ##needed
      et_hu_prd        = DATA(lt_hu_prd)
      ev_nof_loaded_hus = DATA(lv_nof_loaded_hus)
  ).

  zcl_param=>get_parameter(
    EXPORTING
      iv_lgnum     = cs_admin_load-lgnum                  " Warehouse Number/Warehouse Complex
      iv_process   = zif_param_const=>c_zrfui_0001                 " Process ID (Specification, Program, BAdI etc.)
      iv_parameter = zif_param_const=>c_procty_sped                " Parameter ID for process
    IMPORTING
      et_range  = DATA(lt_rng_procty_sped)                 " Parameter-Framework Low
  ).


  "We need this cleanup because the used fm /SCWM/HUMAIN_REFRESH in
  "form SELECT_NOT_LOADED_HUS clears only a part of the memory.
  "After that fm /SCWM/GET_HU_BY_TU_VEH returns no delivery references
  CALL FUNCTION '/SCWM/HU_PACKING_SERVICE_INIT'.

  IF zcs_loading-mix_sped_kep EQ abap_false.
    CALL FUNCTION '/SCWM/RF_SH_TU_SEL_PAI'
      CHANGING
        cs_load       = cs_load                 " Dynpro-Struktur für Entladeprozess
        cs_admin_load = cs_admin_load                 " Administrative Struktur, um globale Variablen zu vermeiden
        ct_load       = ct_load.
  ENDIF.
  IF zcs_loading-mix_sped_kep EQ abap_true.
    "Get BO for TU and check logistic process
    TRY.
        DATA(lo_bom) = /scwm/cl_sr_bom=>get_instance( ).
        DATA(lo_bo_tu) = lo_bom->get_bo_tu_by_key( ls_tu_act_key ).
        lo_bo_tu->get_data(
          IMPORTING
            es_bo_tu_data = DATA(ls_bo_tu_data) ).
        "Transit warehouse TU cannot be handled by this RF transaction
        IF ls_bo_tu_data-log_process = /scwm/if_dl_ind_c=>sc_logproc_tw.
          MESSAGE e664(/scwm/shp_rcv) WITH ls_bo_tu_data-tu_num_ext.
        ENDIF.
      CATCH /scwm/cx_sr_error.
        MESSAGE e331(/scwm/rf_de) WITH cs_load-id_tu_door.
    ENDTRY.

    "Set TU fields in RF working structure ------------------------------
    cs_load-tu_num        = ls_bo_tu_data-tu_num.
    cs_load-tu_sr_act_num = ls_bo_tu_data-tu_sr_act_num.
    cs_load-tu_num_ext    = ls_bo_tu_data-tu_num_ext.
    cs_load-tsp_scac      = ls_bo_tu_data-tsp_curr.
    cs_load-scac          = ls_bo_tu_data-scac_curr.

    "Check TU status --------------------------------------
    TRY.
        "If TU is already posted GI -> loading is not allowed anymore
        "(status management would only issue a warning message)
        IF lo_bo_tu->get_status_by_id( wmesr_status_goods_issue ) = abap_true.
          MESSAGE e667(/scwm/shp_rcv) WITH ls_bo_tu_data-tu_num_ext.
        ENDIF.

      CATCH /scwm/cx_sr_error.
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        /scwm/cl_tm=>cleanup( ).
        MESSAGE e207(/scwm/rf_de).

    ENDTRY.
    ASSIGN ('(/SCWM/SAPLRF_LOADING)GV_TU_NUM') TO <lv_tu_num>.
    IF sy-subrc EQ 0.
      <lv_tu_num> = cs_load-tu_num.
    ENDIF.
    PERFORM set_tu_load_begin IN PROGRAM /scwm/saplrf_loading
       USING
         cs_admin_load-lgnum
         cs_load-tu_num_ext
         cs_load-tsp_scac.

    cs_admin_load-sumhu_proc = lv_nof_loaded_hus.

  ENDIF.
ENDFUNCTION.
