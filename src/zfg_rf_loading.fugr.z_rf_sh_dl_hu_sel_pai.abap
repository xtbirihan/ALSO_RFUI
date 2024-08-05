FUNCTION z_rf_sh_dl_hu_sel_pai.
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
*&  RF UI - Load HU Step PAI
**********************************************************************
  DATA: lt_bapiret  TYPE bapirettab ##needed,
        lv_severity TYPE bapi_mtype,
        lt_delivery TYPE  /scwm/tt_aspk_tu_open_whr.

  FIELD-SYMBOLS <lt_hu_seq> TYPE /scwm/tt_hu_load_seq.
  CONSTANTS:
    c_fcode_close_hu     TYPE /scwm/de_fcode VALUE 'ZCOTU',
    lc_fcode_immediately TYPE /scwm/de_fcode VALUE 'SYIMED'.

  DATA(lo_loading) = NEW zcl_rf_loading( cs_admin_load-lgnum ).

  DATA(lv_huident) = zcs_loading-huident_inp.
  CLEAR zcs_loading-huident_inp.

  IF /scwm/cl_rf_bll_srvc=>get_fcode( ) EQ c_fcode_close_hu.
    IF lo_loading->set_tu_load_complete( is_tu_sr_act_num = VALUE #( tu_num = cs_load-tu_num tu_sr_act_num = cs_load-tu_sr_act_num )
                                         iv_check_completness = xsdbool( zcs_loading-mix_sped_kep EQ abap_false ) ).
*     Set process mode to background
      /scwm/cl_rf_bll_srvc=>set_prmod(
                         /scwm/cl_rf_bll_srvc=>c_prmod_background ).
*     Set fcode
      /scwm/cl_rf_bll_srvc=>set_fcode(
                   /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans ).
    ELSE.
      MESSAGE e097(zmc_rfui) WITH cs_load-tu_num_ext.
    ENDIF.
    RETURN.
  ENDIF.



  lo_loading->get_hu_hdr(
    EXPORTING
      iv_lgnum   = cs_admin_load-lgnum                 " Warehouse Number/Warehouse Complex
      iv_huident = lv_huident                  " Handling Unit Identification
    IMPORTING
      es_huhdr   = DATA(ls_huhdr)                 " Handling unit header
      et_huref   = DATA(lt_huref)
      et_huhdr   = DATA(lt_huhdr)
      ev_unnested = DATA(lv_unnested)
  ).

  DATA(lv_nof_dlv) =  lines( VALUE /scwm/tt_rdocid( FOR GROUPS ref_docid OF ref IN lt_huref
                                                    GROUP BY ( docat = ref-doccat docid = ref-docid ) WITHOUT MEMBERS
                                                   ( ref-docid ) ) ).

  IF ls_huhdr IS INITIAL AND lt_huhdr IS INITIAL.
    MESSAGE e014(/scwm/rf_de) WITH lv_huident.
  ENDIF.

  IF ls_huhdr-loc_type EQ wmegc_tu OR
     lt_huhdr IS NOT INITIAL AND lv_unnested EQ abap_true AND lt_huhdr[ 1 ]-loc_type EQ wmegc_tu.
    MESSAGE e099(zmc_rfui) WITH lv_huident.
  ENDIF.

  lo_loading->select_hu_deliveries(
    EXPORTING
      iv_lgnum    = cs_admin_load-lgnum
      it_guid_hu  = COND #( WHEN lv_unnested EQ abap_false THEN VALUE #( ( guid_hu = ls_huhdr-guid_hu ) )
                            ELSE VALUE #( FOR huhdr IN lt_huhdr ( guid_hu = huhdr-guid_hu ) )
                           )
    IMPORTING
      et_prd_item = DATA(lt_prd_item)
      et_prd_hdr  = DATA(lt_prd_hdr)
  ).
  IF lt_prd_item IS INITIAL.
    MESSAGE e110(zmc_rfui) WITH lv_huident.
  ENDIF.

  zcl_param=>get_parameter(
    EXPORTING
      iv_lgnum     = cs_admin_load-lgnum                  " Warehouse Number/Warehouse Complex
      iv_process   = zif_param_const=>c_zrfui_0001                 " Process ID (Specification, Program, BAdI etc.)
      iv_parameter = zif_param_const=>c_procty_sped                " Parameter ID for process
    IMPORTING
      et_range  = DATA(lt_rng_procty_sped)                 " Parameter-Framework Low
  ).

  zcl_param=>get_parameter(
    EXPORTING
      iv_lgnum     = cs_admin_load-lgnum                  " Warehouse Number/Warehouse Complex
      iv_process   = zif_param_const=>c_zout_0001                 " Process ID (Specification, Program, BAdI etc.)
      iv_parameter = zif_param_const=>c_procty_kep                " Parameter ID for process
    IMPORTING
      et_range  = DATA(lt_rng_procty_kep)                 " Parameter-Framework Low
  ).


  IF lv_unnested EQ abap_true. "Only one of the subitems must be checked since the item in the TOP HU moves together
    READ TABLE lt_huhdr INTO ls_huhdr INDEX 1.
  ENDIF.
  cs_load-rfhu = ls_huhdr-huident.

  lo_loading->get_tu_for_hu(
    EXPORTING iv_guid_hu = ls_huhdr-guid_hu
    IMPORTING et_tu_act_key = DATA(lt_tu_act_for_hu) ).

  IF lt_tu_act_for_hu IS NOT INITIAL AND NOT line_exists( lt_tu_act_for_hu[ tu_num = cs_load-tu_num ] ).
    MESSAGE e090(zmc_rfui) WITH cs_load-rfhu.
  ENDIF.

  IF lt_tu_act_for_hu IS INITIAL AND zcs_loading-mix_sped_kep EQ abap_true.
    IF NOT lo_loading->assign_hu_to_tu(
        it_tu_key = VALUE #( ( tu_num = cs_load-tu_num tu_sr_act_num = cs_load-tu_sr_act_num ) )                 " Table for Key Aspect: Transportation Unit
        it_hu_key = COND #( WHEN lv_unnested EQ abap_true
                            THEN VALUE #( ( huident = ls_huhdr-huident guid_hu = ls_huhdr-guid_hu lgnum = cs_admin_load-lgnum ) )
                            ELSE VALUE #( FOR huhdr IN lt_huhdr ( huident = huhdr-huident guid_hu = huhdr-guid_hu lgnum = cs_admin_load-lgnum )  )
                            )               " Table Type: Key Aspect for TU-HU Assignment
       ).
      MESSAGE e091(zmc_rfui) WITH lv_huident cs_load-tu_num_ext.
    ENDIF.
  ENDIF.


  cs_load-huident = ls_huhdr-huident.
  IF lv_unnested EQ abap_false.
    DATA(ls_open_wt) = lo_loading->get_open_wt_for_hu(
        iv_lgnum   = cs_admin_load-lgnum                 " Warehouse Number/Warehouse Complex
        iv_huident = cs_load-huident                 " Handling Unit Identification
    ).

    "Check, if there is open WT staging task, if ther is one then confirm it
    IF ls_open_wt-procs IS NOT INITIAL.
      SELECT SINGLE FROM /scwm/tprocs FIELDS iproc
       WHERE procs EQ @ls_open_wt-procs
        INTO @DATA(lv_iproc).

      IF lv_iproc EQ zif_wme_c=>gs_iproc-stag.
        DATA(lt_conf) = VALUE /scwm/to_conf_tt( ( tanum = ls_open_wt-tanum ) ).

        CALL FUNCTION '/SCWM/TO_CONFIRM'
          EXPORTING
            iv_lgnum       = cs_admin_load-lgnum
            it_conf        = lt_conf
            iv_update_task = abap_false
            iv_commit_work = abap_false
          IMPORTING
            et_bapiret     = lt_bapiret.
        IF lv_severity CA wmegc_severity_ea.
          ROLLBACK WORK.
        ELSE.
          COMMIT WORK AND WAIT.
          DATA(lv_stag_conf) = abap_true.
        ENDIF.
        /scwm/cl_tm=>cleanup( ).
      ENDIF.
    ENDIF.
  ENDIF.
  IF lv_stag_conf EQ abap_false
     AND zcl_crud_scwm_t331=>get_sttype_ctrl_by_sttype( iv_lgnum = cs_admin_load-lgnum iv_lgtyp =  ls_huhdr-lgtyp )-st_role NE wmegc_strole_stgarea.
    MESSAGE e096(zmc_rfui) WITH lv_huident.
  ENDIF.

  "Call the relevant part of the original original function /SCWM/RF_SH_DL_HU_SEL_PAI


  ASSIGN ('(/SCWM/SAPLRF_LOADING)GT_HU_SEQ') TO <lt_hu_seq>.

  PERFORM hu_read_and_lock IN PROGRAM /scwm/saplrf_loading
    USING
      cs_admin_load
    CHANGING
      ls_huhdr
      cs_load
      ct_load.

*----------------------------------------------------------------------
*- Check if HU already loaded
*----------------------------------------------------------------------
  CALL FUNCTION 'CRM_STATUS_CHECK'
    EXPORTING
      objnr             = ls_huhdr-guid_hu
      status            = wmegc_hustat_loaded
    EXCEPTIONS
      object_not_found  = 1
      status_not_active = 2
      OTHERS            = 3.
  IF sy-subrc = 0.
    CLEAR cs_load-huident.
    READ TABLE <lt_hu_seq> INTO DATA(ls_hu_seq)
         WITH KEY huident = ls_huhdr-huident.
    IF ls_hu_seq-fcode_exc <> 'SHRVLD'.
      MESSAGE e081(/scwm/rf_de) WITH ls_huhdr-huident.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------------
*- Check if HU can be loaded:
*   if the delivery would become blocked because of the loading
*   (loading links on both main and subitems)
*   than we should prevent the loading on RFUI.
*----------------------------------------------------------------------
  PERFORM dlv_hu_load_check IN PROGRAM /scwm/saplrf_loading
    USING cs_admin_load-lgnum
          ls_huhdr-huident.

  "This the "perform to_for_hu_read" copy from the  function module
  "/SCWM/RF_SH_DL_HU_SEL_PAI. It decides syncron/asyncron
  "call of the loading and performs loading
  IF lines( lt_huhdr ) GT 1 AND zcs_loading-mix_sped_kep EQ abap_true
     AND lv_unnested EQ abap_false.

    DATA(lv_unnest) = abap_true.
    zcs_loading-huident_async = ls_huhdr-huident.

  ENDIF.

  lo_loading->to_for_hu_read(
    EXPORTING
      iv_iproc         = wmegc_iproc_load
      iv_tu_num        = cs_load-tu_num                 " Transportation Unit for Loading
      iv_read_only     = abap_false
      iv_skip_tu_check = abap_false
      iv_door_bin      = cs_admin_load-door_bin                 " Storage Bin
      iv_lgnum         = cs_admin_load-lgnum                 " Warehouse Number/Warehouse Complex
      iv_unnest        = lv_unnest
      iv_unnested      = lv_unnested
    CHANGING
      cv_huident       = lv_huident
      cv_tanum         = cs_load-tanum
      cv_nlpla         = cs_load-nlpla
      cv_pt_load       = cs_admin_load-pt_load
  ).

*     Updating HU for loaded
  IF lv_unnested EQ abap_false.
    READ TABLE <lt_hu_seq> INTO ls_hu_seq WITH KEY
                          huident = ls_huhdr-huident.
    IF sy-subrc IS INITIAL.
      ls_hu_seq-status_load = abap_true.
      MODIFY <lt_hu_seq> INDEX sy-tabix FROM ls_hu_seq.
    ENDIF.
  ELSE.
    LOOP AT lt_huhdr INTO DATA(ls_huhdr_denest).
      READ TABLE <lt_hu_seq> INTO ls_hu_seq WITH KEY
                            huident = ls_huhdr_denest-huident.
      IF sy-subrc IS INITIAL.
        ls_hu_seq-status_load = abap_true.
        MODIFY <lt_hu_seq> INDEX sy-tabix FROM ls_hu_seq.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DATA(lv_fcode) = lc_fcode_immediately.
  CLEAR: cs_load-tanum. " WT already confirmed

  lo_loading->delivery_status_set(
      it_huident    = COND #( WHEN lv_unnested EQ abap_false THEN VALUE #( (  ls_huhdr-huident ) )
                              ELSE VALUE #( FOR huhdr IN lt_huhdr ( huhdr-huident ) )
                             )
      iv_status     = /scwm/if_ui_shp_const=>sc_act_status_load_start
      is_admin_load = cs_admin_load
    ).

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
    EXPORTING
      iv_fcode = lv_fcode.
ENDFUNCTION.
