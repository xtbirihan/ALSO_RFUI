FUNCTION z_rf_pt_hu_dest_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PTWY) TYPE  /SCWM/S_RF_PTWY
*"     REFERENCE(CS_ADMIN) TYPE  /SCWM/S_RF_ADMIN
*"     REFERENCE(CT_PTWY) TYPE  /SCWM/TT_RF_PTWY
*"     REFERENCE(CT_LGPLA) TYPE  /SCWM/TT_LGPLA OPTIONAL
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF OPTIONAL
*"     REFERENCE(ZCS_PARTIAL_REPLENISHMENT) TYPE
*"        ZSTR_RF_PARTIAL_REPLENISHMENT
*"----------------------------------------------------------------------

  BREAK-POINT ID /scwm/rf_putaway.


* Introduce the parameters
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'CS_PTWY'.
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'ZCS_PARTIAL_REPLENISHMENT'.

  IF zcs_partial_replenishment-pw_header IS INITIAL.
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZCS_PARTIAL_REPLENISHMENT-PW_HEADER' ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
        iv_screlm_name = 'ZCS_PARTIAL_REPLENISHMENT-PW_HEADER' ).
  ELSE.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off(
        iv_screlm_name = 'ZCS_PARTIAL_REPLENISHMENT-PW_HEADER' ).
  ENDIF.
* Set Business Context for HU Destination
  MOVE  wmegc_execstep_04 TO cs_admin-exec_step.

* noch etwas unscharf, aber erstmal OK
  IF cs_ptwy-started_at IS INITIAL.
    GET TIME STAMP FIELD cs_ptwy-started_at.
  ENDIF.

  TRY.
      CALL FUNCTION '/SCWM/WHO_SELECT'
        EXPORTING
          iv_lgnum    = cs_admin-lgnum
          iv_who      = cs_ptwy-who
          iv_lock_who = 'X'.
    CATCH /scwm/cx_core.
  ENDTRY.

* Read texts (delivery/hazardous)
*  -give to framework
*  -set text indicator
  PERFORM hu_haz_dlv_txt_read IN PROGRAM /scwm/saplrf_putaway
     USING  cs_admin-lgnum
            cs_ptwy-vlenr
            cs_ptwy-hazmat
            cs_ptwy-rdoccat
            cs_ptwy-rdocid
            cs_ptwy-act_type
   CHANGING cs_ptwy-text_ind.

* Warehouse Number specific verification
*---------------------------------------------------
  PERFORM wme_verif_stru_fill IN PROGRAM /scwm/saplrf_putaway
          USING    cs_admin-lgnum
                   cs_ptwy
          CHANGING wme_verif.

* If NLPLA is initial -> turn input on and verification off
  IF cs_ptwy-nlpla IS INITIAL.
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    '/SCWM/S_RF_PTWY-NLPLA_VERIF' ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                    '/SCWM/S_RF_PTWY-NLPLA_VERIF' ).
    /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                    '/SCWM/S_RF_PTWY-NLPLA' ).
  ELSE.
    /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                    '/SCWM/S_RF_PTWY-NLPLA_VERIF' ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off(
                                    '/SCWM/S_RF_PTWY-NLPLA_VERIF' ).
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    '/SCWM/S_RF_PTWY-NLPLA' ).
  ENDIF.


* do not display Source HU, if
* Top HU (HUIDENT) = Source HU (VLENR)
*---------------------------------------------------
  IF cs_ptwy-huident = cs_ptwy-vlenr.
*     HU
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                              gc_scr_elmnt_vlenr ).
*     Verification
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                              gc_scr_elmnt_vlenr_vrf ).
  ENDIF.

* Open destination HU if customized
  DATA: lv_fld_ctrl_hu_dest TYPE /scwm/de_fld_ctrl_hu_dest_sty.
  DATA: ls_ordim_o TYPE /scwm/ordim_o.

  IF cs_ptwy-nlenr IS INITIAL.
    CALL FUNCTION '/SCWM/TO_READ_SINGLE'
      EXPORTING
        iv_lgnum         = cs_admin-lgnum
        iv_tanum         = cs_ptwy-tanum
        iv_flglock       = ' '
        iv_add_to_memory = ' '
        iv_read_from_db  = 'X'
      IMPORTING
        es_ordim_o       = ls_ordim_o
      EXCEPTIONS
        wrong_input      = 1
        not_found        = 2
        foreign_lock     = 3
        error            = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION '/SCWM/FLD_CTRL_HU_DEST_GET'
      EXPORTING
        iv_lgnum            = cs_admin-lgnum
        iv_lgtyp            = cs_ptwy-nltyp
        iv_entitled         = ls_ordim_o-entitled
        iv_matid            = cs_ptwy-matid
        iv_matnr            = cs_ptwy-matnr
        iv_read_prd         = 'X'
      IMPORTING
        ev_fld_ctrl_hu_dest = lv_fld_ctrl_hu_dest.

*   Verification field is always off
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    '/SCWM/S_RF_PTWY-NLENR_VERIF' ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                    '/SCWM/S_RF_PTWY-NLENR_VERIF' ).

    IF lv_fld_ctrl_hu_dest IS NOT INITIAL.   "HU_DEST open for input
      /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                    '/SCWM/S_RF_PTWY-NLENR' ).
    ELSE.
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    '/SCWM/S_RF_PTWY-NLENR' ).
    ENDIF.
  ENDIF.

  IF cs_admin-pos_manag = wmegc_rsrc_hupos_man OR
     cs_admin-pos_manag = wmegc_rsrc_hupos_sys.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                    '/SCWM/S_RF_PTWY-HUIDENT_VERIF' ).
    IF cs_ptwy-vlenr = cs_ptwy-nlenr.
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                      '/SCWM/S_RF_PTWY-NLENR_VERIF' ).
    ENDIF.
  ELSE. "no position management
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                    '/SCWM/S_RF_PTWY-HUIDENT_POS' ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                    '/SCWM/S_RF_PTWY-HUIDENT_POS_VERIF' ).
  ENDIF.

ENDFUNCTION.
