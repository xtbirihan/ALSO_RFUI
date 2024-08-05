FUNCTION z_rf_pick_pichbn_pbo .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_RF_INP_HELP_FOR_BARC) TYPE
*"        ZSTR_RF_INP_HELP_FOR_BARC
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-170323
*& Request No.   : GAP-070 – Internal Input Help In RF For Barcode
**********************************************************************
*& Description (short)
*& For specific storage types the verifiaction code identifies a set
*& of bins. Therefore the identification code is not sufficient, an
*& aditional field is needed to identify the level.
**********************************************************************
  DATA: lv_line TYPE i,
        lv_step TYPE /scwm/de_step.
  DATA: lv_msg        TYPE string,
        lv_data_entry TYPE /scwm/de_data_entry,
        ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv.

  BREAK-POINT ID /scwm/rf_picking.

* Initiate screen parameter
  /scwm/cl_rf_bll_srvc=>init_screen_param( ).

* Set screen parameter
  /scwm/cl_rf_bll_srvc=>set_screen_param('TT_ORDIM_CONFIRM').
  /scwm/cl_rf_bll_srvc=>set_screen_param('ORDIM_CONFIRM').
  /scwm/cl_rf_bll_srvc=>set_screen_param('ZCS_RF_INP_HELP_FOR_BARC').

* Open destination bin verification input field
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                      gc_scr_elmnt_nlpla_vrf ).
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                  gc_scr_elmnt_zstbin_level ).
  lv_line = /scwm/cl_rf_bll_srvc=>get_line( ).
  IF lv_line = 0.
    lv_line = 1.
    /scwm/cl_rf_bll_srvc=>set_line( lv_line ).
  ENDIF.
  READ TABLE tt_ordim_confirm INDEX lv_line INTO ordim_confirm.
  CLEAR ordim_confirm-nlpla_verif.
  CLEAR ordim_confirm-bprop.
  ordim_confirm-nlpla_o = ordim_confirm-nlpla.
  MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.

  gv_stbin_level_active = abap_false.
  clear zcs_rf_inp_help_for_barc-stbin_level.

* Turn off material display for HUTO.
  IF NOT ordim_confirm-flghuto IS INITIAL.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                     gc_scr_elmnt_matnr ).
  ENDIF.

  lv_step = /scwm/cl_rf_bll_srvc=>get_step( ).

  IF lv_step = step_pick_chbd.
* additional data reading in the blocked bin part
    PERFORM add_data_read IN PROGRAM /scwm/saplrf_picking
    CHANGING
      ordim_confirm
      tt_ordim_confirm.
  ENDIF.

  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                  '/SCWM/S_RF_ORDIM_CONFIRM-RFHU' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                  '/SCWM/S_RF_ORDIM_CONFIRM-RFHU' ).


* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry = wmegc_data_entry_voice.
*   Set prompt and help text

*   Enter new destination or ask system for proposal via command
    MESSAGE i586(/scwm/rf_en) INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'LOCATION' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'LOCATION' iv_sf = 'HELP_1' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).
    ls_screlm_pbv-fld_prompt_1 = lv_msg.
    ls_screlm_pbv-help_1 = lv_msg.

    MESSAGE i705(/scwm/rf_en) INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'LOCATION' iv_sf = 'FILLED_1' iv_pt = wmegc_pt_filled iv_use_symsg = 'X' ).
    ls_screlm_pbv-filled_1 = lv_msg.

    ls_screlm_pbv-grammar_lnk_1 = 'EWM_ALPHANUMERIC.JSGF'.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'LOCATION' iv_sf = 'GRAMMAR_LNK_1' iv_pt = wmegc_pt_grammarlnk ).

    /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).
  ENDIF.

  " EWM 9.40 Enhancements
  "---------------------------------------------------------------
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
  "---------------------------------------------------------------

ENDFUNCTION.
