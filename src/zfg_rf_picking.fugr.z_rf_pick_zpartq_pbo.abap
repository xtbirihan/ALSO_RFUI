FUNCTION z_rf_pick_zpartq_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF
*"     REFERENCE(ZCS_PARTIAL_REPLENISHMENT) TYPE
*"        ZSTR_RF_PARTIAL_REPLENISHMENT
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-190423
*& Request No.   : GAP-004 – Display Partial Replenisment
**********************************************************************
*& Description (short)
*& Partial replenishment initialization
*&
*&
**********************************************************************
  DATA: lv_dest        LIKE ordim_confirm-nlpla,
        lv_unique_dest TYPE abap_bool.

  BREAK-POINT ID /scwm/rf_picking.

* Initiate screen parameter
  /scwm/cl_rf_bll_srvc=>init_screen_param( ).
* Set screen parameter
  /scwm/cl_rf_bll_srvc=>set_screen_param('TT_ORDIM_CONFIRM').
  /scwm/cl_rf_bll_srvc=>set_screen_param('ORDIM_CONFIRM').
  /scwm/cl_rf_bll_srvc=>set_screen_param('ZCS_PARTIAL_REPLENISHMENT').

* Get actual fcode
  DATA(lv_fcode) = /scwm/cl_rf_bll_srvc=>get_fcode( ).

* Set the processing mode to foreground
  /scwm/cl_rf_bll_srvc=>set_prmod(
                        /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).
* get current line.
  DATA(lv_line) = /scwm/cl_rf_bll_srvc=>get_line( ).
  IF lv_line = 0.
    lv_line = 1.
    /scwm/cl_rf_bll_srvc=>set_line( lv_line ).
  ENDIF.


  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                  gc_scr_elmnt_nlpla_vrf ).
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off(
                                  gc_scr_elmnt_nlpla_vrf ).

  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                  gc_scr_elmnt_nista_vrf ).
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off(
                                  gc_scr_elmnt_nista_vrf ).

  /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                  gc_scr_elmnt_repl_uom_alt ).
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                  gc_scr_elmnt_repl_uom_alt ).
  ordim_confirm-vsola_chr             = ordim_confirm-meins.
  zcs_partial_replenishment-nista_uom = ordim_confirm-meins.
  zcs_partial_replenishment-repl_uom  = ordim_confirm-meins.
  zcs_partial_replenishment-repl_uom_inp  = ordim_confirm-meins.

  WRITE: ordim_confirm-vsola UNIT ordim_confirm-altme LEFT-JUSTIFIED TO ordim_confirm-vsola_chr.

  DATA(lo_controller_zpartq) = NEW lcl_controller_zpartq( ordim_confirm-lgnum ).

  lo_controller_zpartq->get_repl_data(
    EXPORTING
      is_ordim_confirm   = ordim_confirm
    CHANGING
      cs_partial_repl_hu = zcs_partial_replenishment
  ).

  "Count number of HUs
  CLEAR ordim_confirm-sumahu.
  LOOP AT tt_ordim_confirm INTO DATA(ls_ordim_confirm)
       WHERE srsrc = resource-rsrc AND
             flghuto = gc_xfeld.
    ordim_confirm-sumahu = ordim_confirm-sumahu + 1.
    "Check if we have only one destination
    IF lv_dest IS INITIAL.
      lv_dest = ls_ordim_confirm-nlpla.
    ENDIF.
    IF lv_dest <> ls_ordim_confirm-nlpla.
      CLEAR lv_unique_dest.
    ENDIF.
  ENDLOOP.
  ordim_confirm-vlenr_o = ordim_confirm-vlenr.
  ordim_confirm-srsrc_o = ordim_confirm-srsrc.
  ordim_confirm-nlpla_o = ordim_confirm-nlpla.
  ordim_confirm-nlenr_o = ordim_confirm-nlenr.
  ordim_confirm-processor = sy-uname.
*  ordim_confirm-rfhu = |{ ordim_confirm-nlenr ALPHA = OUT }|.
  MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
ENDFUNCTION.
