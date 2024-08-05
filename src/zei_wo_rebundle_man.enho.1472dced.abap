"Name: \FU:/SCWM/WO_REBUNDLE_MAN\SE:BEGIN\EI
ENHANCEMENT 0 ZEI_WO_REBUNDLE_MAN.
  DATA: lv_return TYPE boole_d.

  zcl_rf_wo_rebundle_man=>call_z_wo_rebundle_man(
    EXPORTING
      iv_lgnum       = iv_lgnum
      iv_keep_pickhu = iv_keep_pickhu
      iv_write_log   = iv_write_log
      is_wo_template = is_wo_template
      ir_tanum       = ir_tanum
      it_who         = gt_who
      it_bapiret     = it_bapiret
    IMPORTING
      ev_return      = lv_return
      ev_logno       = ev_logno
      ev_severity    = ev_severity
      et_wo          = et_wo
      et_bapiret     = et_bapiret ).

  IF lv_return = abap_true.
    RETURN.
  ENDIF.
ENDENHANCEMENT.
