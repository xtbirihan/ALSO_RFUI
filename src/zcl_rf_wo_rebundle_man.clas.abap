CLASS zcl_rf_wo_rebundle_man DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS call_z_wo_rebundle_man
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        !iv_keep_pickhu TYPE xfeld DEFAULT space
        !iv_write_log   TYPE xfeld DEFAULT abap_true
        !is_wo_template TYPE /scwm/s_who_int OPTIONAL
        !ir_tanum       TYPE rseloption
        !it_bapiret     TYPE bapirettab OPTIONAL
        !it_who         TYPE /scwm/tt_who_int OPTIONAL
      EXPORTING
        !ev_return      TYPE boole_d
        !ev_logno       TYPE /scwm/de_whologno
        !ev_severity    TYPE bapi_mtype
        !et_wo          TYPE /scwm/tt_who_int
        !et_bapiret     TYPE bapirettab .

    CLASS-METHODS is_zrebundle_active
      IMPORTING
        !iv_lgnum        TYPE /scwm/lgnum
      RETURNING
        VALUE(rv_result) TYPE boole_d .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_RF_WO_REBUNDLE_MAN IMPLEMENTATION.


  METHOD call_z_wo_rebundle_man.
********************************************************************
*& Key          : <BSUGAREV>-Aug 29, 2023
*& Request No.  : GAP-017 FS Picking WO Bundling
********************************************************************
*& Description  : Call custom version of FM: /scwm/wo_rebundle_man
*&    in this FM check if AAREA is blocked
*&
********************************************************************
    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_rsrc_rebundle.

    IF is_zrebundle_active( iv_lgnum = iv_lgnum ) = abap_false.
      RETURN.
    ENDIF.

    ev_return = abap_true.

    CALL FUNCTION 'Z_WO_REBUNDLE_MAN'
      EXPORTING
        iv_lgnum       = iv_lgnum
        iv_keep_pickhu = iv_keep_pickhu
        iv_write_log   = iv_write_log
        is_wo_template = is_wo_template
        ir_tanum       = ir_tanum
        it_who         = it_who
        it_bapiret     = it_bapiret
      IMPORTING
        ev_logno       = ev_logno
        ev_severity    = ev_severity
        et_wo          = et_wo
        et_bapiret     = et_bapiret.

  ENDMETHOD.


  METHOD is_zrebundle_active.
********************************************************************
*& Key          : BSUGAREV-Dec 7, 2023
*& Request No.  : GAP-017 FS Picking WO Bundling
********************************************************************
*& Description  : Implementation is valid only if it is called from
*&     valid RF transaction
********************************************************************
    DATA(lv_ltrans) = /scwm/cl_rf_bll_srvc=>get_ltrans( ).

    rv_result = zcl_switch=>get_switch_state(
        iv_lgnum  = iv_lgnum
        iv_devid  = zif_switch_const=>c_zrfui_001
        it_fields = VALUE #( ( field = zif_switch_const=>c_ltrans field_value = lv_ltrans ) ) ).
  ENDMETHOD.
ENDCLASS.
