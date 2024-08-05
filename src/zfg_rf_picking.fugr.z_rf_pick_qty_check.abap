FUNCTION z_rf_pick_qty_check.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_VALID_PRF) TYPE  /SCWM/S_VALID_PRF_EXT
*"     REFERENCE(IV_FLG_VERIFIED) TYPE  XFELD
*"  EXPORTING
*"     REFERENCE(EV_FLG_VERIFIED) TYPE  XFELD
*"  CHANGING
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(ZCS_PARTIAL_REPLENISHMENT) TYPE
*"        ZSTR_RF_PARTIAL_REPLENISHMENT
*"----------------------------------------------------------------------
  DATA: ls_ordim_confirm          TYPE /scwm/s_rf_ordim_confirm,
        lv_line                   TYPE i,
        lv_qty_char               TYPE /scwm/de_qty_verif,
        lv_qty_numc               TYPE /scwm/ltap_vsola,
        lv_qty_comp               TYPE /scwm/ltap_vsola,
        lv_fcode                  TYPE /scwm/de_fcode,
        lv_step                   TYPE /scwm/de_step,
        lv_ltrans                 TYPE /scwm/de_ltrans,
        lv_state                  TYPE /scwm/de_state,
        lv_string(2)              TYPE c,
        lc_ltrans_picking(2)      TYPE c VALUE 'PI',
        lc_display_function_error TYPE xfeld VALUE 'E',
        lv_shortcut               TYPE /scwm/de_shortcut,
        lv_actfield               TYPE text60,
        lv_vsola                  TYPE /scwm/de_quantity,
        lv_change_uom             TYPE xfeld.

  DATA: ls_comb_tanum  TYPE /scwm/s_rf_comb_tanum,
        lv_nista       TYPE /scwm/ltap_nista,
        lt_wt          TYPE /scwm/tt_rf_ordim_confirm,
        lt_combined_wt TYPE /scwm/tt_rf_ordim_confirm,
        lo_badi        TYPE REF TO /scwm/ex_rf_pick_wt_combine,
        lx_badi        TYPE REF TO cx_badi,
        ls_exc_tab     TYPE /scwm/s_rf_exc,
        lv_quan_check  TYPE /scwm/de_quantity.

  FIELD-SYMBOLS <ordim_confirm> TYPE /scwm/s_rf_ordim_confirm.

  BREAK-POINT ID /scwm/rf_picking.
  BREAK-POINT ID /scwm/rf_picking_check.

* Update the working structure with the actual data
  READ TABLE tt_ordim_confirm INDEX lv_line INTO ordim_confirm.

* Check if we have special process Change UoM or
*   direct difference w/o diff. screen (e.g. PbV)
  IF ( ordim_confirm-difty IS NOT INITIAL AND
       ordim_confirm-ndifa IS INITIAL     AND
       ordim_confirm-nista IS INITIAL ) OR
     ordim_confirm-change_uom IS NOT INITIAL.
    lv_change_uom = 'X'.
  ELSE.
    CLEAR lv_change_uom.
  ENDIF.

* If we have a positive verification from the RF framework -> exit
  IF iv_flg_verified = /scmb/cl_c=>boole_true AND
     lv_change_uom IS INITIAL.
    ev_flg_verified = /scmb/cl_c=>boole_true.
    EXIT.
  ENDIF.

* Get logical transaction, step, state,fcode, line & shortcut.
  lv_ltrans = /scwm/cl_rf_bll_srvc=>get_ltrans( ).
  lv_step = /scwm/cl_rf_bll_srvc=>get_step( ).
  lv_state = /scwm/cl_rf_bll_srvc=>get_state( ).
  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).
  lv_line  = /scwm/cl_rf_bll_srvc=>get_line( ).
  lv_shortcut = /scwm/cl_rf_bll_srvc=>get_shortcut( ).
  lv_actfield = /scwm/cl_rf_bll_srvc=>get_cursor_field( ).

* When returning from nested HU screen or difference screen
  IF ( lv_fcode = fcode_nesthu AND
       gv_nested_save = /scmb/cl_c=>boole_true ) OR
     ( lv_fcode = fcode_diff AND
       gv_diff_save = /scmb/cl_c=>boole_true ) OR
     ( lv_fcode = fcode_go_to_low_stck AND
       gv_stock_save = /scmb/cl_c=>boole_true ) OR
     ( lv_fcode = fcode_diffbd AND
       gv_diff_save = /scmb/cl_c=>boole_true ).
    ev_flg_verified = /scmb/cl_c=>boole_true.
    EXIT.
  ENDIF.

* Cut ltrans - all picking transaction starts with PI.
  lv_string = lv_ltrans.

* Update the working structure with the actual data
  READ TABLE tt_ordim_confirm INDEX lv_line INTO ordim_confirm.

* Check actual QTY data.
* Check if difference were reported correctly.
* Exception is needed to report, DIFTY is full if reported.
  IF lv_string = lc_ltrans_picking OR
     lv_string = 'PV'.
    IF  lv_step = step_source_mtto OR
        lv_step = step_source_huto OR
        lv_step = step_source_blmt OR
        lv_step = step_source_blhu OR
        lv_step = step_dest_plhu   OR
        lv_step = step_dest_plmt   OR
        lv_step = 'PVMTTO' OR
        lv_step = 'PVHUTO' OR
        lv_step = 'PVPLHU' OR
        lv_step = 'PVPLMT' OR
        lv_step = 'PVBLMT' OR
        lv_step = 'PVBLHU'.
      lv_qty_numc = ordim_confirm-nista_verif.

    ELSEIF lv_step = step_pick_cpmt OR lv_step = step_pbv_cpmt OR
      lv_step = step_source_blcp OR lv_step = step_pbv_blcp.
      lv_qty_numc = ordim_confirm-combqty_verif.
    ELSEIF lv_step = step_dest_mphu.
      TRY.
          lv_qty_numc = ordim_confirm-sumchu_verif.
        CATCH cx_root.
      ENDTRY.
    ENDIF.

    IF lv_fcode = /scwm/cl_rf_bll_srvc=>c_fcode_enter OR
       lv_fcode = /scwm/cl_rf_bll_srvc=>c_fcode_unknown OR
       lv_fcode = fcode_enterf OR
       lv_fcode IS INITIAL.
*     If Source Material TO
      IF lv_step = step_source_mtto OR
         lv_step = step_source_blmt OR
         lv_step = 'PVMTTO' OR
         lv_step = 'PVBLMT'.
*       check for rounding problems comparing 3 digit UI values vs 14 digit internal
        lv_quan_check = abs( ordim_confirm-vsola - lv_qty_numc ).
        IF lv_quan_check > '0.001' OR
           lv_change_uom IS NOT INITIAL.
          IF lv_shortcut IS INITIAL.
            IF ordim_confirm-difty IS INITIAL.
*             Exception code is needed for reporting differences.
              ev_flg_verified = lc_display_function_error.
              /scwm/cl_rf_bll_srvc=>set_keep_value( 'X' ).
              MESSAGE e003(/scwm/rf_en).
              EXIT.
            ELSE.

              IF lv_change_uom IS NOT INITIAL.
                ordim_confirm-nista = ordim_confirm-nista_verif.

                IF ordim_confirm-altme IS NOT INITIAL.
*                 Calculate from-quantity in new AUoM
                  TRY.
                      CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                        EXPORTING
                          iv_matid     = ordim_confirm-matid
                          iv_quan      = ordim_confirm-vsolm
                          iv_unit_from = ordim_confirm-meins
                          iv_unit_to   = ordim_confirm-altme
                          iv_batchid   = ordim_confirm-batchid
                        IMPORTING
                          ev_quan      = lv_vsola.
                      ordim_confirm-ndifa = lv_vsola -
                                       ordim_confirm-nista.
                    CATCH /scwm/cx_md.
                  ENDTRY.
                ENDIF.
                MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
              ENDIF.
              IF lv_qty_numc = ordim_confirm-nista.
*               Qty already verified ok in difference step.
                ev_flg_verified = /scmb/cl_c=>boole_true.
                EXIT.
              ELSE.
*               Qty was changed on screen.
*               ev_flg_verified = lc_display_function_error.
                MESSAGE e021(/scwm/rf_en).
                EXIT.
              ENDIF.

            ENDIF.
          ENDIF.
        ELSE.
*         Qty verified ok.
          ev_flg_verified = /scmb/cl_c=>boole_true.
          EXIT.
        ENDIF.
*     If combine Picking
      ELSEIF lv_step = step_pick_cpmt OR lv_step = step_pbv_cpmt OR
        lv_step = step_source_blcp OR lv_step = step_pbv_blcp.
        IF lv_qty_numc <> ordim_confirm-combqty OR
           lv_change_uom IS NOT INITIAL.
          IF lv_shortcut IS INITIAL.
            IF ordim_confirm-difty IS INITIAL.
*             Exception code is needed for reporting differences.
              ev_flg_verified = lc_display_function_error.
              /scwm/cl_rf_bll_srvc=>set_keep_value( 'X' ).
              MESSAGE e003(/scwm/rf_en).
              EXIT.
            ELSE.

*             DIFTY is set but no difference quantity are set
*             -> special case of change UoM
              IF lv_change_uom IS NOT INITIAL.

                IF ordim_confirm-altme IS NOT INITIAL.
                  lv_nista = ordim_confirm-combqty_verif.
                  CLEAR lt_wt.
                  LOOP AT ordim_confirm-comb_tanums INTO ls_comb_tanum.
                    IF sy-tabix = 1.
*                 Calculate combined quantity in new AUoM
                      TRY.
                          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                            EXPORTING
                              iv_matid     = ordim_confirm-matid
                              iv_quan      = ordim_confirm-combqty
                              iv_unit_from = ls_comb_tanum-altme
                              iv_unit_to   = ordim_confirm-altme
                              iv_batchid   = ordim_confirm-batchid
                            IMPORTING
                              ev_quan      = lv_vsola.
                          ordim_confirm-combqty = lv_vsola.
                        CATCH /scwm/cx_md.
                      ENDTRY.
                    ENDIF.
                    READ TABLE tt_ordim_confirm ASSIGNING <ordim_confirm> WITH KEY tanum = ls_comb_tanum-comb_tanum.
                    IF sy-subrc = 0.
                      TRY.
                          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                            EXPORTING
                              iv_matid     = <ordim_confirm>-matid
                              iv_quan      = <ordim_confirm>-vsola
                              iv_unit_from = ls_comb_tanum-altme
                              iv_unit_to   = ordim_confirm-altme
                              iv_batchid   = <ordim_confirm>-batchid
                            IMPORTING
                              ev_quan      = <ordim_confirm>-vsola.
                          <ordim_confirm>-altme = ordim_confirm-altme.
                          <ordim_confirm>-combqty = ordim_confirm-combqty.
                          <ordim_confirm>-vsola_chr = <ordim_confirm>-vsola.
                        CATCH /scwm/cx_md.
                      ENDTRY.
*                        APPEND <ordim_confirm> TO lt_combined_wt.
                    ENDIF.
                  ENDLOOP.
                  IF lv_step = step_pick_cpmt OR lv_step = step_source_blcp .
                    CALL FUNCTION '/SCWM/RF_COMBPICK_QTY_DISTRIBU'
                      CHANGING
                        tt_ordim_confirm = tt_ordim_confirm
                        ordim_confirm    = ordim_confirm
                        lv_nista         = lv_nista.

                  ENDIF.
                ENDIF.
              ENDIF.
              ev_flg_verified = /scmb/cl_c=>boole_true.
              EXIT.

            ENDIF.
          ENDIF.
        ELSE.
*         Qty verified ok.
          ev_flg_verified = /scmb/cl_c=>boole_true.
          EXIT.
        ENDIF.
*     If destination Material TO
      ELSEIF lv_step = step_dest_plmt OR
             lv_step = 'PVPLMT'.
        IF lv_qty_numc <> ordim_confirm-nista.
          IF lv_shortcut IS INITIAL.
            IF ordim_confirm-difty IS INITIAL.
*             Exception code is needed for reporting differences.
              ev_flg_verified = lc_display_function_error.
              /scwm/cl_rf_bll_srvc=>set_keep_value( 'X' ).
              MESSAGE e003(/scwm/rf_en).
              EXIT.
            ELSE.
*             IF DIFTY is set but no difference quantity
*               -> special case PbV with difference via pushbutton
              IF ordim_confirm-difty IS NOT INITIAL AND
                 ordim_confirm-ndifa IS INITIAL.
                ordim_confirm-nista = ordim_confirm-nista_verif.
                ordim_confirm-ndifa = ordim_confirm-vsola -
                                     ordim_confirm-nista.
                MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
              ENDIF.
*             Qty verified ok.
              ev_flg_verified = /scmb/cl_c=>boole_true.
              EXIT.
            ENDIF.
          ENDIF.
        ELSE.
*         Qty verified ok.
          ev_flg_verified = /scmb/cl_c=>boole_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* If the verification value is the same like
  IF ( ( ( lv_step = step_source_mtto OR
           lv_step = step_pick_cpmt   OR
           lv_step = step_source_blmt OR
           lv_step = step_source_blcp OR
           lv_step = step_pbv_cpmt OR
           lv_step = step_pbv_blcp OR
           lv_step = 'PVMTTO' OR
           lv_step = 'PVBLMT' ) AND
         lv_qty_numc = ordim_confirm-vsola ) OR
       ( ( lv_step = step_dest_plmt OR
          lv_step = 'PVPLMT'  ) AND
          lv_qty_numc = ordim_confirm-nista ) ) OR
       (  lv_step = step_dest_mphu AND
          lv_qty_numc = ordim_confirm-sumchu ).
    ev_flg_verified = /scmb/cl_c=>boole_true.
    EXIT.
  ENDIF.

  IF NOT ordim_confirm-nista IS INITIAL.
    ordim_confirm-nista_verif = ordim_confirm-nista.
  ENDIF.

  IF lv_fcode = fcode_list AND
     ev_flg_verified IS INITIAL.
    /scwm/cl_rf_bll_srvc=>set_keep_value( 'X' ).
  ENDIF.

  IF NOT ordim_confirm-ndifa IS INITIAL.
    ordim_confirm-ndifa_verif = ordim_confirm-ndifa.
  ENDIF.

  IF lv_step EQ gc_step_zpartq.
    DATA(lo_controller_zpartq) = NEW lcl_controller_zpartq( ordim_confirm-lgnum ).
    lo_controller_zpartq->do_validate_input(
       CHANGING
         cs_ordim_confirm   = ordim_confirm
         ct_ordim_confirm   = tt_ordim_confirm
         cs_partial_repl_hu = zcs_partial_replenishment ).
  ENDIF.

  MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.

ENDFUNCTION.
