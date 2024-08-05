FUNCTION z_rf_pick_zpartq_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(WHO) TYPE  /SCWM/S_WHO_INT
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(NESTED_HU) TYPE  /SCWM/S_RF_NESTED_HU
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_NESTED_HU) TYPE  /SCWM/TT_RF_NESTED_HU
*"     REFERENCE(T_RF_PICK_HUS) TYPE  /SCWM/TT_RF_PICK_HUS
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF
*"     REFERENCE(ZCS_PARTIAL_REPLENISHMENT) TYPE
*"        ZSTR_RF_PARTIAL_REPLENISHMENT
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-190423
*& Request No.   : GAP-004 – Display Partial Replenisment
**********************************************************************
*& Description (short)
*& Partial replenishment functions. Display replenishment quantity
*& and call putaway if not the whole HU is used for the replenishment
*&
**********************************************************************

  DATA: bapiret     TYPE bapirettab,
        severity    TYPE bapi_mtype,
        tanum       TYPE /scwm/tanum,
        ltap_vb     TYPE /scwm/tt_ltap_vb,
        create      TYPE /scwm/s_to_create_int,
        ls_confirm  TYPE /scwm/to_conf,
        create_hu   TYPE /scwm/s_to_crea_hu,
        lv_work_who TYPE /scwm/de_who,
        quantity    TYPE /scwm/de_quantity,
        error       TYPE bapiret2.

  DATA: lv_restart_transaction TYPE xfeld,
        lv_ser_err             TYPE xfeld.

  BREAK-POINT ID /scwm/rf_picking.

  DATA(lv_line) = /scwm/cl_rf_bll_srvc=>get_line( ).

  DATA(lo_controller_zpartq) = NEW lcl_controller_zpartq( ordim_confirm-lgnum ).

  CASE /scwm/cl_rf_bll_srvc=>get_fcode( ).
    WHEN /scwm/cl_rf_bll_srvc=>c_fcode_enter.

      lo_controller_zpartq->do_validate_input(
         CHANGING
           cs_ordim_confirm   = ordim_confirm
           ct_ordim_confirm   = tt_ordim_confirm
           cs_partial_repl_hu = zcs_partial_replenishment ).

      create = CORRESPONDING /scwm/s_to_create_int( ordim_confirm ).

      IF zcs_partial_replenishment-partial_qty = abap_true.

        DATA(huident) = ordim_confirm-vlenr.
        DATA(lgnum)   = ordim_confirm-lgnum.

        CALL FUNCTION 'ZINT_TO_CANCEL'
          EXPORTING
            iv_lgnum    = ordim_confirm-lgnum
            it_cancl    = VALUE /scwm/tt_cancl( ( tanum = ordim_confirm-tanum ) )
          IMPORTING
            et_bapiret  = bapiret
            ev_severity = severity.
        IF severity CA wmegc_severity_ea.
          CALL FUNCTION 'DB_ROLLBACK'.

          error = VALUE #( bapiret[ type = severity ] OPTIONAL ).
          MESSAGE ID error-id TYPE severity NUMBER error-number
                  WITH error-message_v1 error-message_v2 error-message_v3 error-message_v4.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.

        zcl_core_ltap=>set_ltap_cancelled( CORRESPONDING #( ordim_confirm ) ).

        DATA(configuration) = lo_controller_zpartq->read_configuration( ).
        create              = CORRESPONDING /scwm/s_to_create_int( ordim_confirm ).
        create-procty       = configuration-procty_part_repl.
        CLEAR: create-letyp, create-nlenr, create-dguid_hu.

        IF zcs_partial_replenishment-nista_uom <> zcs_partial_replenishment-repl_uom_inp.

          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = ordim_confirm-matid
              iv_quan      = ordim_confirm-nista
              iv_unit_from = zcs_partial_replenishment-repl_uom_inp
              iv_unit_to   = ordim_confirm-meins
              iv_batchid   = VALUE /scwm/de_batchid( )
            IMPORTING
              ev_quan      = quantity.

          create-anfme = ordim_confirm-nista = quantity.

        ELSE.
          create-anfme = ordim_confirm-nista.
        ENDIF.
        create-altme = zcs_partial_replenishment-repl_uom_inp.
        CALL FUNCTION '/SCWM/TO_CREATE'
          EXPORTING
            iv_lgnum       = ordim_confirm-lgnum
            iv_commit_work = abap_false
            it_create      = VALUE /scwm/tt_to_create_int( ( create ) )
          IMPORTING
            ev_tanum       = tanum
            et_ltap_vb     = ltap_vb
            et_bapiret     = bapiret
            ev_severity    = severity.
        IF severity CA wmegc_severity_ea.
          ROLLBACK WORK.
          error = VALUE #( bapiret[ type = severity ] OPTIONAL ).
          CLEAR ordim_confirm-nista_verif.
          MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.

          MESSAGE ID error-id TYPE severity NUMBER error-number
                  WITH error-message_v1 error-message_v2 error-message_v3 error-message_v4.
        ELSE.
          COMMIT WORK AND WAIT.
          /scwm/cl_tm=>cleanup( ).
        ENDIF.

        ls_confirm = CORRESPONDING #( ltap_vb[ 1 ] ).
        ls_confirm-nista = create-anfme.
        ls_confirm-altme = zcs_partial_replenishment-repl_uom_inp.
        CALL FUNCTION '/SCWM/TO_CONFIRM'
          EXPORTING
            iv_lgnum       = ordim_confirm-lgnum
            iv_commit_work = abap_false
            it_conf        = VALUE /scwm/to_conf_tt( ( ls_confirm ) )
          IMPORTING
            et_ltap_vb     = ltap_vb
            et_bapiret     = bapiret
            ev_severity    = severity.
        IF severity CA wmegc_severity_ea.
          ROLLBACK WORK.
          error = VALUE #( bapiret[ type = severity ] OPTIONAL ).
          MESSAGE ID error-id TYPE severity NUMBER error-number
                  WITH error-message_v1 error-message_v2 error-message_v3 error-message_v4.
        ELSE.
          COMMIT WORK AND WAIT.
          /scwm/cl_tm=>cleanup( ).
        ENDIF.

        lo_controller_zpartq->do_putaway_remaining_repl_qty(
          EXPORTING
            iv_lgtyp    = zcs_partial_replenishment-orig_src_lgtyp
            iv_huident  = huident
            iv_procty   = configuration-procty_ptwy_part_repl
            iv_queue    = who-queue
            iv_resource = resource-rsrc ).

        CLEAR zcs_partial_replenishment.

      ELSE.
        CLEAR ordim_confirm-nista_verif.
        ordim_confirm-pickhu = ordim_confirm-vlenr.
        ordim_confirm-pickhu_verif = ordim_confirm-vlenr.

        MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.

        CALL FUNCTION '/SCWM/RF_PICK_PIPLHU_PAI'
          CHANGING
            selection        = selection
            resource         = resource
            who              = who
            ordim_confirm    = ordim_confirm
            nested_hu        = nested_hu
            tt_ordim_confirm = tt_ordim_confirm
            tt_nested_hu     = tt_nested_hu
            t_rf_pick_hus    = t_rf_pick_hus
            wme_verif        = wme_verif.


        /scwm/cl_rf_bll_srvc=>set_fcode( /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans ).
        /scwm/cl_rf_bll_srvc=>set_ltrans( gc_ltrans_pysisg ).

      ENDIF.


  ENDCASE.





ENDFUNCTION.
