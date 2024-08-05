FUNCTION z_rf_pick_pichbn_pai .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_NESTED_HU) TYPE  /SCWM/TT_RF_NESTED_HU
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(WHO) TYPE  /SCWM/S_WHO_INT
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

* Check difference quantity and difference indicator are set.
* In case of an error or no sufficient data returns to difference
* screen.
  DATA: lv_fcode               TYPE /scwm/de_fcode,
        lv_restart_transaction TYPE xfeld VALUE IS INITIAL ##needed,
        lv_proc                TYPE xfeld VALUE IS INITIAL,
        lt_ordim_confirm       TYPE /scwm/tt_rf_ordim_confirm,
        lv_line                TYPE i,
        ls_lagp                TYPE /scwm/lagp,
        ls_t331                TYPE /scwm/t331,
        lv_data_entry          TYPE /scwm/de_data_entry,
        ls_ordim_o             TYPE /scwm/ordim_o,
        lv_return              TYPE boole_d.


  FIELD-SYMBOLS <lv_exec_step> TYPE /scwm/de_exec_step.

  BREAK-POINT ID /scwm/rf_picking.

  ASSIGN ('(/SCWM/SAPLRF_PICKING)GV_EXEC_STEP') TO <lv_exec_step>.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).
  lv_line = /scwm/cl_rf_bll_srvc=>get_line( ).

  READ TABLE tt_ordim_confirm INTO ordim_confirm INDEX lv_line.

  IF ordim_confirm-nlpla_verif = ordim_confirm-nlpla_o.
*    READ TABLE tt_ordim_confirm INDEX lv_line INTO ordim_confirm.
    CLEAR ordim_confirm-nlpla_verif.
    MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
    MESSAGE e093(/scwm/rf_en).
  ENDIF.


* Get actual function code
  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).

  "for the asynchronous rf processing we have to check at this point in time whether
  "all the asynchronous processes have finished successfully
  /scwm/cl_rf_confirm_picking=>get_instance( )->complete_async_processing(
    EXPORTING
      iv_tablin              = lv_line
    IMPORTING
      ev_return              = lv_return
      ev_restart_transaction = lv_restart_transaction
    CHANGING
      cs_ordim_confirm = ordim_confirm
      ct_ordim_confirm = tt_ordim_confirm ).

  IF lv_return = abap_true.
    RETURN.
  ENDIF.



  REFRESH lt_ordim_confirm.
  lt_ordim_confirm[] = tt_ordim_confirm[].
  ordim_confirm-nlpla = ordim_confirm-nlpla_verif.
  "***********************************************
  "Is additional logic needed for target bin
  CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
    EXPORTING
      iv_lgnum      = ordim_confirm-lgnum
      iv_lgpla      = ordim_confirm-nlpla
    IMPORTING
      es_lagp       = ls_lagp
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      enqueue_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    SELECT FROM /scwm/lagp
      FIELDS *
       WHERE verif EQ @ordim_confirm-nlpla_verif
        INTO TABLE @DATA(lt_lagp_check).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      DATA(lv_dest_lgtyp) = lt_lagp_check[ 1 ]-lgtyp.
    ENDIF.
  ELSE.
    lv_dest_lgtyp = ls_lagp-lgtyp.
  ENDIF.

  "As default bin level is switched off
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_zstbin_level ).

  IF lv_fcode = /scwm/cl_rf_bll_srvc=>c_fcode_enter OR
     lv_fcode = fcode_save.
    "Master switch.
    IF  zcl_switch=>get_switch_state( iv_lgnum = /scwm/cl_tm=>sv_lgnum
                                        iv_devid = zif_switch_const=>c_zint_001 ) EQ abap_true.
      "Field level switch on/off
      DATA(lt_switch_fields) = VALUE ztt_switch_fields( ( field       = zif_switch_const=>c_lgtype
                                                          field_value = lv_dest_lgtyp ) ).

      IF zcl_switch=>get_switch_state( iv_lgnum  = /scwm/cl_tm=>sv_lgnum
                                       iv_devid  = zif_switch_const=>c_zint_001
                                       it_fields = lt_switch_fields ) EQ abap_true.
        /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_zstbin_level ).
        /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_scr_elmnt_nlpla_vrf ).

        IF zcs_rf_inp_help_for_barc-stbin_level IS INITIAL.
          IF gv_stbin_level_active = abap_true.
            MESSAGE e013(zmc_rfui).
          ELSE.
            gv_stbin_level_active = abap_true.
            /scwm/cl_rf_bll_srvc=>set_field( CONV #( gc_scr_elmnt_zstbin_level ) ).
          ENDIF.
          /scwm/cl_rf_bll_srvc=>set_prmod(
                     /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).
          /scwm/cl_rf_bll_srvc=>set_fcode(
                     /scwm/cl_rf_bll_srvc=>c_fcode_enter ).
          RETURN.
        ELSE.
          DATA(lv_level) = CONV /scwm/lagp-lvl_v( |{ CONV num2( zcs_rf_inp_help_for_barc-stbin_level ) ALIGN = RIGHT WIDTH = 18 }| ).
          SELECT FROM /scwm/lagp
            FIELDS *
             WHERE verif EQ @ordim_confirm-nlpla_verif
               AND lvl_v EQ @lv_level
              INTO TABLE @DATA(lt_lagp)
            UP TO 1 ROWS.
          IF sy-subrc NE 0.
            MESSAGE e013(zmc_rfui).
          ENDIF.
          ls_lagp = lt_lagp[ 1 ].
          ordim_confirm-nlpla = ls_lagp-lgpla.

          IF ls_lagp-skzua IS NOT INITIAL
             OR ls_lagp-skzue IS NOT INITIAL
             OR ls_lagp-skzsi IS NOT INITIAL.
            MESSAGE e015(zmc_rfui).
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY lt_ordim_confirm FROM ordim_confirm
     TRANSPORTING nlpla nlpla_verif
            WHERE mandt = ordim_confirm-mandt AND
                  lgnum = ordim_confirm-lgnum AND
                  tanum = ordim_confirm-tanum.




*   Set the PBV relevant fields (aisle, stack, lvl_v)
    PERFORM pbv_piplhu_pbo IN PROGRAM /scwm/saplrf_picking CHANGING ordim_confirm.
*   We clear the verification field
*     to open it again on the next screen
*   Close the verification field in case of manual selected bin
    IF ordim_confirm-bprop IS NOT INITIAL.
      CLEAR ordim_confirm-nlpla_verif.
    ELSEIF lv_data_entry = wmegc_data_entry_voice.
*     SET (close) PBV relevant verification fields in case of
*     manual bin selection
      ordim_confirm-aisle_verif = ordim_confirm-aisle.
      ordim_confirm-stack_verif = ordim_confirm-stack.
      ordim_confirm-lvl_v_verif = ordim_confirm-lvl_v.
    ENDIF.
    IF ordim_confirm-sumahu <= 1.
      MODIFY lt_ordim_confirm FROM ordim_confirm
        TRANSPORTING nlpla nlpla_verif
            aisle aisle_verif stack stack_verif lvl_v lvl_v_verif
        WHERE mandt = ordim_confirm-mandt AND
            lgnum = ordim_confirm-lgnum AND
            tanum = ordim_confirm-tanum.
    ELSE.
      MODIFY lt_ordim_confirm FROM ordim_confirm
        TRANSPORTING nlpla nlpla_verif
            aisle aisle_verif stack stack_verif lvl_v lvl_v_verif
          WHERE mandt = ordim_confirm-mandt AND
              lgnum = ordim_confirm-lgnum.
    ENDIF.

    CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
      EXPORTING
        iv_lgnum      = ordim_confirm-lgnum
        iv_lgpla      = ordim_confirm-nlpla
      IMPORTING
        es_lagp       = ls_lagp
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        enqueue_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL FUNCTION '/SCWM/T331_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ordim_confirm-lgnum
        iv_lgtyp  = ls_lagp-lgtyp
      IMPORTING
        es_t331   = ls_t331
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION '/SCWM/RF_PICK_ORDIM_CONFIRM'
      EXPORTING
        iv_simulate            = gc_xfeld
      IMPORTING
        ev_restart_transaction = lv_restart_transaction
      CHANGING
        resource               = resource
        tt_ordim_confirm       = lt_ordim_confirm
        ordim_confirm          = ordim_confirm
        tt_nested_hu           = tt_nested_hu
        who                    = who
        ev_proc                = lv_proc.

    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( gc_scr_elmnt_nlpla_vrf ).
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( gc_scr_elmnt_zstbin_level ).

    IF lv_proc = gc_xfeld.
      tt_ordim_confirm[] = lt_ordim_confirm[].
      /scwm/cl_rf_bll_srvc=>set_prmod(
                 /scwm/cl_rf_bll_srvc=>c_prmod_background ).
      /scwm/cl_rf_bll_srvc=>set_fcode( fcode_save ).
    ENDIF.

  ELSEIF lv_fcode = fcode_backf.

    CALL FUNCTION '/SCWM/TO_READ_SINGLE'
      EXPORTING
        iv_lgnum     = ordim_confirm-lgnum
        iv_tanum     = ordim_confirm-tanum
        iv_flglock   = abap_false
      IMPORTING
        es_ordim_o   = ls_ordim_o
      EXCEPTIONS
        wrong_input  = 1
        not_found    = 2
        foreign_lock = 3
        error        = 4
        OTHERS       = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ordim_confirm-nltyp = ls_ordim_o-nltyp.
    ordim_confirm-nlber = ls_ordim_o-nlber.
    ordim_confirm-nlpla = ls_ordim_o-nlpla.
    CLEAR ordim_confirm-nlpla_verif.
    DELETE ordim_confirm-exc_tab WHERE iprcode = wmegc_iprcode_chbd.

    CALL FUNCTION '/SCWM/T331_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ordim_confirm-lgnum
        iv_lgtyp  = ordim_confirm-nltyp
      IMPORTING
        es_t331   = ls_t331
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF ls_t331-bingn IS NOT INITIAL.
      CLEAR ordim_confirm-nlpla_o.
    ENDIF.
    MODIFY tt_ordim_confirm FROM ordim_confirm
      TRANSPORTING nlpla nlpla_verif nltyp nlber nlpla_o exc_tab
            WHERE mandt = ordim_confirm-mandt AND
                  lgnum = ordim_confirm-lgnum AND
                  tanum = ordim_confirm-tanum.
    /scwm/cl_rf_bll_srvc=>set_prmod(
               /scwm/cl_rf_bll_srvc=>c_prmod_background ).
    IF <lv_exec_step> = wmegc_execstep_b4.
      /scwm/cl_rf_bll_srvc=>set_fcode( /scwm/cl_rf_bll_srvc=>c_fcode_end_ltrans ).
    ELSE.
      /scwm/cl_rf_bll_srvc=>set_fcode(
                 /scwm/cl_rf_bll_srvc=>c_fcode_update_back ).
    ENDIF.
  ENDIF.


ENDFUNCTION.
