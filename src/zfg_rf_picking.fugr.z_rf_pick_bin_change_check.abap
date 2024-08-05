FUNCTION z_rf_pick_bin_change_check .
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
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-170323
*& Request No.   : GAP-070 – Internal Input Help In RF For Barcode
**********************************************************************
*& Description (short)
*& If the logic is activated for the storage type, then check the
*& storage location existence against the verification field.
*& Thereafter continue with the normal logic
**********************************************************************
* Verify entered storage bin against storage bin master data.
* Storage bin can be storage bin or verification field (LAGP-VERIF).
*
* No messages are raised. On any error 'EV_FLG_VERIFIED' should returned
*   with initial value.
* On case of convertion exits (named in IS_VERIF_PRF-CONVEXIT) we must
*   do the convertion by hand.

  DATA: lv_ltrans                 TYPE /scwm/de_ltrans,
        lv_line                   TYPE i,
        lc_display_function_error TYPE xfeld VALUE 'E',
        ls_lagp                   TYPE /scwm/lagp,
        ls_lagp_int               TYPE /scwm/s_lagp_int.


  BREAK-POINT ID /scwm/rf_picking.
  CLEAR ev_flg_verified.

* If we have a positive verification material against material from
*   the RF framework we leave the fm

* Get logical transaction, step and actual line of the table.
  lv_ltrans = /scwm/cl_rf_bll_srvc=>get_ltrans( ).
  lv_line = /scwm/cl_rf_bll_srvc=>get_line( ).


* Update the working structure with the actual data
  READ TABLE tt_ordim_confirm INDEX lv_line INTO ordim_confirm.

  "***********************************************
  "Is additional logic needed for target bin
  DATA(lv_dest_lgtyp) = ordim_confirm-nlpla_verif+0(4).
  IF zcl_switch=>get_switch_state( iv_lgnum = /scwm/cl_tm=>sv_lgnum
                                   iv_devid = zif_switch_const=>c_zint_001 ) EQ abap_true.
    "Field level switch on/off
    DATA(lt_switch_fields) = VALUE ztt_switch_fields( ( field       = zif_switch_const=>c_lgtype
                                                        field_value = lv_dest_lgtyp ) ).

    IF zcl_switch=>get_switch_state( iv_lgnum  = /scwm/cl_tm=>sv_lgnum
                                     iv_devid  = zif_switch_const=>c_zint_001
                                     it_fields = lt_switch_fields ) EQ abap_true.
      SELECT FROM /scwm/lagp
        FIELDS lgpla
        WHERE lgnum EQ @ordim_confirm-lgnum
          AND verif EQ @ordim_confirm-nlpla_verif
        INTO TABLE @DATA(lt_lgpla_exists)
        UP TO 1 ROWS.
      IF lt_lgpla_exists IS INITIAL.
        ev_flg_verified = lc_display_function_error.
        MESSAGE e014(zmc_rfui) WITH ordim_confirm-nlpla_verif.
      ENDIF.
      ev_flg_verified = /scmb/cl_c=>boole_true.
      RETURN.
    ENDIF.
  ENDIF.

  ordim_confirm-nlpla = ordim_confirm-nlpla_verif.


  CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
    EXPORTING
      iv_lgnum    = ordim_confirm-lgnum
      iv_lgpla    = ordim_confirm-nlpla
    IMPORTING
      es_lagp     = ls_lagp
    EXCEPTIONS
*      wrong_input     = 1                    " Eingabeparameter fehlerhaft
*      not_found       = 2                    " Kein Lagerplatz gefunden
*      enqueue_error   = 3                    " Fehler beim Setzen der Enqueue-Sperre
      others          = 0.

  MOVE-CORRESPONDING ls_lagp TO ls_lagp_int.
*Could be a not existing sub-bin

  CALL FUNCTION '/SCWM/LAGP_CHECK_CREATE_SUBBIN'
    EXPORTING
      iv_lgnum          = ordim_confirm-lgnum
      iv_lgpla          = ordim_confirm-nlpla
      iv_letyp          = ordim_confirm-letyp
      iv_create_subbins = 'X'
      is_lagp_int       = ls_lagp_int
      iv_vlenr          = ordim_confirm-vlenr
      iv_nlenr          = ordim_confirm-nlenr
    IMPORTING
      es_lagp_int       = ls_lagp_int
      ev_lgpla          = ordim_confirm-nlpla
    EXCEPTIONS
      wrong_input       = 1
      not_created       = 2
      OTHERS            = 3.
  IF sy-subrc NE 0.
    ev_flg_verified = lc_display_function_error.
    ordim_confirm-nlpla = ordim_confirm-nlpla_o.
    MESSAGE e045(/scwm/rf_en) WITH ordim_confirm-nlpla_verif.
  ENDIF.

* changing bin if allowed.
* updating table with new bin.
  IF ev_flg_verified IS INITIAL.
    ev_flg_verified = /scmb/cl_c=>boole_true.
    ordim_confirm-nltyp = ls_lagp_int-lgtyp.
    ordim_confirm-nlber = ls_lagp_int-lgber.

    MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
  ENDIF.

ENDFUNCTION.
