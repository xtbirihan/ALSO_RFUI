FUNCTION z_rf_perform_unload.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IS_TU_ACT) TYPE  /SCWM/S_ASPK_TU
*"     VALUE(IV_HUIDENT) TYPE  /SCWM/HUIDENT
*"     VALUE(IV_UNASSIGN_HU) TYPE  FLAG
*"     VALUE(IV_GUID_HU) TYPE  /SCWM/GUID_HU
*"  EXPORTING
*"     VALUE(EV_NOT_LOADED) TYPE  FLAG
*"     VALUE(EV_WRONG_TU) TYPE  FLAG
*"     VALUE(EV_CANNOT_BE_UNLOADED) TYPE  FLAG
*"     VALUE(EV_CANNOT_UNASSIGN) TYPE  FLAG
*"----------------------------------------------------------------------

**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Decision of densesting and then load the HU/HUs to the TU
**********************************************************************
  data: lv_msg TYPE string.

  DATA(lo_loading) = NEW zcl_rf_loading( iv_lgnum ).
  lo_loading->init_log( |{ is_tu_act-tu_num ALPHA = OUT }{ iv_huident ALPHA = OUT }| ).
  lo_loading->get_hu_hdr(
    EXPORTING
      iv_lgnum    = iv_lgnum                 " Warehouse Number/Warehouse Complex
      iv_huident  = iv_huident                 " Handling Unit Identification
    IMPORTING
      et_huhdr    = DATA(lt_huhdr)                    " Table Type for HU Headers in the Internal Structure
      ev_unnested = DATA(lv_unnested)
  ).
  IF lv_unnested EQ abap_false.
    lo_loading->reverse_loading_hu_perform(
      EXPORTING
        is_tu_act             = is_tu_act                           " Key for Aspect: TU
        iv_huident            = iv_huident                          " Handling Unit Identification
        iv_unassign_hu        = iv_unassign_hu
        iv_guid_hu            = iv_guid_hu                          " Unique Internal Identification of a Handling Unit
      IMPORTING
        ev_not_loaded         = ev_not_loaded
        ev_wrong_tu           = ev_wrong_tu
        ev_cannot_be_unloaded = ev_cannot_be_unloaded
        ev_cannot_unassign    = ev_cannot_unassign
    ).
    IF ev_not_loaded EQ abap_true.
      MESSAGE e093(zmc_rfui) WITH iv_huident INTO lv_msg.
    ENDIF.
    IF ev_wrong_tu EQ abap_true.
      MESSAGE e094(zmc_rfui) WITH iv_huident INTO lv_msg.
    ENDIF.
    IF ev_cannot_be_unloaded EQ abap_true.
      MESSAGE e095(zmc_rfui) WITH iv_huident INTO lv_msg.
    ENDIF.
    IF ev_cannot_unassign EQ abap_true.
      MESSAGE e095(zmc_rfui) WITH iv_huident INTO lv_msg.
    ENDIF.
  ELSE.
    LOOP AT lt_huhdr INTO DATA(ls_huhdr).
      lo_loading->reverse_loading_hu_perform(
        EXPORTING
          is_tu_act             = is_tu_act                           " Key for Aspect: TU
          iv_huident            = ls_huhdr-huident                          " Handling Unit Identification
          iv_unassign_hu        = iv_unassign_hu
          iv_guid_hu            = ls_huhdr-guid_hu                          " Unique Internal Identification of a Handling Unit
        IMPORTING
          ev_not_loaded         = DATA(lv_not_loaded)
          ev_wrong_tu           = DATA(lv_wrong_tu)
          ev_cannot_be_unloaded = DATA(lv_cannot_be_unloaded)
          ev_cannot_unassign    = DATA(lv_cannot_unassign) ).
      IF lv_not_loaded EQ abap_true.
        MESSAGE e093(zmc_rfui) WITH ls_huhdr-huident INTO lv_msg.
      ENDIF.
      IF lv_wrong_tu EQ abap_true.
        MESSAGE e094(zmc_rfui) WITH ls_huhdr-huident INTO lv_msg.
      ENDIF.
      IF lv_cannot_be_unloaded EQ abap_true.
        MESSAGE e095(zmc_rfui) WITH ls_huhdr-huident INTO lv_msg.
      ENDIF.
      IF lv_cannot_unassign EQ abap_true.
        MESSAGE e095(zmc_rfui) WITH ls_huhdr-huident INTO lv_msg.
      ENDIF.
    ENDLOOP.
  ENDIF.

  lo_loading->save_applog( ).


ENDFUNCTION.
