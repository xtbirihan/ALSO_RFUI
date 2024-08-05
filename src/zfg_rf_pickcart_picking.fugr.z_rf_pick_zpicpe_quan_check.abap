FUNCTION z_rf_pick_zpicpe_quan_check.
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
********************************************************************
*& Key          : <BSUGAREV>-Nov 3, 2023
*& Request No.  :
********************************************************************
*& Description  : Quantity validation
*&
*&
********************************************************************

  NEW lcl_input_fields_validator( )->validate_quan(
    EXPORTING
      is_valid_prf           = is_valid_prf
      iv_flg_verified        = iv_flg_verified
    IMPORTING
      ev_flg_verified        = ev_flg_verified
    CHANGING
      ct_ordim_confirm       = tt_ordim_confirm
      cs_ordim_confirm       = ordim_confirm ).

ENDFUNCTION.
