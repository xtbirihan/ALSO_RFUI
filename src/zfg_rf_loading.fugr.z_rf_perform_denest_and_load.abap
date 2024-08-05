FUNCTION z_rf_perform_denest_and_load.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_IPROC) TYPE  /SCWM/DE_IPROC
*"     VALUE(IV_TU_NUM) TYPE  /SCWM/DE_RF_TU_NUM_LOAD
*"     VALUE(IV_READ_ONLY) TYPE  BOOLE_D
*"     VALUE(IV_SKIP_TU_CHECK) TYPE  BOOLE_D
*"     VALUE(IV_DOOR_BIN) TYPE  /SCWM/LGPLA
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_HUIDENT) TYPE  /SCWM/HUIDENT
*"     VALUE(IV_SYNCRON) TYPE  FLAG OPTIONAL
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-011223
*& Request No.   : GAP-91 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Decision of densesting and then load the HU/HUs to the TU
**********************************************************************

  DATA(lo_loading) = NEW zcl_rf_loading( iv_lgnum ).
  lo_loading->denest_and_load(
    iv_iproc         = iv_iproc
    iv_tu_num        = iv_tu_num
    iv_read_only     = iv_read_only
    iv_skip_tu_check = iv_skip_tu_check
    iv_door_bin      = iv_door_bin
    iv_lgnum         = iv_lgnum
    iv_huident       = iv_huident
  ).

ENDFUNCTION.
