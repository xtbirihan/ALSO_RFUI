FUNCTION z_rf_admin_print .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_HUHDR) TYPE  /SCWM/S_HUHDR_INT
*"     VALUE(IV_RF_APPLIC) TYPE  /SCWM/DE_APPLIC
*"     VALUE(IV_LTRANS) TYPE  /SCWM/DE_LTRANS
*"     VALUE(IV_STEP) TYPE  /SCWM/DE_STEP
*"     VALUE(IV_FCODE) TYPE  /SCWM/DE_FCODE
*"----------------------------------------------------------------------

  CALL FUNCTION '/SCWM/PRINT_GLOBAL_DATA'
    EXPORTING
      iv_rf_applic = iv_rf_applic
      iv_ltrans    = iv_ltrans
      iv_step      = iv_step
      iv_fcode     = iv_fcode.

  CALL FUNCTION '/SCWM/PRINT_HU'
    EXPORTING
      it_huhdr          = VALUE /scwm/tt_huhdr_int( ( is_huhdr ) )
      iv_caller         = wmegc_hu_processing
      iv_hustep         = zif_wme_c=>gs_hustep-init "'I'
    EXCEPTIONS
      no_previous_print = 1
      error_on_log_save = 2
      previous_print    = 3
      OTHERS            = 4.

*  COMMIT WORK.

ENDFUNCTION.
