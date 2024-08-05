FUNCTION z_rf_create_hus.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_PACK_MAT) TYPE  /SCWM/DE_MATNR
*"     VALUE(IV_BIN) TYPE  /SCWM/LGPLA
*"     VALUE(IT_HU_IDENT) TYPE  /SCWM/TT_HUIDENT_MON
*"     VALUE(IV_MAX_HUS) TYPE  I
*"     VALUE(IV_PRINT_HU_LAST) TYPE  XFELD OPTIONAL
*"     VALUE(IV_RF_APPLIC) TYPE  /SCWM/DE_APPLIC
*"     VALUE(IV_LTRANS) TYPE  /SCWM/DE_LTRANS
*"     VALUE(IV_STEP) TYPE  /SCWM/DE_STEP
*"     VALUE(IV_FCODE) TYPE  /SCWM/DE_FCODE
*"----------------------------------------------------------------------
********************************************************************
*& Key          : RMANOVA-Jan 18, 2024
*& Request No.  : GAP-010 Inbound GR Dummy HU
********************************************************************
*& Description  :
*&
********************************************************************

  DATA: ls_huhdr TYPE /scwm/s_huhdr_int.

  DATA(lv_qname) = CONV trfcqnam( |{ 'ZRFPRT' }{ iv_lgnum }{ sy-uname }{ sy-uzeit }| ).

  zcl_param=>get_parameter(
    EXPORTING
      iv_lgnum     = iv_lgnum
      iv_process   = zif_param_const=>c_zrfui_0002
      iv_parameter = zif_param_const=>c_cr_wt_waiting_time
    IMPORTING
      ev_constant  = DATA(lv_wait) ).

  DATA(lv_counter) = 0.

  DO iv_max_hus TIMES.

    lv_counter += 1.
*    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
*      EXPORTING
*        qin_name = lv_qname.

    CALL FUNCTION 'Z_RF_CREATE_DUMMY_HU' "" IN BACKGROUND TASK
      EXPORTING
        iv_lgnum         = iv_lgnum
        iv_pack_mat      = iv_pack_mat
        iv_bin           = iv_bin
        it_hu_ident      = it_hu_ident
*       iv_curr_cnt      = sy-index
*       iv_max_hus       = iv_max_hus
        iv_print_hu_last = iv_print_hu_last
*       iv_qname         = lv_qname
*       iv_rf_applic     = iv_rf_applic
*       iv_ltrans        = iv_ltrans
*       iv_step          = iv_step
*       iv_fcode         = iv_fcode
      IMPORTING
        es_huhdr         = ls_huhdr.

    " Save the last or first HU to be printed
    IF ( iv_print_hu_last = abap_true AND lv_counter = iv_max_hus ) OR
       ( iv_print_hu_last = abap_false AND lv_counter = 1 ).

      DATA(ls_hu_print) = ls_huhdr.
    ENDIF.
*    COMMIT WORK.
*    WAIT UP TO CONV i( lv_wait ) SECONDS.
  ENDDO.

  IF ls_hu_print IS NOT INITIAL.
*    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
*      EXPORTING
*        qin_name = lv_qname.

    CALL FUNCTION 'Z_RF_ADMIN_PRINT'"" IN BACKGROUND TASK
      EXPORTING
        is_huhdr     = ls_hu_print
        iv_rf_applic = iv_rf_applic
        iv_ltrans    = iv_ltrans
        iv_step      = iv_step
        iv_fcode     = iv_fcode.

  ENDIF.
ENDFUNCTION.
