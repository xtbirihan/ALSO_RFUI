FUNCTION z_rf_create_dummy_hu.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_PACK_MAT) TYPE  /SCWM/DE_MATNR
*"     VALUE(IV_BIN) TYPE  /SCWM/LGPLA
*"     VALUE(IT_HU_IDENT) TYPE  /SCWM/TT_HUIDENT_MON
*"     VALUE(IV_CURR_CNT) TYPE  I OPTIONAL
*"     VALUE(IV_MAX_HUS) TYPE  I OPTIONAL
*"     VALUE(IV_PRINT_HU_LAST) TYPE  XFELD
*"     VALUE(IV_QNAME) TYPE  TRFCQNAM OPTIONAL
*"     VALUE(IV_RF_APPLIC) TYPE  /SCWM/DE_APPLIC OPTIONAL
*"     VALUE(IV_LTRANS) TYPE  /SCWM/DE_LTRANS OPTIONAL
*"     VALUE(IV_STEP) TYPE  /SCWM/DE_STEP OPTIONAL
*"     VALUE(IV_FCODE) TYPE  /SCWM/DE_FCODE OPTIONAL
*"  EXPORTING
*"     VALUE(ES_HUHDR) TYPE  /SCWM/S_HUHDR_INT
*"----------------------------------------------------------------------
********************************************************************
*& Key          : RMANOVA-Jan 18, 2024
*& Request No.  : GAP-010 Inbound GR Dummy HU
********************************************************************
*& Description  :
*&
********************************************************************
  DATA:
    lv_pmatid_c22 TYPE /sapapo/matid,
    lv_pmatid_x16 TYPE /scwm/de_matid,
*    ls_huhdr      TYPE /scwm/s_huhdr_int,
    lo_uuid_obj   TYPE REF TO cl_system_uuid,
    lo_pack_obj   TYPE REF TO /scwm/cl_wm_packing.

  /scwm/cl_tm=>cleanup( EXPORTING iv_lgnum = iv_lgnum ).

  /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = lo_pack_obj ).

  lo_pack_obj->init(
    EXPORTING
      iv_lgnum = iv_lgnum
    EXCEPTIONS
      error    = 1
      OTHERS   = 2 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Read MATID from MATNR, APO FORMAT
  CALL FUNCTION '/SAPAPO/DM_MATERIAL_GET_MATID'
    EXPORTING
      iv_matnr = iv_pack_mat
    IMPORTING
      ev_matid = lv_pmatid_c22
    EXCEPTIONS
      OTHERS   = 2.

  "Convert to X16 of EWM
  IF sy-subrc = 0.
    TRY.
        lo_uuid_obj = NEW cl_system_uuid( ).
        CALL METHOD lo_uuid_obj->if_system_uuid~convert_uuid_c22
          EXPORTING
            uuid     = lv_pmatid_c22
          IMPORTING
            uuid_x16 = lv_pmatid_x16.
      CATCH cx_uuid_error .
        CLEAR lv_pmatid_x16.
    ENDTRY.
  ENDIF.

  lo_pack_obj->create_hu(
        EXPORTING
          iv_pmat      = lv_pmatid_x16
          i_location   = iv_bin
        RECEIVING
          es_huhdr     = es_huhdr
        EXCEPTIONS
          error        = 1
          OTHERS       = 2 ).

  IF sy-subrc <> 0.
    "Dummy HU cannot be created
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT it_hu_ident ASSIGNING FIELD-SYMBOL(<ls_hu_ident>).
    CALL METHOD lo_pack_obj->/scwm/if_pack_bas~hu_ident_set
      EXPORTING
        iv_guid_hu = es_huhdr-guid_hu
        iv_huident = <ls_hu_ident>-add_ident
        iv_idart   = <ls_hu_ident>-idart
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.

  lo_pack_obj->save(
    EXPORTING
      iv_commit = abap_true
      iv_wait   = abap_true
    EXCEPTIONS
      error     = 1
      OTHERS    = 2 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


*  " Save the last or first HU to be printed
*  IF ( iv_print_hu_last = abap_true AND iv_curr_cnt = iv_max_hus ) OR
*     ( iv_print_hu_last = abap_false AND iv_curr_cnt = 1 ).
*
*    DATA(ls_hu_print) = ls_huhdr.
*  ENDIF.
*
*  zcl_param=>get_parameter(
*    EXPORTING
*      iv_lgnum     = iv_lgnum
*      iv_process   = zif_param_const=>c_zrfui_0002
*      iv_parameter = zif_param_const=>c_cr_wt_waiting_time
*    IMPORTING
*      ev_constant  = DATA(lv_wait) ).
*
*  " Print Admin Label for first or last HU
*  IF iv_curr_cnt = iv_max_hus.
*    WAIT UP TO CONV i( lv_wait ) SECONDS.
*    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
*      EXPORTING
*        qin_name = iv_qname.
*
*    CALL FUNCTION 'Z_RF_ADMIN_PRINT' IN BACKGROUND TASK
*      EXPORTING
*        is_huhdr     = ls_hu_print
*        iv_rf_applic = iv_rf_applic
*        iv_ltrans    = iv_ltrans
*        iv_step      = iv_step
*        iv_fcode     = iv_fcode.
*  ENDIF.

ENDFUNCTION.
