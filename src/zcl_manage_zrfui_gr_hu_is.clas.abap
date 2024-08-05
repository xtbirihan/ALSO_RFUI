class ZCL_MANAGE_ZRFUI_GR_HU_IS definition
  public
  final
  create public .

public section.

  class-methods GET_INST
    returning
      value(RO_INST) type ref to ZCL_MANAGE_ZRFUI_GR_HU_IS .
  methods CREATE_HU_IS
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RV_IS_NUMB) type ZDE_IS_NUMBER .
  methods CONSTRUCTOR .
protected section.
private section.

  class-data GO_INST type ref to ZCL_MANAGE_ZRFUI_GR_HU_IS .
  data MO_ZRFUIGRHUIS type ref to ZCL_CRUD_RFUI_GR_HU_IS .

  methods GET_WHN_DATE
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RV_DATE) type ZDE_RF_HU_IS_DATE .
ENDCLASS.



CLASS ZCL_MANAGE_ZRFUI_GR_HU_IS IMPLEMENTATION.


  METHOD constructor.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Class constructor
*&
**********************************************************************

    mo_zrfuigrhuis = zcl_crud_rfui_gr_hu_is=>get_inst( ).

  ENDMETHOD.


  METHOD create_hu_is.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Create sequence number for Inbound Shipment (IS).
*&
**********************************************************************
    DATA:
      lv_locked        TYPE abap_bool,
      lv_whn_date      TYPE zde_rf_hu_is_date,
      lv_is_date_n     TYPE zde_rf_hu_is_date,
      lv_is_seqno_n    TYPE zde_rf_hu_is_seqno,
      lv_is_numb_n 	   TYPE zde_is_number,
      ls_hu_is_new     TYPE zrfi_gr_hu_is,
      ls_rfui_gr_hu_is TYPE zrfi_gr_hu_is.

    " Try to lock table ZRFUI_GR_HU_IS. In case it is already locked try 10 more times
    DO 10 TIMES.
      IF mo_zrfuigrhuis->lock_table( iv_lgnum = iv_lgnum ) = abap_true.
        lv_locked = abap_true.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.

    IF lv_locked <> abap_true.
      " Cannot lock table &
      MESSAGE e001(hrpadit_infotypes) WITH 'ZRFI_GR_HU_IS'.
      RETURN.
    ENDIF.

    TRY.

        ls_rfui_gr_hu_is = mo_zrfuigrhuis->select_single_by_lgnum( iv_lgnum = iv_lgnum ).

        lv_whn_date = get_whn_date( iv_lgnum = iv_lgnum ).

        IF ls_rfui_gr_hu_is IS INITIAL.
          " Assumption: this is the firts entry in the table.
          " Should be executed only once
          lv_is_seqno_n = 1.
          WRITE lv_whn_date TO lv_is_date_n YYMMDD.
          lv_is_numb_n = |{ iv_lgnum }| & |{ lv_is_date_n }| & |{ lv_is_seqno_n }|. "lgnum + YYMMDD + nnnn

          ls_rfui_gr_hu_is-lgnum    = iv_lgnum.
          ls_rfui_gr_hu_is-is_date  = lv_whn_date.
          ls_rfui_gr_hu_is-is_seqno = lv_is_seqno_n.

          mo_zrfuigrhuis->create( is_rf_hu_is = ls_rfui_gr_hu_is ).

        ELSE.

          IF lv_whn_date <> ls_rfui_gr_hu_is-is_date. " Case different date

            lv_is_seqno_n = 1.
            WRITE lv_whn_date TO lv_is_date_n YYMMDD.
            lv_is_numb_n = |{ iv_lgnum }| & |{ lv_is_date_n }| & |{ lv_is_seqno_n }|. "lgnum + YYMMDD + nnnn

            ls_rfui_gr_hu_is-is_date = lv_whn_date.
            ls_rfui_gr_hu_is-is_seqno = lv_is_seqno_n.

            mo_zrfuigrhuis->update( is_rf_hu_is = ls_rfui_gr_hu_is ).
          ELSE.
            " Current date
            lv_is_seqno_n = ls_rfui_gr_hu_is-is_seqno + 1.
            WRITE ls_rfui_gr_hu_is-is_date TO lv_is_date_n YYMMDD.
            lv_is_numb_n = |{ iv_lgnum }| & |{ lv_is_date_n }| & |{ lv_is_seqno_n }|. "lgnum + YYMMDD + nnnn

            ls_rfui_gr_hu_is-is_seqno = lv_is_seqno_n.

            mo_zrfuigrhuis->update( is_rf_hu_is = ls_rfui_gr_hu_is ).
          ENDIF.
        ENDIF.

        mo_zrfuigrhuis->unlock_table( iv_lgnum = iv_lgnum ).

        rv_is_numb = lv_is_numb_n.
      CATCH zcx_db_error INTO DATA(lx_ewm_rf).

        mo_zrfuigrhuis->unlock_table( iv_lgnum = iv_lgnum ).

        MESSAGE ID lx_ewm_rf->if_t100_message~t100key-msgid
              TYPE lx_ewm_rf->if_t100_dyn_msg~msgty
            NUMBER lx_ewm_rf->if_t100_message~t100key-msgno
              WITH lx_ewm_rf->if_t100_dyn_msg~msgv1
                   lx_ewm_rf->if_t100_dyn_msg~msgv2
                   lx_ewm_rf->if_t100_dyn_msg~msgv3
                   lx_ewm_rf->if_t100_dyn_msg~msgv4.

    ENDTRY.

  ENDMETHOD.


  METHOD get_inst.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Get class instance
*&
**********************************************************************

    IF go_inst IS NOT BOUND.
      go_inst = NEW #( ).
    ENDIF.

    ro_inst = go_inst.

  ENDMETHOD.


  METHOD get_whn_date.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Returns the warehause date .
*&
**********************************************************************

    DATA:
      lv_tzone     TYPE tznzone,
      lv_timestamp TYPE timestamp.

    CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
      EXPORTING
        iv_lgnum        = iv_lgnum
      IMPORTING
        ev_tzone        = lv_tzone
      EXCEPTIONS
        interface_error = 1
        data_not_found  = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    GET TIME STAMP FIELD lv_timestamp.

    " Convert timestamp to date using TIMEZONE
    CONVERT TIME STAMP lv_timestamp
             TIME ZONE lv_tzone
             INTO DATE rv_date.

  ENDMETHOD.
ENDCLASS.
