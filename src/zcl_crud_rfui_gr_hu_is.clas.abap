class ZCL_CRUD_RFUI_GR_HU_IS definition
  public
  final
  create public .

public section.

  class-methods GET_INST
    returning
      value(RO_INST) type ref to ZCL_CRUD_RFUI_GR_HU_IS .
  methods SELECT_SINGLE_BY_LGNUM
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RS_RFUI_GR_HU_IS) type ZRFI_GR_HU_IS .
  methods CREATE
    importing
      !IS_RF_HU_IS type ZRFI_GR_HU_IS
    raising
      ZCX_DB_ERROR .
  methods UPDATE
    importing
      !IS_RF_HU_IS type ZRFI_GR_HU_IS
    raising
      ZCX_DB_ERROR .
  methods DELETE
    importing
      !IS_RF_HU_IS type ZRFI_GR_HU_IS
    raising
      ZCX_DB_ERROR .
  methods LOCK_TABLE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_IS_DATE type ZDE_RF_HU_IS_DATE optional
    returning
      value(RV_OK) type ABAP_BOOL .
  methods UNLOCK_TABLE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_IS_DATE type ZDE_RF_HU_IS_DATE optional .
protected section.
private section.

  class-data SO_INST type ref to ZCL_CRUD_RFUI_GR_HU_IS .
  constants C_ENQUEUE_WRITE type ENQMODE value 'E' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_CRUD_RFUI_GR_HU_IS IMPLEMENTATION.


  METHOD create.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Create a new record for sequence number for Inbound Shipment (IS).
*&
**********************************************************************

    INSERT zrfi_gr_hu_is FROM is_rf_hu_is.

    IF sy-subrc <> 0.
      " Error creating entry in table ZRFUI_GR_HU_IS!
      RAISE EXCEPTION TYPE zcx_db_error MESSAGE e003(zmc_rfui).
    ENDIF.

  ENDMETHOD.


  METHOD delete.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Create a new record for sequence number for Inbound Shipment (IS).
*&
**********************************************************************

    DELETE zrfi_gr_hu_is FROM is_rf_hu_is.

    IF sy-subrc <> 0.
      " Error deleting entry in table ZRFUI_GR_HU_IS!
      RAISE EXCEPTION TYPE zcx_db_error MESSAGE e011(zmc_rfui).
    ENDIF.

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

    IF so_inst IS NOT BOUND.
      so_inst = NEW #( ).
    ENDIF.

    ro_inst = so_inst.

  ENDMETHOD.


  METHOD lock_table.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Enqueue table ZRFUI_GR_HU_IS.
*&
**********************************************************************

    " Default value for lock 'E'
    CALL FUNCTION 'ENQUEUE_EZRFUI_GR_HU_IS'
      EXPORTING
        lgnum          = iv_lgnum
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      rv_ok = abap_false.
      RETURN.
    ENDIF.
    rv_ok = abap_true.

  ENDMETHOD.


  METHOD select_single_by_lgnum.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Returns the current date and sequence number for Inbound Shipment (IS).
*&
**********************************************************************

    SELECT SINGLE * FROM zrfi_gr_hu_is
                   WHERE lgnum = @iv_lgnum
                    INTO @rs_rfui_gr_hu_is ##WARN_OK.

  ENDMETHOD.


  METHOD unlock_table.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Enqueue table ZRFUI_GR_HU_IS.
*&
**********************************************************************

    " Default value for lock 'E'
    CALL FUNCTION 'DEQUEUE_EZRFUI_GR_HU_IS'
      EXPORTING
        lgnum = iv_lgnum.


  ENDMETHOD.


  METHOD update.
**********************************************************************
*& Key           : RM-230223
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Create a new record for sequence number for Inbound Shipment (IS).
*&
**********************************************************************

    UPDATE zrfi_gr_hu_is FROM is_rf_hu_is.

    IF sy-subrc <> 0.
      " Error creating entry in table ZRFUI_GR_HU_IS!
      RAISE EXCEPTION TYPE zcx_db_error MESSAGE e003(zmc_rfui).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
