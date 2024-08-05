CLASS zcl_rf_move_gr_dummy_hu DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_rf_move_gr_dummy_hu .
    METHODS move_gr_dummy_hu
      IMPORTING
        iv_warehousenumber  TYPE /scwm/lgnum
      CHANGING
        cv_handling_unit    TYPE /scwm/de_huident
        cv_dest_storage_bin TYPE /scwm/lgpla.
    METHODS error_orccurred
      RETURNING
        VALUE(rv_error_occurred) TYPE abap_bool .
    METHODS get_last_error_message RETURNING VALUE(rs_error) TYPE bapiret2.

    METHODS is_handlung_unit_valid
      IMPORTING
        iv_handling_unit   TYPE /scwm/de_huident
      RETURNING
        VALUE(rv_hu_valid) TYPE abap_bool.
    METHODS is_storage_bin_valid
      IMPORTING
        iv_warehousenumber          TYPE /scwm/lgnum
        iv_dest_storage_bin         TYPE /scwm/lgpla
      RETURNING
        VALUE(rv_storage_bin_valid) TYPE abap_bool .
    METHODS is_warehousenumber_valid
      IMPORTING
        iv_warehousenumber              TYPE /scwm/lgnum
      RETURNING
        VALUE(re_warehousenumber_valid) TYPE abap_bool .
    METHODS is_warehouse_processtype_valid
      IMPORTING
        iv_warehousenumber         TYPE /scwm/lgnum
      RETURNING
        VALUE(rv_is_wh_prct_valid) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_singleton_instance TYPE REF TO zcl_rf_move_gr_dummy_hu.
    CLASS-DATA gv_warehousenumber TYPE /scwm/lgnum.
    CLASS-DATA go_application_log TYPE REF TO /scwm/cl_log.
    DATA mv_warehouse_process_type TYPE /scwm/de_procty.
    DATA mv_dest_storage_bin TYPE /scwm/lgpla.
    DATA mv_handling_unit_ident TYPE /scwm/de_huident.
    DATA mv_severity TYPE bapi_mtype.
    DATA mt_protocol TYPE bapirettab.

    METHODS get_warehouse_process_type
      IMPORTING
        iv_warehousenumber TYPE /scwm/lgnum.
    METHODS is_input_valid
      RETURNING
        VALUE(rv_input_valid) TYPE abap_bool .
    METHODS create_warehouse_task .
ENDCLASS.



CLASS ZCL_RF_MOVE_GR_DUMMY_HU IMPLEMENTATION.


  METHOD create_warehouse_task.
**********************************************************************
*& Key           : AD-230214
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Creates a warehouse task which performs the moving for the HU
*&
******************************************************************
    DATA: lt_protocol TYPE bapirettab.
    DATA: lv_severity TYPE bapi_mtype.

    " Build iTab with HU to move
    DATA(lt_move_hu) = VALUE /scwm/tt_to_crea_hu(
                                (
                                  huident = mv_handling_unit_ident   " Selected HU ident
                                  procty = mv_warehouse_process_type " Processtype from parameter framework
                                  squit = 'X'                        " Immediatley confirm warehouse task
                                  nlpla = mv_dest_storage_bin        " Destination Storage bin
                                )
                               ).

    " Set warehouse number in global settings
    /scwm/cl_tm=>set_lgnum( gv_warehousenumber ).

    " Create warehouse task to move HU
    CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'
      EXPORTING
        iv_lgnum       = gv_warehousenumber
        iv_wtcode      = wmegc_wtcode_adhoc_hu
        iv_update_task = abap_true
        iv_commit_work = abap_true
        it_create_hu   = lt_move_hu
      IMPORTING
        et_bapiret     = lt_protocol
        ev_severity    = lv_severity. " Returns highest severity

    IF lv_severity CA wmegc_severity_eax.
      ROLLBACK WORK.
    ENDIF.

    go_application_log->add_log(  it_prot = lt_protocol  ).
  ENDMETHOD.


  METHOD error_orccurred.
**********************************************************************
*& Key           : AD-230214
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Checks, whether the application log contains a message of severity E, A or X.
*& In that chase, an error occurred during processing.
**********************************************************************
    IF mv_severity CA wmegc_severity_eax.
      rv_error_occurred = abap_true.
    ELSE.
      rv_error_occurred = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
**********************************************************************
*& Key           : AD-230214
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Returns the singleton instance of this class
*&
**********************************************************************
    IF go_singleton_instance IS NOT BOUND.
      " Create singleton instance
      go_singleton_instance = NEW #( ).

      " Create application log instance
      /scwm/cl_log=>get_instance( IMPORTING eo_instance = go_application_log ).
    ENDIF.

    " Return singleton instance
    ro_instance = go_singleton_instance.
  ENDMETHOD.


  METHOD get_last_error_message.
**********************************************************************
*& Key           : AD-230321
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Return the last error Message from the protocol
*&
******************************************************************
    IF line_exists( mt_protocol[ type = 'E' ] ).
      SORT mt_protocol BY type ASCENDING row DESCENDING.
      rs_error = mt_protocol[ type = 'E' ].
    ENDIF.
  ENDMETHOD.


  METHOD get_warehouse_process_type.
**********************************************************************
*& Key           : AD-230214
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Determines the warehouse process type for the desiganted
*& warehouse number.
******************************************************************
    CLEAR: mv_warehouse_process_type.

    zcl_param=>get_parameter(
        EXPORTING
          iv_lgnum = iv_warehousenumber
          iv_process = zif_param_const=>c_zrfui_0001
          iv_parameter = zif_param_const=>c_wh_process_type
       IMPORTING
        ev_constant  =  DATA(lv_constant) ).

    mv_warehouse_process_type = CONV #( lv_constant ).

    IF mv_warehouse_process_type IS INITIAL.
      MESSAGE e142(/scwm/rf_de) INTO DATA(lv_string).
* Enter a warehouse process type
      go_application_log->add_message( ).

      CLEAR: mv_warehouse_process_type.
    ENDIF.
  ENDMETHOD.


  METHOD is_handlung_unit_valid.
**********************************************************************
*& Key           : AD-240116
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Checks, whether the choosen handling unit is valid
*&
******************************************************************
    SELECT SINGLE FROM /scwm/huhdr
    FIELDS guid_hu
     WHERE huident = @iv_handling_unit
      INTO @DATA(lv_guid_hu).

    IF lv_guid_hu IS NOT INITIAL.
      rv_hu_valid = abap_true.
    ELSE.
      rv_hu_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD is_input_valid.
**********************************************************************
*& Key           : AD-230214
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Performs alle input validations
*& If a validation falis, the return value will be false.
******************************************************************
    rv_input_valid = abap_true.

    IF mv_warehouse_process_type IS INITIAL.
      rv_input_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD is_storage_bin_valid.
**********************************************************************
*& Key           : AD-230214
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Checks, whether the choosen destination storage bin is valid
*&
******************************************************************
    SELECT COUNT(*)
      INTO @DATA(lv_counter)
      FROM /scwm/lagp
     WHERE lgnum = @iv_warehousenumber
       AND lgpla = @iv_dest_storage_bin.

    IF lv_counter = 1.
      rv_storage_bin_valid = abap_true.
    ELSE.
      rv_storage_bin_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD is_warehousenumber_valid.
**********************************************************************
*& Key           : AD-230214
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Cheks, whether the choosen warehouse number is valid
*&
******************************************************************
    SELECT SINGLE lgnum
      INTO @DATA(lv_lgnum)
      FROM /scwm/t300
     WHERE lgnum = @iv_warehousenumber.

    IF lv_lgnum IS INITIAL.
      re_warehousenumber_valid = abap_false.
    ELSE.
      re_warehousenumber_valid = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_warehouse_processtype_valid.
**********************************************************************
*& Key           : AD-240118
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Checks, if the processtype in the customzing is maintained.
*&
******************************************************************
    get_warehouse_process_type( iv_warehousenumber  ).
    IF mv_warehouse_process_type IS INITIAL.
      rv_is_wh_prct_valid = abap_false.
    ELSE.
      rv_is_wh_prct_valid = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD move_gr_dummy_hu.
**********************************************************************
*& Key           : AD-230214
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Moves the designated goods received dummy handling unit to
*& another storage bin.
******************************************************************
    DATA: lv_msg TYPE string.
    DATA: ls_log TYPE bal_s_log.

    gv_warehousenumber = iv_warehousenumber.
    mv_handling_unit_ident = cv_handling_unit.
    mv_dest_storage_bin = cv_dest_storage_bin.

    MESSAGE i000(/scwm/it_devkit) WITH 'Start: Move GR dummy HU'(001) INTO lv_msg.
    go_application_log->add_message( ).
    MESSAGE i000(/scwm/it_devkit) WITH |{ 'HU Ident'(003) }: { mv_handling_unit_ident } | INTO lv_msg.
    go_application_log->add_message( ).
    MESSAGE i000(/scwm/it_devkit) WITH |{ 'Dest. Storage Bin'(004) }: { mv_dest_storage_bin } | INTO lv_msg.
    go_application_log->add_message( ).

    create_warehouse_task( ).

    MESSAGE i000(/scwm/it_devkit) WITH 'Finish: Move GR dummy HU'(002) INTO lv_msg.
    go_application_log->add_message( ).

    mt_protocol = go_application_log->get_prot( ).

    " set up parameter for applicaton log.
    ls_log-object    = wmegc_apl_object_wme.
    ls_log-subobject = wmegc_apl_subob_whs_task.
    ls_log-aldate    = go_application_log->mv_created_date.
    ls_log-altime    = go_application_log->mv_created_time.

    mv_severity = go_application_log->get_severity( ).

    " write log in update task with low priority
    CALL FUNCTION '/SCWM/APP_LOG_WRITE_V2' IN UPDATE TASK
      EXPORTING
        is_log     = ls_log
        it_bapiret = mt_protocol.

    COMMIT WORK.

    cv_handling_unit = mv_handling_unit_ident.
    cv_dest_storage_bin = mv_dest_storage_bin.

    " Always clean up at the end.
    /scwm/cl_tm=>cleanup( ).
  ENDMETHOD.
ENDCLASS.
